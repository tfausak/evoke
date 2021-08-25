module Evoke
  ( plugin
  ) where

import qualified Control.Monad as Monad
import qualified Control.Monad.IO.Class as IO
import qualified Data.Bifunctor as Bifunctor
import qualified Data.Maybe as Maybe
import qualified Data.Version as Version
import qualified Evoke.Generator.Arbitrary as Arbitrary
import qualified Evoke.Generator.Common as Common
import qualified Evoke.Generator.FromJSON as FromJSON
import qualified Evoke.Generator.ToJSON as ToJSON
import qualified Evoke.Generator.ToSchema as ToSchema
import qualified Evoke.Hsc as Hsc
import qualified Evoke.Options as Options
import qualified Evoke.Type.Config as Config
import qualified Evoke.Type.Flag as Flag
import qualified GHC.Hs as Ghc
import qualified GhcPlugins as Ghc
import qualified Paths_evoke as This
import qualified System.Console.GetOpt as Console

-- | The compiler plugin. You can enable this plugin with the following pragma:
--
-- > {-# OPTIONS_GHC -fplugin=Evoke #-}
--
-- This plugin accepts some options. Pass @-fplugin-opt=Evoke:--help@ to see
-- what they are. For example:
--
-- > {-# OPTIONS_GHC -fplugin=Evoke -fplugin-opt=Evoke:--help #-}
--
-- Once this plugin is enabled, you can use it by deriving instances like this:
--
-- > data Person = Person
-- >   { name :: String
-- >   , age :: Int
-- >   } deriving ToJSON via "Evoke"
--
-- The GHC user's guide has more detail about compiler plugins in general:
-- <https://downloads.haskell.org/~ghc/8.10.4/docs/html/users_guide/extending_ghc.html#compiler-plugins>.
plugin :: Ghc.Plugin
plugin = Ghc.defaultPlugin
  { Ghc.parsedResultAction = parsedResultAction
  , Ghc.pluginRecompile = Ghc.purePlugin
  }

-- | This is the main entry point for the plugin. It receives the command line
-- options, module summary, and parsed module from GHC. Ultimately it produces
-- a new parsed module to replace the old one.
--
-- From a high level, this function parses the command line options to build a
-- config, then hands things off to the next function ('handleLHsModule').
parsedResultAction
  :: [Ghc.CommandLineOption]
  -> Ghc.ModSummary
  -> Ghc.HsParsedModule
  -> Ghc.Hsc Ghc.HsParsedModule
parsedResultAction commandLineOptions modSummary hsParsedModule = do
  let
    lHsModule1 = Ghc.hpm_module hsParsedModule
    srcSpan = Ghc.getLoc lHsModule1
  flags <- Options.parse Flag.options commandLineOptions srcSpan
  let config = Config.fromFlags flags
  Monad.when (Config.help config)
    . Hsc.throwError srcSpan
    . Ghc.vcat
    . fmap Ghc.text
    . lines
    $ Console.usageInfo ("Evoke version " <> version) Flag.options
  Monad.when (Config.version config) . Hsc.throwError srcSpan $ Ghc.text
    version

  let moduleName = Ghc.moduleName $ Ghc.ms_mod modSummary
  lHsModule2 <- handleLHsModule config moduleName lHsModule1
  pure hsParsedModule { Ghc.hpm_module = lHsModule2 }

-- | This package's version number as a string.
version :: String
version = Version.showVersion This.version

-- | This is the start of the plumbing functions. Our goal is to take the
-- parsed module, find any relevant deriving clauses, and replace them with
-- generated instances. This means we need to walk the tree of the parsed
-- module looking for relevant deriving clauses. When we find them, we're going
-- to remove them and emit new declarations (and imports), which need to be
-- inserted back into the parsed module tree.
--
-- All of these functions are plumbing. If you want to skip to the interesting
-- part, go to 'handleLHsSigType'.
handleLHsModule
  :: Config.Config
  -> Ghc.ModuleName
  -> LHsModule Ghc.GhcPs
  -> Ghc.Hsc (LHsModule Ghc.GhcPs)
handleLHsModule config moduleName lHsModule = do
  hsModule <- handleHsModule config moduleName $ Ghc.unLoc lHsModule
  pure $ Ghc.mapLoc (const hsModule) lHsModule

-- | Most GHC types have type aliases for their located versions. For some
-- reason the module type doesn't.
type LHsModule pass = Ghc.Located (Ghc.HsModule pass)

-- | See 'handleLHsModule' and 'handleLHsSigType'.
handleHsModule
  :: Config.Config
  -> Ghc.ModuleName
  -> Ghc.HsModule Ghc.GhcPs
  -> Ghc.Hsc (Ghc.HsModule Ghc.GhcPs)
handleHsModule config moduleName hsModule = do
  (lImportDecls, lHsDecls) <- handleLHsDecls config moduleName
    $ Ghc.hsmodDecls hsModule
  pure hsModule
    { Ghc.hsmodImports = Ghc.hsmodImports hsModule <> lImportDecls
    , Ghc.hsmodDecls = lHsDecls
    }

-- | See 'handleLHsModule' and 'handleLHsSigType'.
handleLHsDecls
  :: Config.Config
  -> Ghc.ModuleName
  -> [Ghc.LHsDecl Ghc.GhcPs]
  -> Ghc.Hsc ([Ghc.LImportDecl Ghc.GhcPs], [Ghc.LHsDecl Ghc.GhcPs])
handleLHsDecls config moduleName lHsDecls = do
  tuples <- mapM (handleLHsDecl config moduleName) lHsDecls
  pure . Bifunctor.bimap mconcat mconcat $ unzip tuples

-- | See 'handleLHsModule' and 'handleLHsSigType'.
handleLHsDecl
  :: Config.Config
  -> Ghc.ModuleName
  -> Ghc.LHsDecl Ghc.GhcPs
  -> Ghc.Hsc ([Ghc.LImportDecl Ghc.GhcPs], [Ghc.LHsDecl Ghc.GhcPs])
handleLHsDecl config moduleName lHsDecl = case Ghc.unLoc lHsDecl of
  Ghc.TyClD xTyClD tyClDecl1 -> do
    (tyClDecl2, (lImportDecls, lHsDecls)) <- handleTyClDecl
      config
      moduleName
      tyClDecl1
    pure
      ( lImportDecls
      , Ghc.mapLoc (const $ Ghc.TyClD xTyClD tyClDecl2) lHsDecl : lHsDecls
      )
  _ -> pure ([], [lHsDecl])

-- | See 'handleLHsModule' and 'handleLHsSigType'.
handleTyClDecl
  :: Config.Config
  -> Ghc.ModuleName
  -> Ghc.TyClDecl Ghc.GhcPs
  -> Ghc.Hsc
       ( Ghc.TyClDecl Ghc.GhcPs
       , ([Ghc.LImportDecl Ghc.GhcPs], [Ghc.LHsDecl Ghc.GhcPs])
       )
handleTyClDecl config moduleName tyClDecl = case tyClDecl of
  Ghc.DataDecl tcdDExt tcdLName tcdTyVars tcdFixity tcdDataDefn -> do
    (hsDataDefn, (lImportDecls, lHsDecls)) <- handleHsDataDefn
      config
      moduleName
      tcdLName
      tcdTyVars
      tcdDataDefn
    pure
      ( Ghc.DataDecl tcdDExt tcdLName tcdTyVars tcdFixity hsDataDefn
      , (lImportDecls, lHsDecls)
      )
  _ -> pure (tyClDecl, ([], []))

-- | See 'handleLHsModule' and 'handleLHsSigType'.
handleHsDataDefn
  :: Config.Config
  -> Ghc.ModuleName
  -> Ghc.LIdP Ghc.GhcPs
  -> Ghc.LHsQTyVars Ghc.GhcPs
  -> Ghc.HsDataDefn Ghc.GhcPs
  -> Ghc.Hsc
       ( Ghc.HsDataDefn Ghc.GhcPs
       , ([Ghc.LImportDecl Ghc.GhcPs], [Ghc.LHsDecl Ghc.GhcPs])
       )
handleHsDataDefn config moduleName lIdP lHsQTyVars hsDataDefn =
  case hsDataDefn of
    Ghc.HsDataDefn dd_ext dd_ND dd_ctxt dd_cType dd_kindSig dd_cons dd_derivs
      -> do
        (hsDeriving, (lImportDecls, lHsDecls)) <- handleHsDeriving
          config
          moduleName
          lIdP
          lHsQTyVars
          dd_cons
          dd_derivs
        pure
          ( Ghc.HsDataDefn
            dd_ext
            dd_ND
            dd_ctxt
            dd_cType
            dd_kindSig
            dd_cons
            hsDeriving
          , (lImportDecls, lHsDecls)
          )
    _ -> pure (hsDataDefn, ([], []))

-- | See 'handleLHsModule' and 'handleLHsSigType'.
handleHsDeriving
  :: Config.Config
  -> Ghc.ModuleName
  -> Ghc.LIdP Ghc.GhcPs
  -> Ghc.LHsQTyVars Ghc.GhcPs
  -> [Ghc.LConDecl Ghc.GhcPs]
  -> Ghc.HsDeriving Ghc.GhcPs
  -> Ghc.Hsc
       ( Ghc.HsDeriving Ghc.GhcPs
       , ( [Ghc.LImportDecl Ghc.GhcPs]
         , [Ghc.LHsDecl Ghc.GhcPs]
         )
       )
handleHsDeriving config moduleName lIdP lHsQTyVars lConDecls hsDeriving = do
  (lHsDerivingClauses, (lImportDecls, lHsDecls)) <-
    handleLHsDerivingClauses config moduleName lIdP lHsQTyVars lConDecls
      $ Ghc.unLoc hsDeriving
  pure
    ( Ghc.mapLoc (const lHsDerivingClauses) hsDeriving
    , (lImportDecls, lHsDecls)
    )

-- | See 'handleLHsModule' and 'handleLHsSigType'.
handleLHsDerivingClauses
  :: Config.Config
  -> Ghc.ModuleName
  -> Ghc.LIdP Ghc.GhcPs
  -> Ghc.LHsQTyVars Ghc.GhcPs
  -> [Ghc.LConDecl Ghc.GhcPs]
  -> [Ghc.LHsDerivingClause Ghc.GhcPs]
  -> Ghc.Hsc
       ( [Ghc.LHsDerivingClause Ghc.GhcPs]
       , ( [Ghc.LImportDecl Ghc.GhcPs]
         , [Ghc.LHsDecl Ghc.GhcPs]
         )
       )
handleLHsDerivingClauses config moduleName lIdP lHsQTyVars lConDecls lHsDerivingClauses
  = do
    tuples <- mapM
      (handleLHsDerivingClause config moduleName lIdP lHsQTyVars lConDecls)
      lHsDerivingClauses
    pure
      . Bifunctor.bimap
          Maybe.catMaybes
          (Bifunctor.bimap mconcat mconcat . unzip)
      $ unzip tuples

-- | See 'handleLHsModule' and 'handleLHsSigType'.
handleLHsDerivingClause
  :: Config.Config
  -> Ghc.ModuleName
  -> Ghc.LIdP Ghc.GhcPs
  -> Ghc.LHsQTyVars Ghc.GhcPs
  -> [Ghc.LConDecl Ghc.GhcPs]
  -> Ghc.LHsDerivingClause Ghc.GhcPs
  -> Ghc.Hsc
       ( Maybe (Ghc.LHsDerivingClause Ghc.GhcPs)
       , ( [Ghc.LImportDecl Ghc.GhcPs]
         , [Ghc.LHsDecl Ghc.GhcPs]
         )
       )
handleLHsDerivingClause config moduleName lIdP lHsQTyVars lConDecls lHsDerivingClause
  = case Ghc.unLoc lHsDerivingClause of
    Ghc.HsDerivingClause _ deriv_clause_strategy deriv_clause_tys
      | Just options <- parseDerivingStrategy deriv_clause_strategy -> do
        (lImportDecls, lHsDecls) <-
          handleLHsSigTypes config moduleName lIdP lHsQTyVars lConDecls options
            $ Ghc.unLoc deriv_clause_tys
        pure (Nothing, (lImportDecls, lHsDecls))
    _ -> pure (Just lHsDerivingClause, ([], []))

-- | This plugin only fires on specific deriving strategies. In particular it
-- looks for clauses like this:
--
-- > deriving C via "Evoke ..."
--
-- This function is responsible for analyzing a deriving strategy to determine
-- if the plugin should fire or not.
parseDerivingStrategy
  :: Maybe (Ghc.LDerivStrategy Ghc.GhcPs) -> Maybe [String]
parseDerivingStrategy mLDerivStrategy = do
  lDerivStrategy <- mLDerivStrategy
  lHsSigType <- case Ghc.unLoc lDerivStrategy of
    Ghc.ViaStrategy x -> Just x
    _ -> Nothing
  lHsType <- case lHsSigType of
    Ghc.HsIB _ x -> Just x
    _ -> Nothing
  hsTyLit <- case Ghc.unLoc lHsType of
    Ghc.HsTyLit _ x -> Just x
    _ -> Nothing
  fastString <- case hsTyLit of
    Ghc.HsStrTy _ x -> Just x
    _ -> Nothing
  case words $ Ghc.unpackFS fastString of
    "Evoke" : x -> Just x
    _ -> Nothing

-- | See 'handleLHsModule' and 'handleLHsSigType'.
handleLHsSigTypes
  :: Config.Config
  -> Ghc.ModuleName
  -> Ghc.LIdP Ghc.GhcPs
  -> Ghc.LHsQTyVars Ghc.GhcPs
  -> [Ghc.LConDecl Ghc.GhcPs]
  -> [String]
  -> [Ghc.LHsSigType Ghc.GhcPs]
  -> Ghc.Hsc
       ( [Ghc.LImportDecl Ghc.GhcPs]
       , [Ghc.LHsDecl Ghc.GhcPs]
       )
handleLHsSigTypes config moduleName lIdP lHsQTyVars lConDecls options lHsSigTypes
  = do
    tuples <- mapM
      (handleLHsSigType config moduleName lIdP lHsQTyVars lConDecls options)
      lHsSigTypes
    pure . Bifunctor.bimap mconcat mconcat $ unzip tuples

-- | This is the main workhorse of the plugin. By the time things get here,
-- everything has already been plumbed correctly. (See 'handleLHsModule' for
-- details.) This function is responsible for actually generating the instance.
-- If we don't know how to generate an instance for the requested class, an
-- error will be thrown. If the user requested verbose output, the generated
-- instance will be printed.
handleLHsSigType
  :: Config.Config
  -> Ghc.ModuleName
  -> Ghc.LIdP Ghc.GhcPs
  -> Ghc.LHsQTyVars Ghc.GhcPs
  -> [Ghc.LConDecl Ghc.GhcPs]
  -> [String]
  -> Ghc.LHsSigType Ghc.GhcPs
  -> Ghc.Hsc
       ( [Ghc.LImportDecl Ghc.GhcPs]
       , [Ghc.LHsDecl Ghc.GhcPs]
       )
handleLHsSigType config moduleName lIdP lHsQTyVars lConDecls options lHsSigType
  = do
    let
      srcSpan = case lHsSigType of
        Ghc.HsIB _ x -> Ghc.getLoc x
        _ -> Ghc.getLoc lIdP
    (lImportDecls, lHsDecls) <- case getGenerator lHsSigType of
      Just generate ->
        generate moduleName lIdP lHsQTyVars lConDecls options srcSpan
      Nothing -> Hsc.throwError srcSpan $ Ghc.text "unsupported type class"

    Monad.when (Config.verbose config) $ do
      dynFlags <- Ghc.getDynFlags
      IO.liftIO $ do
        putStrLn $ replicate 80 '-'
        mapM_ (putStrLn . Ghc.showSDocDump dynFlags . Ghc.ppr) lImportDecls
        mapM_ (putStrLn . Ghc.showSDocDump dynFlags . Ghc.ppr) lHsDecls

    pure (lImportDecls, lHsDecls)

getGenerator :: Ghc.LHsSigType Ghc.GhcPs -> Maybe Common.Generator
getGenerator lHsSigType = do
  className <- getClassName lHsSigType
  lookup className generators

generators :: [(String, Common.Generator)]
generators =
  [ ("Arbitrary", Arbitrary.generate)
  , ("FromJSON", FromJSON.generate)
  , ("ToJSON", ToJSON.generate)
  , ("ToSchema", ToSchema.generate)
  ]

-- | Extracts the class name out of a type signature.
getClassName :: Ghc.LHsSigType Ghc.GhcPs -> Maybe String
getClassName lHsSigType = do
  lHsType <- case lHsSigType of
    Ghc.HsIB _ x -> Just x
    _ -> Nothing
  lIdP <- case Ghc.unLoc lHsType of
    Ghc.HsTyVar _ _ x -> Just x
    _ -> Nothing
  case Ghc.unLoc lIdP of
    Ghc.Unqual x -> Just $ Ghc.occNameString x
    _ -> Nothing
