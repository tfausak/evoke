module Evoke.Generator.Common
  ( Generator
  , applyAll
  , fieldNameOptions
  , makeInstanceDeclaration
  , makeLHsBind
  , makeRandomModule
  , makeRandomVariable
  ) where

import qualified Bag as Ghc
import qualified Control.Monad.IO.Class as IO
import qualified Data.Char as Char
import qualified Data.IORef as IORef
import qualified Data.List as List
import qualified Data.Maybe as Maybe
import qualified Data.Text as Text
import qualified Evoke.Hs as Hs
import qualified Evoke.Hsc as Hsc
import qualified Evoke.Type.Constructor as Constructor
import qualified Evoke.Type.Field as Field
import qualified Evoke.Type.Type as Type
import qualified GHC.Hs as Ghc
import qualified GhcPlugins as Ghc
import qualified System.Console.GetOpt as Console
import qualified System.IO.Unsafe as Unsafe
import qualified Text.Printf as Printf

type Generator
  = Ghc.ModuleName
  -> Ghc.LIdP Ghc.GhcPs
  -> Ghc.LHsQTyVars Ghc.GhcPs
  -> [Ghc.LConDecl Ghc.GhcPs]
  -> [String]
  -> Ghc.SrcSpan
  -> Ghc.Hsc
       ([Ghc.LImportDecl Ghc.GhcPs], [Ghc.LHsDecl Ghc.GhcPs])

fieldNameOptions
  :: Ghc.SrcSpan -> [Console.OptDescr (String -> Ghc.Hsc String)]
fieldNameOptions srcSpan =
  [ Console.Option [] ["kebab"] (Console.NoArg $ pure . kebab) ""
  , Console.Option [] ["camel"] (Console.NoArg $ pure . lower) ""
  , Console.Option [] ["snake"] (Console.NoArg $ pure . snake) ""
  , Console.Option
    []
    ["prefix", "strip"]
    (Console.ReqArg
      (stripPrefix srcSpan)
      "PREFIX"
    )
    ""
  , Console.Option
    []
    ["suffix"]
    (Console.ReqArg
      (stripSuffix srcSpan)
      "SUFFIX"
    )
    ""
  , Console.Option [] ["title"] (Console.NoArg $ pure . upper) ""
  ]

stripPrefix :: Ghc.SrcSpan -> String -> String -> Ghc.Hsc String
stripPrefix srcSpan prefix s1 = case List.stripPrefix prefix s1 of
  Nothing ->
    Hsc.throwError srcSpan
      . Ghc.text
      $ show prefix
      <> " is not a prefix of "
      <> show s1
  Just s2 -> pure s2

stripSuffix :: Ghc.SrcSpan -> String -> String -> Ghc.Hsc String
stripSuffix srcSpan suffix s1 = case Text.stripSuffix (Text.pack suffix) (Text.pack s1) of
  Nothing ->
    Hsc.throwError srcSpan
      . Ghc.text
      $ show suffix
      <> " is not a suffix of "
      <> show s1
  Just s2 -> pure $ Text.unpack s2

-- | Applies all the monadic functions in order beginning with some starting
-- value.
applyAll :: Monad m => [a -> m a] -> a -> m a
applyAll fs x = case fs of
  [] -> pure x
  f : gs -> do
    y <- f x
    applyAll gs y

-- | Converts the first character into upper case.
upper :: String -> String
upper = overFirst Char.toUpper

-- | Converts the first character into lower case.
lower :: String -> String
lower = overFirst Char.toLower

overFirst :: (a -> a) -> [a] -> [a]
overFirst f xs = case xs of
  x : ys -> f x : ys
  _ -> xs

-- | Converts the string into kebab case.
--
-- >>> kebab "DoReMi"
-- "do-re-mi"
kebab :: String -> String
kebab = camelTo '-'

-- | Converts the string into snake case.
--
-- >>> snake "DoReMi"
-- "do_re_mi"
snake :: String -> String
snake = camelTo '_'

camelTo :: Char -> String -> String
camelTo char =
  let
    go wasUpper string = case string of
      "" -> ""
      first : rest -> if Char.isUpper first
        then if wasUpper
          then Char.toLower first : go True rest
          else char : Char.toLower first : go True rest
        else first : go False rest
  in go True

makeLHsType
  :: Ghc.SrcSpan
  -> Ghc.ModuleName
  -> Ghc.OccName
  -> Type.Type
  -> Ghc.LHsType Ghc.GhcPs
makeLHsType srcSpan moduleName className =
  Ghc.L srcSpan
    . Ghc.HsAppTy
        Ghc.noExtField
        (Ghc.L srcSpan
        . Ghc.HsTyVar Ghc.noExtField Ghc.NotPromoted
        . Ghc.L srcSpan
        $ Ghc.Qual moduleName className
        )
    . toLHsType srcSpan

toLHsType :: Ghc.SrcSpan -> Type.Type -> Ghc.LHsType Ghc.GhcPs
toLHsType srcSpan type_ =
  let
    ext :: Ghc.NoExtField
    ext = Ghc.noExtField

    loc :: a -> Ghc.Located a
    loc = Ghc.L srcSpan

    initial :: Ghc.LHsType Ghc.GhcPs
    initial = loc . Ghc.HsTyVar ext Ghc.NotPromoted . loc $ Type.name type_

    combine
      :: Ghc.LHsType Ghc.GhcPs -> Ghc.IdP Ghc.GhcPs -> Ghc.LHsType Ghc.GhcPs
    combine x =
      loc . Ghc.HsAppTy ext x . loc . Ghc.HsTyVar ext Ghc.NotPromoted . loc

    bare :: Ghc.LHsType Ghc.GhcPs
    bare = List.foldl' combine initial $ Type.variables type_
  in case Type.variables type_ of
    [] -> bare
    _ -> loc $ Ghc.HsParTy ext bare

makeHsContext
  :: Ghc.SrcSpan
  -> Ghc.ModuleName
  -> Ghc.OccName
  -> Type.Type
  -> [Ghc.LHsType Ghc.GhcPs]
makeHsContext srcSpan moduleName className =
  fmap
      (Ghc.L srcSpan
      . Ghc.HsAppTy
          Ghc.noExtField
          (Ghc.L srcSpan
          . Ghc.HsTyVar Ghc.noExtField Ghc.NotPromoted
          . Ghc.L srcSpan
          $ Ghc.Qual moduleName className
          )
      . Ghc.L srcSpan
      . Ghc.HsTyVar Ghc.noExtField Ghc.NotPromoted
      . Ghc.L srcSpan
      . Ghc.Unqual
      )
    . List.nub
    . Maybe.mapMaybe
        (\field -> case Field.type_ field of
          Ghc.HsTyVar _ _ lRdrName -> case Ghc.unLoc lRdrName of
            Ghc.Unqual occName | Ghc.isTvOcc occName -> Just occName
            _ -> Nothing
          _ -> Nothing
        )
    . concatMap Constructor.fields
    . Type.constructors

makeHsImplicitBndrs
  :: Ghc.SrcSpan
  -> Type.Type
  -> Ghc.ModuleName
  -> Ghc.OccName
  -> Ghc.HsImplicitBndrs Ghc.GhcPs (Ghc.LHsType Ghc.GhcPs)
makeHsImplicitBndrs srcSpan type_ moduleName className =
  let
    withoutContext = makeLHsType srcSpan moduleName className type_
    context = makeHsContext srcSpan moduleName className type_
    withContext = if null context
      then withoutContext
      else Ghc.L srcSpan
        $ Ghc.HsQualTy Ghc.noExtField (Ghc.L srcSpan context) withoutContext
  in Ghc.HsIB Ghc.noExtField withContext

-- | Makes a random variable name using the given prefix.
makeRandomVariable :: Ghc.SrcSpan -> String -> Ghc.Hsc (Ghc.LIdP Ghc.GhcPs)
makeRandomVariable srcSpan prefix = do
  n <- bumpCounter
  pure . Ghc.L srcSpan . Ghc.Unqual . Ghc.mkVarOcc $ Printf.printf
    "%s%d"
    prefix
    n

-- | Makes a random module name. This will convert any periods to underscores
-- and add a unique suffix.
--
-- >>> makeRandomModule "Data.Aeson"
-- "Data_Aeson_1"
makeRandomModule :: Ghc.ModuleName -> Ghc.Hsc Ghc.ModuleName
makeRandomModule moduleName = do
  n <- bumpCounter
  pure . Ghc.mkModuleName $ Printf.printf
    "%s_%d"
    (underscoreAll moduleName)
    n

underscoreAll :: Ghc.ModuleName -> String
underscoreAll = fmap underscoreOne . Ghc.moduleNameString

underscoreOne :: Char -> Char
underscoreOne c = case c of
  '.' -> '_'
  _ -> c

makeInstanceDeclaration
  :: Ghc.SrcSpan
  -> Type.Type
  -> Ghc.ModuleName
  -> Ghc.OccName
  -> [Ghc.LHsBind Ghc.GhcPs]
  -> Ghc.LHsDecl Ghc.GhcPs
makeInstanceDeclaration srcSpan type_ moduleName occName lHsBinds =
  let hsImplicitBndrs = makeHsImplicitBndrs srcSpan type_ moduleName occName
  in makeLHsDecl srcSpan hsImplicitBndrs lHsBinds

makeLHsDecl
  :: Ghc.SrcSpan
  -> Ghc.HsImplicitBndrs Ghc.GhcPs (Ghc.LHsType Ghc.GhcPs)
  -> [Ghc.LHsBind Ghc.GhcPs]
  -> Ghc.LHsDecl Ghc.GhcPs
makeLHsDecl srcSpan hsImplicitBndrs lHsBinds =
  Ghc.L srcSpan
    . Ghc.InstD Ghc.noExtField
    . Ghc.ClsInstD Ghc.noExtField
    $ Ghc.ClsInstDecl
        Ghc.noExtField
        hsImplicitBndrs
        (Ghc.listToBag lHsBinds)
        []
        []
        []
        Nothing

makeLHsBind
  :: Ghc.SrcSpan
  -> Ghc.OccName
  -> [Ghc.LPat Ghc.GhcPs]
  -> Ghc.LHsExpr Ghc.GhcPs
  -> Ghc.LHsBind Ghc.GhcPs
makeLHsBind srcSpan occName pats =
  Hs.funBind srcSpan occName . makeMatchGroup srcSpan occName pats

makeMatchGroup
  :: Ghc.SrcSpan
  -> Ghc.OccName
  -> [Ghc.LPat Ghc.GhcPs]
  -> Ghc.LHsExpr Ghc.GhcPs
  -> Ghc.MatchGroup Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
makeMatchGroup srcSpan occName lPats hsExpr = Ghc.MG
  Ghc.noExtField
  (Ghc.L srcSpan [Ghc.L srcSpan $ makeMatch srcSpan occName lPats hsExpr])
  Ghc.Generated

makeMatch
  :: Ghc.SrcSpan
  -> Ghc.OccName
  -> [Ghc.LPat Ghc.GhcPs]
  -> Ghc.LHsExpr Ghc.GhcPs
  -> Ghc.Match Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
makeMatch srcSpan occName lPats =
  Ghc.Match
      Ghc.noExtField
      (Ghc.FunRhs
        (Ghc.L srcSpan $ Ghc.Unqual occName)
        Ghc.Prefix
        Ghc.NoSrcStrict
      )
      lPats
    . makeGRHSs srcSpan

makeGRHSs
  :: Ghc.SrcSpan
  -> Ghc.LHsExpr Ghc.GhcPs
  -> Ghc.GRHSs Ghc.GhcPs (Ghc.LHsExpr Ghc.GhcPs)
makeGRHSs srcSpan hsExpr =
  Ghc.GRHSs Ghc.noExtField [Hs.grhs srcSpan hsExpr]
    . Ghc.L srcSpan
    $ Ghc.EmptyLocalBinds Ghc.noExtField

bumpCounter :: IO.MonadIO m => m Word
bumpCounter = IO.liftIO . IORef.atomicModifyIORef' counterRef $ \ n -> (n + 1, n)

counterRef :: IORef.IORef Word
counterRef = Unsafe.unsafePerformIO $ IORef.newIORef 0
{-# NOINLINE counterRef #-}
