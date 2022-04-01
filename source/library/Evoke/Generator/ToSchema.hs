module Evoke.Generator.ToSchema
  ( generate
  ) where

import qualified Data.List as List
import qualified Evoke.Constant.Module as Module
import qualified Evoke.Generator.Common as Common
import qualified Evoke.Hs as Hs
import qualified Evoke.Hsc as Hsc
import qualified Evoke.Options as Options
import qualified Evoke.Type.Constructor as Constructor
import qualified Evoke.Type.Field as Field
import qualified Evoke.Type.Type as Type
import qualified GHC.Hs as Ghc
import qualified GhcPlugins as Ghc

generate :: Common.Generator
generate moduleName lIdP lHsQTyVars lConDecls options srcSpan = do
  type_ <- Type.make lIdP lHsQTyVars lConDecls srcSpan
  case Type.constructors type_ of
    [_] -> pure ()
    _ -> Hsc.throwError srcSpan $ Ghc.text "requires exactly one constructor"

  modifyFieldName <-
    Common.applyAll
      <$> Options.parse (Common.fieldNameOptions srcSpan) options srcSpan

  fields <-
    mapM (fromField srcSpan modifyFieldName)
    . List.sortOn Field.name
    . concatMap Constructor.fields
    $ Type.constructors type_

  applicative <- Common.makeRandomModule Module.controlApplicative
  lens <- Common.makeRandomModule Module.controlLens
  hashMap <- Common.makeRandomModule Module.dataHashMapStrictInsOrd
  dataMaybe <- Common.makeRandomModule Module.dataMaybe
  monoid <- Common.makeRandomModule Module.dataMonoid
  proxy <- Common.makeRandomModule Module.dataProxy
  string <- Common.makeRandomModule Module.dataString
  swagger <- Common.makeRandomModule Module.dataSwagger
  ignored <- Common.makeRandomVariable srcSpan "_proxy_"
  let
    lImportDecls = Hs.importDecls
      srcSpan
      [ (Module.controlApplicative, applicative)
      , (Module.controlLens, lens)
      , (Module.dataHashMapStrictInsOrd, hashMap)
      , (Module.dataMaybe, dataMaybe)
      , (Module.dataMonoid, monoid)
      , (Module.dataProxy, proxy)
      , (Module.dataString, string)
      , (Module.dataSwagger, swagger)
      ]

    toBind field var =
      Hs.bindStmt srcSpan (Hs.varPat srcSpan var)
        . Hs.app
            srcSpan
            (Hs.qualVar srcSpan swagger $ Ghc.mkVarOcc "declareSchemaRef")
        . Hs.par srcSpan
        . Ghc.L srcSpan
        . Ghc.ExprWithTySig
            Ghc.noExtField
            (Hs.qualVar srcSpan proxy $ Ghc.mkDataOcc "Proxy")
        . Ghc.HsWC Ghc.noExtField
        . Ghc.HsIB Ghc.noExtField
        . Ghc.L srcSpan
        . Ghc.HsAppTy
            Ghc.noExtField
            (Hs.qualTyVar srcSpan proxy $ Ghc.mkClsOcc "Proxy")
        . Ghc.L srcSpan
        . Ghc.HsParTy Ghc.noExtField
        . Ghc.L srcSpan
        $ Field.type_ field -- TODO: This requires `ScopedTypeVariables`.

    bindStmts = fmap (\((field, _), var) -> toBind field var) fields

    setType =
      Hs.opApp
          srcSpan
          (Hs.qualVar srcSpan swagger $ Ghc.mkVarOcc "type_")
          (Hs.qualVar srcSpan lens $ Ghc.mkVarOcc "?~")
        . Hs.qualVar srcSpan swagger
        $ Ghc.mkDataOcc "SwaggerObject"

    setProperties =
      Hs.opApp
          srcSpan
          (Hs.qualVar srcSpan swagger $ Ghc.mkVarOcc "properties")
          (Hs.qualVar srcSpan lens $ Ghc.mkVarOcc ".~")
        . Hs.app srcSpan (Hs.qualVar srcSpan hashMap $ Ghc.mkVarOcc "fromList")
        . Hs.explicitList srcSpan
        $ fmap
            (\((_, name), var) -> Hs.explicitTuple srcSpan $ fmap
              (Hs.tupArg srcSpan)
              [ Hs.app srcSpan (Hs.qualVar srcSpan string $ Ghc.mkVarOcc "fromString")
              . Hs.lit srcSpan
              $ Hs.string name
              , Hs.var srcSpan var
              ]
            )
            fields

    setRequired =
      Hs.opApp
          srcSpan
          (Hs.qualVar srcSpan swagger $ Ghc.mkVarOcc "required")
          (Hs.qualVar srcSpan lens $ Ghc.mkVarOcc ".~")
        . Hs.explicitList srcSpan
        . fmap
            (Hs.app srcSpan (Hs.qualVar srcSpan string $ Ghc.mkVarOcc "fromString")
            . Hs.lit srcSpan
            . Hs.string
            . snd
            . fst
            )
        $ filter (not . Field.isOptional . fst . fst) fields

    lastStmt =
      Hs.lastStmt srcSpan
        . Hs.app srcSpan (Hs.qualVar srcSpan applicative $ Ghc.mkVarOcc "pure")
        . Hs.par srcSpan
        . Hs.app
            srcSpan
            (Hs.app
                srcSpan
                (Hs.qualVar srcSpan swagger $ Ghc.mkDataOcc "NamedSchema")
            . Hs.par srcSpan
            . Hs.app
                srcSpan
                (Hs.qualVar srcSpan dataMaybe $ Ghc.mkDataOcc "Just")
            . Hs.par srcSpan
            . Hs.app srcSpan (Hs.qualVar srcSpan string $ Ghc.mkVarOcc "fromString")
            . Hs.lit srcSpan
            . Hs.string
            $ Type.qualifiedName moduleName type_
            )
        . Hs.par srcSpan
        . makePipeline srcSpan lens [setType, setProperties, setRequired]
        . Hs.qualVar srcSpan monoid
        $ Ghc.mkVarOcc "mempty"

    lHsBind =
      Common.makeLHsBind
          srcSpan
          (Ghc.mkVarOcc "declareNamedSchema")
          [Hs.varPat srcSpan ignored]
        . Hs.doExpr srcSpan
        $ bindStmts
        <> [lastStmt]

    lHsDecl = Common.makeInstanceDeclaration
      srcSpan
      type_
      swagger
      (Ghc.mkClsOcc "ToSchema")
      [lHsBind]

  pure (lImportDecls, [lHsDecl])

fromField
  :: Ghc.SrcSpan
  -> (String -> Ghc.Hsc String)
  -> Field.Field
  -> Ghc.Hsc ((Field.Field, String), Ghc.LIdP Ghc.GhcPs)
fromField srcSpan modifyFieldName field = do
  let fieldName = Field.name field
  name <- modifyFieldName $ Ghc.occNameString fieldName
  var <- Common.makeRandomVariable srcSpan . (<> "_") $ Ghc.occNameString
    fieldName
  pure ((field, name), var)

makePipeline
  :: Ghc.SrcSpan
  -> Ghc.ModuleName
  -> [Ghc.LHsExpr Ghc.GhcPs]
  -> Ghc.LHsExpr Ghc.GhcPs
  -> Ghc.LHsExpr Ghc.GhcPs
makePipeline srcSpan m es e = case es of
  [] -> e
  h : t -> makePipeline srcSpan m t
    $ Hs.opApp srcSpan e (Hs.qualVar srcSpan m $ Ghc.mkVarOcc "&") h
