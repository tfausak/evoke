module Evoke.Generator.FromJSON
  ( generate,
  )
where

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
import qualified GHC.Plugins as Ghc

generate :: Common.Generator
generate moduleName lIdP lHsQTyVars lConDecls options srcSpan = do
  type_ <- Type.make lIdP lHsQTyVars lConDecls srcSpan
  constructor <- case Type.constructors type_ of
    [x] -> pure x
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
  aeson <- Common.makeRandomModule Module.dataAeson
  string <- Common.makeRandomModule Module.dataString
  object <- Common.makeRandomVariable srcSpan "object_"
  let lImportDecls =
        Hs.importDecls
          srcSpan
          [ (Module.controlApplicative, applicative),
            (Module.dataAeson, aeson),
            (Module.dataString, string)
          ]

      bindStmts =
        fmap
          ( \(field, (name, var)) ->
              Hs.bindStmt srcSpan (Hs.varPat srcSpan var)
                . Hs.opApp
                  srcSpan
                  (Hs.var srcSpan object)
                  ( Hs.qualVar srcSpan aeson
                      . Ghc.mkVarOcc
                      $ if Field.isOptional field then ".:?" else ".:"
                  )
                . Hs.app srcSpan (Hs.qualVar srcSpan string $ Ghc.mkVarOcc "fromString")
                . Hs.lit srcSpan
                $ Hs.string name
          )
          fields

      lastStmt =
        Hs.lastStmt srcSpan
          . Hs.app srcSpan (Hs.qualVar srcSpan applicative $ Ghc.mkVarOcc "pure")
          . Hs.recordCon srcSpan (Ghc.L srcSpan $ Constructor.name constructor)
          . Hs.recFields
          $ fmap
            ( \(field, (_, var)) ->
                Hs.recField
                  srcSpan
                  (Hs.fieldOcc srcSpan . Hs.unqual srcSpan $ Field.name field)
                  $ Hs.var srcSpan var
            )
            fields

      lHsBind =
        Common.makeLHsBind srcSpan (Ghc.mkVarOcc "parseJSON") []
          . Hs.app
            srcSpan
            ( Hs.app
                srcSpan
                (Hs.qualVar srcSpan aeson $ Ghc.mkVarOcc "withObject")
                . Hs.lit srcSpan
                . Hs.string
                $ Type.qualifiedName moduleName type_
            )
          . Hs.par srcSpan
          . Hs.lam srcSpan
          . Hs.mg
          $ Ghc.L
            srcSpan
            [ Hs.match srcSpan Ghc.LambdaExpr [Hs.varPat srcSpan object] $
                Hs.grhss
                  srcSpan
                  [ Hs.grhs srcSpan
                      . Hs.doExpr srcSpan
                      $ bindStmts
                        <> [lastStmt]
                  ]
            ]

      lHsDecl =
        Common.makeInstanceDeclaration
          srcSpan
          type_
          aeson
          (Ghc.mkClsOcc "FromJSON")
          [lHsBind]

  pure (lImportDecls, [lHsDecl])

fromField ::
  Ghc.SrcSpan ->
  (String -> Ghc.Hsc String) ->
  Field.Field ->
  Ghc.Hsc (Field.Field, (String, Ghc.LIdP Ghc.GhcPs))
fromField srcSpan modifyFieldName field = do
  let fieldName = Field.name field
  name <- modifyFieldName $ Ghc.occNameString fieldName
  var <-
    Common.makeRandomVariable srcSpan . (<> "_") $
      Ghc.occNameString
        fieldName
  pure (field, (name, var))
