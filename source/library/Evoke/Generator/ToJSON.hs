module Evoke.Generator.ToJSON
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
import qualified GHC.Plugins as Ghc

generate :: Common.Generator
generate _ lIdP lHsQTyVars lConDecls options srcSpan = do
  type_ <- Type.make lIdP lHsQTyVars lConDecls srcSpan
  case Type.constructors type_ of
    [_] -> pure ()
    _ -> Hsc.throwError srcSpan $ Ghc.text "requires exactly one constructor"

  modifyFieldName <-
    Common.applyAll
      <$> Options.parse (Common.fieldNameOptions srcSpan) options srcSpan

  fieldNames <-
    mapM (fromField modifyFieldName)
    . List.sort
    . fmap Field.name
    . concatMap Constructor.fields
    $ Type.constructors type_

  aeson <- Common.makeRandomModule Module.dataAeson
  monoid <- Common.makeRandomModule Module.dataMonoid
  string <- Common.makeRandomModule Module.dataString
  var1 <- Common.makeRandomVariable srcSpan "var_"
  var2 <- Common.makeRandomVariable srcSpan "var_"
  let
    lImportDecls = Hs.importDecls
      srcSpan
      [ (Module.dataAeson, aeson)
      , (Module.dataMonoid, monoid)
      , (Module.dataString, string)
      ]

    toPair lRdrName (occName, fieldName) =
      Hs.opApp
          srcSpan
          (Hs.app srcSpan (Hs.qualVar srcSpan string $ Ghc.mkVarOcc "fromString")
          . Hs.lit srcSpan
          $ Hs.string fieldName
          )
          (Hs.qualVar srcSpan aeson $ Ghc.mkVarOcc ".=")
        . Hs.app srcSpan (Hs.var srcSpan $ Hs.unqual srcSpan occName)
        $ Hs.var srcSpan lRdrName

    lHsExprs lRdrName = fmap (toPair lRdrName) fieldNames

    toJSON =
      Common.makeLHsBind
          srcSpan
          (Ghc.mkVarOcc "toJSON")
          [Hs.varPat srcSpan var1]
        . Hs.app srcSpan (Hs.qualVar srcSpan aeson $ Ghc.mkVarOcc "object")
        . Hs.explicitList srcSpan
        $ lHsExprs var1

    toEncoding =
      Common.makeLHsBind
          srcSpan
          (Ghc.mkVarOcc "toEncoding")
          [Hs.varPat srcSpan var2]
        . Hs.app srcSpan (Hs.qualVar srcSpan aeson $ Ghc.mkVarOcc "pairs")
        . Hs.par srcSpan
        . Hs.app srcSpan (Hs.qualVar srcSpan monoid $ Ghc.mkVarOcc "mconcat")
        . Hs.explicitList srcSpan
        $ lHsExprs var2

    lHsDecl = Common.makeInstanceDeclaration
      srcSpan
      type_
      aeson
      (Ghc.mkClsOcc "ToJSON")
      [toJSON, toEncoding]

  pure (lImportDecls, [lHsDecl])

fromField
  :: (String -> Ghc.Hsc String) -> Ghc.OccName -> Ghc.Hsc (Ghc.OccName, String)
fromField modifyFieldName occName = do
  fieldName <- modifyFieldName $ Ghc.occNameString occName
  pure (occName, fieldName)
