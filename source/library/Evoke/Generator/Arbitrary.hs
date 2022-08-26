module Evoke.Generator.Arbitrary
  ( generate,
  )
where

import qualified Data.List as List
import qualified Evoke.Constant.Module as Module
import qualified Evoke.Generator.Common as Common
import qualified Evoke.Hs as Hs
import qualified Evoke.Hsc as Hsc
import qualified Evoke.Type.Constructor as Constructor
import qualified Evoke.Type.Field as Field
import qualified Evoke.Type.Type as Type
import qualified GHC.Hs as Ghc
import qualified GHC.Plugins as Ghc

generate :: Common.Generator
generate _ lIdP lHsQTyVars lConDecls _ srcSpan = do
  type_ <- Type.make lIdP lHsQTyVars lConDecls srcSpan
  constructor <- case Type.constructors type_ of
    [x] -> pure x
    _ -> Hsc.throwError srcSpan $ Ghc.text "requires exactly one constructor"
  fields <-
    mapM (fromField srcSpan)
      . List.sortOn Field.name
      . concatMap Constructor.fields
      $ Type.constructors type_

  applicative <- Common.makeRandomModule Module.controlApplicative
  quickCheck <- Common.makeRandomModule Module.testQuickCheck
  let lImportDecls =
        Hs.importDecls
          srcSpan
          [ (Module.controlApplicative, applicative),
            (Module.testQuickCheck, quickCheck)
          ]

      bindStmts =
        fmap
          ( \(_, var) ->
              Hs.bindStmt srcSpan (Hs.varPat srcSpan var)
                . Hs.qualVar srcSpan quickCheck
                $ Ghc.mkVarOcc "arbitrary"
          )
          fields

      lastStmt =
        Hs.lastStmt srcSpan
          . Hs.app srcSpan (Hs.qualVar srcSpan applicative $ Ghc.mkVarOcc "pure")
          . Hs.recordCon srcSpan (Ghc.reLocA . Ghc.L srcSpan $ Constructor.name constructor)
          . Hs.recFields
          $ fmap
            ( \(field, var) ->
                Hs.recField
                  srcSpan
                  (Hs.fieldOcc srcSpan . Hs.unqual srcSpan $ Field.name field)
                  $ Hs.var srcSpan var
            )
            fields

      lHsBind =
        Common.makeLHsBind srcSpan (Ghc.mkVarOcc "arbitrary") []
          . Hs.doExpr srcSpan
          $ bindStmts
            <> [lastStmt]

      lHsDecl =
        Common.makeInstanceDeclaration
          srcSpan
          type_
          quickCheck
          (Ghc.mkClsOcc "Arbitrary")
          [lHsBind]

  pure (lImportDecls, [lHsDecl])

fromField ::
  Ghc.SrcSpan -> Field.Field -> Ghc.Hsc (Field.Field, Ghc.LIdP Ghc.GhcPs)
fromField srcSpan field = do
  var <-
    Common.makeRandomVariable srcSpan
      . (<> "_")
      . Ghc.occNameString
      $ Field.name field
  pure (field, var)
