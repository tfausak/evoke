module Evoke.Type.Field
  ( Field (..),
    make,
    isOptional,
  )
where

import qualified Evoke.Hsc as Hsc
import qualified GHC.Hs as Ghc
import qualified GHC.Plugins as Ghc

data Field = Field
  { name :: Ghc.OccName,
    type_ :: Ghc.HsType Ghc.GhcPs
  }

make ::
  Ghc.SrcSpan ->
  Ghc.LHsType Ghc.GhcPs ->
  Ghc.LFieldOcc Ghc.GhcPs ->
  Ghc.Hsc Field
make srcSpan lHsType lFieldOcc = do
  lRdrName <- case Ghc.unLoc lFieldOcc of
    Ghc.FieldOcc _ x -> pure x
  occName <- case Ghc.unLoc lRdrName of
    Ghc.Unqual x -> pure x
    _ -> Hsc.throwError srcSpan $ Ghc.text "unsupported RdrName"
  pure Field {name = occName, type_ = Ghc.unLoc lHsType}

isOptional :: Field -> Bool
isOptional field = case type_ field of
  Ghc.HsAppTy _ lHsType _ -> case Ghc.unLoc lHsType of
    Ghc.HsTyVar _ _ lIdP -> case Ghc.unLoc lIdP of
      Ghc.Unqual occName -> Ghc.occNameString occName == "Maybe"
      _ -> False
    _ -> False
  _ -> False
