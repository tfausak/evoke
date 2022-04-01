module Evoke.Type.Type
  ( Type(..)
  , make
  , qualifiedName
  ) where

import qualified Control.Monad as Monad
import qualified Evoke.Hsc as Hsc
import qualified Evoke.Type.Constructor as Constructor
import qualified GHC.Hs as Ghc
import qualified GHC.Plugins as Ghc

data Type = Type
  { name :: Ghc.IdP Ghc.GhcPs
  , variables :: [Ghc.IdP Ghc.GhcPs]
  , constructors :: [Constructor.Constructor]
  }

make
  :: Ghc.LIdP Ghc.GhcPs
  -> Ghc.LHsQTyVars Ghc.GhcPs
  -> [Ghc.LConDecl Ghc.GhcPs]
  -> Ghc.SrcSpan
  -> Ghc.Hsc Type
make lIdP lHsQTyVars lConDecls srcSpan = do
  lHsTyVarBndrs <- case lHsQTyVars of
    Ghc.HsQTvs _ hsq_explicit -> pure hsq_explicit
    _ -> Hsc.throwError srcSpan $ Ghc.text "unsupported LHsQTyVars"
  theVariables <- Monad.forM lHsTyVarBndrs $ \lHsTyVarBndr ->
    case Ghc.unLoc lHsTyVarBndr of
      Ghc.UserTyVar _ _ var -> pure $ Ghc.unLoc var
      _ -> Hsc.throwError srcSpan $ Ghc.text "unknown LHsTyVarBndr"
  theConstructors <- mapM (Constructor.make srcSpan) lConDecls
  pure Type
    { name = Ghc.unLoc lIdP
    , variables = theVariables
    , constructors = theConstructors
    }

qualifiedName :: Ghc.ModuleName -> Type -> String
qualifiedName moduleName type_ = mconcat
  [ Ghc.moduleNameString moduleName
  , "."
  , Ghc.occNameString . Ghc.rdrNameOcc $ name type_
  ]
