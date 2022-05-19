module Evoke.Hsc
  ( addWarning
  , throwError
  ) where

import qualified GHC.Data.Bag as Ghc
import qualified Control.Monad.IO.Class as IO
import qualified GHC.Utils.Error as Ghc
import qualified GHC.Plugins as Ghc

-- | Adds a warning, which only causes compilation to fail if @-Werror@ is
-- enabled.
addWarning :: Ghc.SrcSpan -> Ghc.MsgDoc -> Ghc.Hsc ()
addWarning srcSpan msgDoc = do
  dynFlags <- Ghc.getDynFlags
  IO.liftIO
    . Ghc.printOrThrowWarnings dynFlags
    . Ghc.unitBag
    $ Ghc.mkPlainWarnMsg dynFlags srcSpan msgDoc

-- | Throws an error, which will cause compilation to fail.
throwError :: Ghc.SrcSpan -> Ghc.MsgDoc -> Ghc.Hsc a
throwError srcSpan msgDoc = do
  dynFlags <- Ghc.getDynFlags
  Ghc.throwOneError $ Ghc.mkPlainErrMsg dynFlags srcSpan msgDoc
