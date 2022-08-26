module Evoke.Hsc
  ( addWarning,
    throwError,
  )
where

import qualified Control.Monad.IO.Class as IO
import qualified GHC as Ghc
import qualified GHC.Data.Bag as Ghc
import qualified GHC.Driver.Errors as Ghc
import qualified GHC.Plugins as Ghc
import qualified GHC.Utils.Error as Ghc

-- | Adds a warning, which only causes compilation to fail if @-Werror@ is
-- enabled.
addWarning :: Ghc.SrcSpan -> Ghc.SDoc -> Ghc.Hsc ()
addWarning srcSpan msgDoc = do
  logger <- Ghc.getLogger
  dynFlags <- Ghc.getDynFlags
  IO.liftIO
    . Ghc.printOrThrowWarnings logger dynFlags
    . Ghc.unitBag
    $ Ghc.mkPlainWarnMsg srcSpan msgDoc

-- | Throws an error, which will cause compilation to fail.
throwError :: Ghc.SrcSpan -> Ghc.SDoc -> Ghc.Hsc a
throwError srcSpan msgDoc = do
  Ghc.throwOneError $ Ghc.mkPlainWarnMsg srcSpan msgDoc
