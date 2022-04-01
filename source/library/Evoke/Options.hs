module Evoke.Options
  ( parse
  ) where

import qualified Control.Monad as Monad
import qualified Evoke.Hsc as Hsc
import qualified GHC.Plugins as Ghc
import qualified System.Console.GetOpt as Console

-- | Parses command line options. Adds warnings and throws errors as
-- appropriate. Returns the list of parsed options.
parse :: [Console.OptDescr a] -> [String] -> Ghc.SrcSpan -> Ghc.Hsc [a]
parse optDescrs strings srcSpan = do
  let
    (xs, args, opts, errs) = Console.getOpt' Console.Permute optDescrs strings
  Monad.forM_ opts
    $ Hsc.addWarning srcSpan
    . Ghc.text
    . mappend "unknown option: "
    . quote
  Monad.forM_ args
    $ Hsc.addWarning srcSpan
    . Ghc.text
    . mappend "unexpected argument: "
    . quote
  Monad.unless (null errs)
    . Hsc.throwError srcSpan
    . Ghc.vcat
    . fmap Ghc.text
    . lines
    $ mconcat errs
  pure xs

-- | Quotes a string using GHC's weird quoting format.
--
-- >>> quote "thing"
-- "`thing'"
quote :: String -> String
quote string = "`" <> string <> "'"
