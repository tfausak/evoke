module Evoke.Type.Config
  ( Config(..)
  , fromFlags
  ) where

import qualified Data.List as List
import qualified Evoke.Type.Flag as Flag

data Config = Config
  { help :: Bool
  , verbose :: Bool
  , version :: Bool
  }
  deriving (Eq, Show)

initial :: Config
initial = Config { help = False, verbose = False, version = False }

fromFlags :: Foldable t => t Flag.Flag -> Config
fromFlags = List.foldl' applyFlag initial

applyFlag :: Config -> Flag.Flag -> Config
applyFlag config flag = case flag of
  Flag.Help -> config { help = True }
  Flag.Verbose -> config { verbose = True }
  Flag.Version -> config { version = True }
