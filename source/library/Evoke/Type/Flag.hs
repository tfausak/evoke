module Evoke.Type.Flag
  ( Flag (..),
    options,
  )
where

import qualified System.Console.GetOpt as Console

data Flag
  = Help
  | Verbose
  | Version
  deriving (Eq, Show)

options :: [Console.OptDescr Flag]
options =
  [ Console.Option
      ['h', '?']
      ["help"]
      (Console.NoArg Help)
      "shows this help message and exits",
    Console.Option
      ['v']
      ["version"]
      (Console.NoArg Version)
      "shows the version number and exits",
    Console.Option
      []
      ["verbose"]
      (Console.NoArg Verbose)
      "outputs derived instances"
  ]
