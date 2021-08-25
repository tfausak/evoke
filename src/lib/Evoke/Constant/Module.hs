{- hlint ignore "Use camelCase" -}

module Evoke.Constant.Module
  ( control_applicative
  , control_lens
  , data_aeson
  , data_hashMap_strict_insOrd
  , data_maybe
  , data_monoid
  , data_proxy
  , data_swagger
  , data_text
  , test_quickCheck
  ) where

import qualified Module as Ghc

control_applicative :: Ghc.ModuleName
control_applicative = Ghc.mkModuleName "Control.Applicative"

control_lens :: Ghc.ModuleName
control_lens = Ghc.mkModuleName "Control.Lens"

data_aeson :: Ghc.ModuleName
data_aeson = Ghc.mkModuleName "Data.Aeson"

data_hashMap_strict_insOrd :: Ghc.ModuleName
data_hashMap_strict_insOrd = Ghc.mkModuleName "Data.HashMap.Strict.InsOrd"

data_maybe :: Ghc.ModuleName
data_maybe = Ghc.mkModuleName "Data.Maybe"

data_monoid :: Ghc.ModuleName
data_monoid = Ghc.mkModuleName "Data.Monoid"

data_proxy :: Ghc.ModuleName
data_proxy = Ghc.mkModuleName "Data.Proxy"

data_swagger :: Ghc.ModuleName
data_swagger = Ghc.mkModuleName "Data.Swagger"

data_text :: Ghc.ModuleName
data_text = Ghc.mkModuleName "Data.Text"

test_quickCheck :: Ghc.ModuleName
test_quickCheck = Ghc.mkModuleName "Test.QuickCheck"
