module Evoke.Constant.Module
  ( controlApplicative
  , controlLens
  , dataAeson
  , dataHashMapStrictInsOrd
  , dataMaybe
  , dataMonoid
  , dataProxy
  , dataSwagger
  , dataText
  , testQuickCheck
  ) where

import qualified Module as Ghc

controlApplicative :: Ghc.ModuleName
controlApplicative = Ghc.mkModuleName "Control.Applicative"

controlLens :: Ghc.ModuleName
controlLens = Ghc.mkModuleName "Control.Lens"

dataAeson :: Ghc.ModuleName
dataAeson = Ghc.mkModuleName "Data.Aeson"

dataHashMapStrictInsOrd :: Ghc.ModuleName
dataHashMapStrictInsOrd = Ghc.mkModuleName "Data.HashMap.Strict.InsOrd"

dataMaybe :: Ghc.ModuleName
dataMaybe = Ghc.mkModuleName "Data.Maybe"

dataMonoid :: Ghc.ModuleName
dataMonoid = Ghc.mkModuleName "Data.Monoid"

dataProxy :: Ghc.ModuleName
dataProxy = Ghc.mkModuleName "Data.Proxy"

dataSwagger :: Ghc.ModuleName
dataSwagger = Ghc.mkModuleName "Data.Swagger"

dataText :: Ghc.ModuleName
dataText = Ghc.mkModuleName "Data.Text"

testQuickCheck :: Ghc.ModuleName
testQuickCheck = Ghc.mkModuleName "Test.QuickCheck"
