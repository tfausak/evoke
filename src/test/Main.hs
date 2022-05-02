{-# OPTIONS_GHC -fplugin=Evoke #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ScopedTypeVariables #-}

import qualified Data.Aeson as Aeson
import qualified Data.Aeson.QQ.Simple as Aeson
import qualified Data.Proxy as Proxy
import qualified Data.Swagger as Swagger
import qualified Test.HUnit as Unit
import qualified Test.QuickCheck as QuickCheck

main :: IO ()
main = Unit.runTestTTAndExit $ Unit.test
  [ testToJSON T1C1 { t1c1f1 = 1 } [Aeson.aesonQQ| { "t1c1f1": 1 } |]
  , testToJSON
    T2C1 { t2c1f1 = 2, t2c1f2 = 3 }
    [Aeson.aesonQQ| { "t2c1f1": 2, "t2c1f2": 3 } |]
  , testToJSON T3C1 { t3c1f1aA = 4 } [Aeson.aesonQQ| { "t3c1f1a_a": 4 } |]
  , testToJSON T4C1 { t4c1f1aA = 5 } [Aeson.aesonQQ| { "t4c1f1a-a": 5 } |]
  , testToJSON T5C1 { t5c1f1 = 6 } [Aeson.aesonQQ| { "T5c1f1": 6 } |]
  , testToJSON T6C1 { t6c1f1 = 7 } [Aeson.aesonQQ| { "f1": 7 } |]
  , testToJSON T7C1 { t7c1f1 = 8 } [Aeson.aesonQQ| { "F1": 8 } |]
  , testToJSON T8C1 { t8c1f1 = 9 } [Aeson.aesonQQ| { "f1": 9 } |]
  , testToJSON
    (T9C1 { t9c1f1 = 10 } :: T9 X)
    [Aeson.aesonQQ| { "t9c1f1": 10 } |]
  , testToJSON
    T10C1 { t10c1f1 = 11 :: Int }
    [Aeson.aesonQQ| { "t10c1f1": 11 } |]
  , testToJSON T11C1 { t11c1f1A = 12 } [Aeson.aesonQQ| { "a": 12 } |]
  , testToJSON
    (T12C1 { t12c1f1 = 13, t12c1f2 = 14 } :: T12 X Int X Int X)
    [Aeson.aesonQQ| { "t12c1f1": 13, "t12c1f2": 14 } |]
  , testToJSON
    T13C1 { t13c1f1 = 15, t13c1f2 = 16 }
    [Aeson.aesonQQ| { "t13c1f1": 15, "t13c1f2": 16 } |]
  , testToJSON T14C1 { t14c1f1 = Just 17 } [Aeson.aesonQQ| { "t14c1f1": 17 } |]
  , checkInstances (Proxy.Proxy :: Proxy.Proxy T1)
  , checkInstances (Proxy.Proxy :: Proxy.Proxy T2)
  , checkInstances (Proxy.Proxy :: Proxy.Proxy T3)
  , checkInstances (Proxy.Proxy :: Proxy.Proxy T4)
  , checkInstances (Proxy.Proxy :: Proxy.Proxy T5)
  , checkInstances (Proxy.Proxy :: Proxy.Proxy T6)
  , checkInstances (Proxy.Proxy :: Proxy.Proxy T7)
  , checkInstances (Proxy.Proxy :: Proxy.Proxy T8)
  , checkInstances (Proxy.Proxy :: Proxy.Proxy (T9 X))
  , checkInstances (Proxy.Proxy :: Proxy.Proxy (T10 Int))
  , checkInstances (Proxy.Proxy :: Proxy.Proxy T11)
  , checkInstances (Proxy.Proxy :: Proxy.Proxy (T12 X Int X Int X))
  , checkInstances (Proxy.Proxy :: Proxy.Proxy T13)
  , checkInstances (Proxy.Proxy :: Proxy.Proxy T14)
  ]

checkInstances
  :: ( QuickCheck.Arbitrary a
     , Eq a
     , Aeson.FromJSON a
     , Show a
     , Aeson.ToJSON a
     , Swagger.ToSchema a
     )
  => Proxy.Proxy a
  -> Unit.Test
checkInstances proxy = Unit.TestCase $ do
  value <- QuickCheck.generate QuickCheck.arbitrary
  Aeson.decode (Aeson.encode value) Unit.@?= Just value
  let json = Aeson.toJSON $ Proxy.asProxyTypeOf value proxy
  Aeson.fromJSON json Unit.@?= Aeson.Success value
  Swagger.validateJSON mempty (Swagger.toSchema proxy) json Unit.@?= []

testToJSON :: Aeson.ToJSON a => a -> Aeson.Value -> Unit.Test
testToJSON = (Unit.~?=) . Aeson.toJSON

-- custom void type with no instances
data X

-- one field
newtype T1 = T1C1
  { t1c1f1 :: Int
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke"

-- multiple fields
data T2 = T2C1
  { t2c1f1 :: Int
  , t2c1f2 :: Int
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke"

-- snake case
newtype T3 = T3C1
  { t3c1f1aA :: Int
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke --snake"

-- kebab case
newtype T4 = T4C1
  { t4c1f1aA :: Int
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke --kebab"

-- capitalized
newtype T5 = T5C1
  { t5c1f1 :: Int
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke --title"

-- stripped prefix
newtype T6 = T6C1
  { t6c1f1 :: Int
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke --prefix t6c1"

-- stripped then capitalized
newtype T7 = T7C1
  { t7c1f1 :: Int
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke --prefix t7c1 --title"

-- capitalized then stripped
newtype T8 = T8C1
  { t8c1f1 :: Int
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke --title --prefix T8c1"

-- phantom type
newtype T9 a = T9C1
  { t9c1f1 :: Int
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke"

-- type variable
newtype T10 a = T10C1
  { t10c1f1 :: a
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke"

-- lower cased
newtype T11 = T11C1
  { t11c1f1A :: Int
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke --prefix t11c1f1 --camel"

-- multiple type variables
data T12 a b c d e = T12C1
  { t12c1f1 :: d
  , t12c1f2 :: b
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke"

-- multiple fields with one signature
data T13 = T13C1
  { t13c1f1, t13c1f2 :: Int
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke"

-- optional field
newtype T14 = T14C1
  { t14c1f1 :: Maybe Int
  }
  deriving (Eq, Show)
  deriving (Arbitrary, FromJSON, ToJSON, ToSchema) via "Evoke"
