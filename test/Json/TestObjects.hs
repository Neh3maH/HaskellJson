module Json.TestObjects
( TestObject(..)
, TestMultCtors(..)
) where

import Data.Map as Map
import Json.Conversion
import Json.Types

data TestObject = TestObject String Int Bool deriving (Eq, Show)

instance JsonConvertible TestObject where
  fromJson (JsonObject value) = case lookupJsonFields value ["str", "int", "bool"] of
    Just [str, int, bool] -> (TestObject <$> (json2Str str)) <*> (json2Int int) <*> (json2Bool bool)
    _ -> jsonConversionError "TestObject"
  fromJson _ = jsonConversionError "TestObject"
  toJson (TestObject str int bool) = JsonObject $ Map.fromList [("str", toJson str), ("int", toJson int), ("bool", toJson bool)]


data TestMultCtors = A String | B Bool deriving (Eq, Show)

instance JsonConvertible TestMultCtors where
  fromJson (JsonObject value) = case (Map.lookup "type" value) of
    Just (JsonStr "A") ->
      let toTestObject json = A <$> (json2Str json) in
      maybe (jsonConversionError "TestMultCtors") toTestObject (Map.lookup "str" value)
    Just (JsonStr "B") ->
      let toTestObject json = B <$> (json2Bool json) in
      maybe (jsonConversionError "TestMultCtors") toTestObject (Map.lookup "bool" value)
    _ -> jsonConversionError "TestMultCtors"
  fromJson _ = jsonConversionError "TestMultCtors"
  toJson (A str)  = JsonObject $ Map.fromList [("type", JsonStr "A"), ("str", JsonStr str)]
  toJson (B bool) = JsonObject $ Map.fromList [("type", JsonStr "B"), ("bool", JsonBool bool)]
