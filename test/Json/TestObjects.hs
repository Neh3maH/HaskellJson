module Json.TestObjects
( TestObject(..)
, TestMultCtors(..)
, TestListObject(..)
, TestNested(..)
, TestNestedList(..)
, json2TestObject
, json2TestMultCtors
, json2TestListObject
, json2TestNested
, json2TestNestedList
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

json2TestObject :: Json -> JsonConversionReturn TestObject
json2TestObject = fromJson :: Json -> JsonConversionReturn TestObject


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

json2TestMultCtors :: Json -> JsonConversionReturn TestMultCtors
json2TestMultCtors = fromJson :: Json -> JsonConversionReturn TestMultCtors

data TestListObject = TestListObject [Int] deriving (Eq, Show)

instance JsonConvertible TestListObject where
  fromJson (JsonObject value) =
    let content = maybe (jsonConversionError "TestListObject at maybe") (\x -> fromJson x :: JsonConversionReturn [Int]) (Map.lookup "lst" value) in
    TestListObject <$> content
  fromJson _ = jsonConversionError "TestListObject"
  toJson (TestListObject value) = JsonObject $ Map.singleton "lst" (toJson value)

json2TestListObject :: Json -> JsonConversionReturn TestListObject
json2TestListObject = fromJson :: Json -> JsonConversionReturn TestListObject


data TestNested = TestNested TestObject deriving (Eq, Show)

instance JsonConvertible TestNested where
  fromJson (JsonObject value) =
    let content = maybe (jsonConversionError "TestListObject at maybe") (\x -> fromJson x :: JsonConversionReturn TestObject) (Map.lookup "internal" value) in
    TestNested <$> content
  fromJson _ = jsonConversionError "TestNested"
  toJson (TestNested value) = JsonObject $ Map.singleton "internal" (toJson value)

json2TestNested :: Json -> JsonConversionReturn TestNested
json2TestNested = fromJson :: Json -> JsonConversionReturn TestNested


data TestNestedList = TestNestedList [TestMultCtors] deriving (Eq, Show)

instance JsonConvertible TestNestedList where
  fromJson (JsonObject value) =
    let content = maybe (jsonConversionError "TestListNested at maybe") (\x -> fromJson x :: JsonConversionReturn [TestMultCtors]) (Map.lookup "lst" value) in
    TestNestedList <$> content
  fromJson _ = jsonConversionError "TestListNested"
  toJson (TestNestedList value) = JsonObject $ Map.singleton "lst" (toJson value)

json2TestNestedList :: Json -> JsonConversionReturn TestNestedList
json2TestNestedList = fromJson :: Json -> JsonConversionReturn TestNestedList

