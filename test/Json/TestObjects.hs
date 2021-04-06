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
  fromJson (JsonObject value) =
    let keySet = ["str", "int", "bool"] in
    case lookupJsonFields value keySet of
      Just [str, int, bool] -> (TestObject <$> (json2Str str)) <*> (json2Int int) <*> (json2Bool bool)
      _ -> jsonConversionErrorObj value (AbsentKeySet "TestObject" keySet)
  fromJson json = jsonConversionError json (WrongType "TestObject")
  toJson (TestObject str int bool) = JsonObject $ Map.fromList [("str", toJson str), ("int", toJson int), ("bool", toJson bool)]

json2TestObject :: Json -> JsonConversionReturn TestObject
json2TestObject = fromJson :: Json -> JsonConversionReturn TestObject


data TestMultCtors = A String | B Bool deriving (Eq, Show)

instance JsonConvertible TestMultCtors where
  fromJson (JsonObject value) =
    let missingKeyError k = jsonConversionErrorObj value (MissingKey "TestMultCtors" k) in
    case (Map.lookup "type" value) of
      Just (JsonStr "A") ->
        let toTestObject json = A <$> (json2Str json) in
        maybe (missingKeyError "str") toTestObject (Map.lookup "str" value)
      Just (JsonStr "B") ->
        let toTestObject json = B <$> (json2Bool json) in
        maybe (missingKeyError "bool") toTestObject (Map.lookup "bool" value)
      _ -> missingKeyError "type"
  fromJson json = jsonConversionError json (WrongType "TestMultCtors")
  toJson (A str)  = JsonObject $ Map.fromList [("type", JsonStr "A"), ("str", JsonStr str)]
  toJson (B bool) = JsonObject $ Map.fromList [("type", JsonStr "B"), ("bool", JsonBool bool)]

json2TestMultCtors :: Json -> JsonConversionReturn TestMultCtors
json2TestMultCtors = fromJson :: Json -> JsonConversionReturn TestMultCtors

data TestListObject = TestListObject [Int] deriving (Eq, Show)

instance JsonConvertible TestListObject where
  fromJson (JsonObject value) =
    let error = jsonConversionErrorObj value (MissingKey "TestListObject" "lst") in
    let content = maybe error (\x -> fromJson x :: JsonConversionReturn [Int]) (Map.lookup "lst" value) in
    TestListObject <$> content
  fromJson json = jsonConversionError json (WrongType "TestListObject")
  toJson (TestListObject value) = JsonObject $ Map.singleton "lst" (toJson value)

json2TestListObject :: Json -> JsonConversionReturn TestListObject
json2TestListObject = fromJson :: Json -> JsonConversionReturn TestListObject


data TestNested = TestNested TestObject deriving (Eq, Show)

instance JsonConvertible TestNested where
  fromJson (JsonObject value) =
    let error = jsonConversionErrorObj value (MissingKey "TestNested" "internal") in
    let content = maybe error json2TestObject (Map.lookup "internal" value) in
    TestNested <$> content
  fromJson json = jsonConversionError json (WrongType "TestNested")
  toJson (TestNested value) = JsonObject $ Map.singleton "internal" (toJson value)

json2TestNested :: Json -> JsonConversionReturn TestNested
json2TestNested = fromJson :: Json -> JsonConversionReturn TestNested


data TestNestedList = TestNestedList [TestMultCtors] deriving (Eq, Show)

instance JsonConvertible TestNestedList where
  fromJson (JsonObject value) =
    let error = jsonConversionErrorObj value (MissingKey "TestNestedList" "lst") in
    let content = maybe error (\x -> fromJson x :: JsonConversionReturn [TestMultCtors]) (Map.lookup "lst" value) in
    TestNestedList <$> content
  fromJson json = jsonConversionError json (WrongType "TestNestedList")
  toJson (TestNestedList value) = JsonObject $ Map.singleton "lst" (toJson value)

json2TestNestedList :: Json -> JsonConversionReturn TestNestedList
json2TestNestedList = fromJson :: Json -> JsonConversionReturn TestNestedList

