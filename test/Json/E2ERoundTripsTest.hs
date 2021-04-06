module Json.E2ERoundTripsTest
( testJsonE2ERoundTrips
) where

import Test.HUnit
import Json.Conversion
import Json.Parser(parse)
import Json.Types
import Json.TestObjects


fun :: (JsonConvertible a) => (Json -> JsonConversionReturn a) -> a -> JsonConversionReturn a
fun jsonConversion value =
  let error = jsonConversionError (toJson value) (WrongType "idk it failed") in
  maybe error jsonConversion $ parse . show . toJson $ value

testRoundTripObject =
  let object = TestObject "hello" 2 False in
  TestCase ( assertEqual "hello"
    (Right object)
    (fun json2TestObject object))

testRoundTripList =
  let listObject = TestListObject [1, 2, 3, 0] in
  TestCase ( assertEqual "hello2"
    (Right listObject)
    (fun json2TestListObject listObject))

testRoundTripNested =
  let nestedObject = TestNested $ TestObject "str" 3 False in
  TestCase ( assertEqual "hello3"
    (Right nestedObject)
    (fun json2TestNested nestedObject))

testRoundTripNestedList =
  let nestedObjectList = TestNestedList [A "a string", B True] in
  TestCase ( assertEqual "hello4"
    (Right nestedObjectList)
    (fun json2TestNestedList nestedObjectList))

testJsonE2ERoundTrips = TestList
  [ testRoundTripObject
  , testRoundTripList
  , testRoundTripNested
  , testRoundTripNestedList
  ]
