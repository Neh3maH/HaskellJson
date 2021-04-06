module Json.ConversionTest
( testJsonConversion
) where

import Test.HUnit
import Json.Conversion
import Json.Types
import Data.Map as Map
import Data.Map(Map)
import Control.Applicative

-- test numbers conversions
testIntSuccess      = TestCase (assertEqual "get an int from a JsonInt"
  (Right 1)
  (json2Int $ JsonNum $ JsonInt 1))
testIntFailureFloat = TestCase (assertEqual "fail on getting an int from a JsonFloat"
  (jsonConversionError "int")
  (json2Int $ JsonNum $ JsonFloat 1.0))
testIntFailure      = TestCase (assertEqual "fail on getting an int from not a JsonInt"
  (jsonConversionError "int")
  (json2Int $ JsonStr "hello"))

testIntegers = TestList
  [ testIntSuccess
  , testIntFailureFloat
  , testIntFailure
  ]

testFloatSuccess    = TestCase (assertEqual "get a float from a JsonFloat"
  (Right 3.14)
  (json2Float $ JsonNum $ JsonFloat 3.14))
testFloatFromInt    = TestCase (assertEqual "get a frloat from a JsonInt"
  (Right 3)
  (json2Float $ JsonNum $ JsonInt 3))
testFloatFailure    = TestCase (assertEqual "fail on getting a float from not a JsonNum"
  (jsonConversionError "float")
  (json2Float $ JsonStr "hello"))

testFloats = TestList
  [ testFloatSuccess
  , testFloatFromInt
  , testFloatFailure
  ]

testDoubleSuccess    = TestCase (assertEqual "get a double from a JsonFloat"
  (Right 3.14)
  (json2Double $ JsonNum $ JsonFloat 3.14))
testDoubleFromInt    = TestCase (assertEqual "get a frloat from a JsonInt"
  (Right 3)
  (json2Double $ JsonNum $ JsonInt 3))
testDoubleFailure    = TestCase (assertEqual "fail on getting a double from not a JsonNum"
  (jsonConversionError "double")
  (json2Double $ JsonStr "hello"))

testDoubles = TestList
  [ testDoubleSuccess
  , testDoubleFromInt
  , testDoubleFailure
  ]

testNumber = TestList
  [ TestLabel "test integer conversions" testIntegers
  , TestLabel "test float conversions" testFloats
  , TestLabel "test double conversions" testDoubles
  ]


-- test boolean conversions
testTrue        = TestCase (assertEqual "get a true value"
  (Right True)
  (json2Bool $ JsonBool True))
testFalse       = TestCase (assertEqual "get a false value"
  (Right False)
  (json2Bool $ JsonBool False))
testBoolFailure = TestCase (assertEqual "fail on getting a boolean from not a JsonBool"
  (jsonConversionError "boolean")
  (json2Bool $ JsonStr "hello"))

testBoolean = TestList
  [ testTrue
  , testFalse
  , testBoolFailure
  ]

-- test string conversions
testStrSuccess    = TestCase (assertEqual "get a String from a JsonStr"
  (Right "hello")
  (json2Str $ JsonStr "hello"))
testStrFromInt    = TestCase (assertEqual "get a String from a JsonInt"
  (Right "1")
  (json2Str $ JsonNum $ JsonInt 1))
testStrFromFloat  = TestCase (assertEqual "get a String from a JsonInt"
  (Right "3.14")
  (json2Str $ JsonNum $ JsonFloat 3.14))
testStrFromBool   = TestCase (assertEqual "get a String from a JsonBool"
  (Right "True")
  (json2Str $ JsonBool True))

testString = TestList
  [ testStrSuccess
  , testStrFromInt
  , testStrFromFloat
  , testStrFromBool
  ]


-- test array conversions
testHomogenousArray = TestCase (assertEqual "get an homogenous array"
  (Right [True, False])
  (fromJson (JsonArray [JsonBool True, JsonBool False]) :: JsonConversionReturn [Bool]))
testMixedNumArray = TestCase (assertEqual "get a mixed numbers array as floats"
  (Right [1.0, 3.14])
  (fromJson (JsonArray [JsonNum $ JsonInt 1, JsonNum $ JsonFloat 3.14]) :: JsonConversionReturn [Double]))
testMixedStringArray = TestCase (assertEqual "get a mixed types array as strings"
  (Right ["3.14", "True"])
  (fromJson (JsonArray [JsonNum $ JsonFloat 3.14, JsonBool True]) :: JsonConversionReturn [String]))

testArray = TestList
  [ testHomogenousArray
  , testMixedNumArray
  , testMixedStringArray
  ]


-- test map conversions
testHomogenousMap = TestCase (assertEqual "get an integer map"
  (Right $ Map.fromList [("one", 1), ("two", 2)])
  (fromJson (JsonObject $ Map.fromList [("one", JsonNum $ JsonInt 1), ("two", JsonNum $ JsonInt 2)]) :: JsonConversionReturn (Map String Int)))

testMap = TestList
  [ testHomogenousMap
  ]


-- test object conversions
data TestObject = TestObject String Int Bool deriving (Eq, Show)
instance JsonConvertible TestObject where
  fromJson (JsonObject value) = case lookupJsonFields value ["str", "int", "bool"] of
    Just [str, int, bool] -> (TestObject <$> (json2Str str)) <*> (json2Int int) <*> (json2Bool bool)
    _ -> jsonConversionError "TestObject"
  fromJson _ = jsonConversionError "TestObject"
  toJson (TestObject str int bool) = JsonObject $ Map.fromList [("str", toJson str), ("int", toJson int), ("bool", toJson bool)]

testSingleConstructorObject =
  let testObj = TestObject "hello" 7 False in
  TestCase (assertEqual "roundtrip an object with a single constructor"
    (Right testObj)
    (fromJson (toJson testObj) :: JsonConversionReturn TestObject)
  )


data TestObject' = A String | B Bool deriving (Eq, Show)
instance JsonConvertible TestObject' where
  fromJson (JsonObject value) = case (Map.lookup "type" value) of
    Just (JsonStr "A") ->
      let toTestObject json = A <$> (json2Str json) in
      maybe (jsonConversionError "testObject'") toTestObject (Map.lookup "str" value)
    Just (JsonStr "B") ->
      let toTestObject json = B <$> (json2Bool json) in
      maybe (jsonConversionError "testObject'") toTestObject (Map.lookup "bool" value)
    _ -> jsonConversionError "TestObject'"
  fromJson _ = jsonConversionError "TestObject'"
  toJson (A str)  = JsonObject $ Map.fromList [("type", JsonStr "A"), ("str", JsonStr str)]
  toJson (B bool) = JsonObject $ Map.fromList [("type", JsonStr "B"), ("bool", JsonBool bool)]

testMultipleConstructorObjectA =
  let testObj = A "blop" in
  TestCase (assertEqual "roundtrip an object with multiple constructors"
    (Right testObj)
    (fromJson (toJson testObj) :: JsonConversionReturn TestObject')
  )

testObject = TestList
  [ testSingleConstructorObject
  , testMultipleConstructorObjectA
  ]

testJsonConversion = TestList
  [ TestLabel "test numbers conversion" testNumber
  , TestLabel "test booleans conversion" testBoolean
  , TestLabel "test string conversion" testString
  , TestLabel "test array conversions" testArray
  , TestLabel "test map conversions" testMap
  , TestLabel "test object conversion" testObject
  ]
