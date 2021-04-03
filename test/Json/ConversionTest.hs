module Json.ConversionTest
( testJsonConversion
) where

import Test.HUnit
import Json.Conversion
import Json.Types
import Data.Map as Map
import Data.Map(Map)

-- test numbers conversions
testIntSuccess      = TestCase (assertEqual "get an int from a JsonInt"
  (Right 1)
  (fromJson (JsonNum $ JsonInt 1) :: JsonConversionReturn Int))
testIntFailureFloat = TestCase (assertEqual "fail on getting an int from a JsonFloat"
  (jsonConversionError "int")
  (fromJson (JsonNum $ JsonFloat 1.0) :: JsonConversionReturn Int))
testIntFailure      = TestCase (assertEqual "fail on getting an int from not a JsonInt"
  (jsonConversionError "int")
  (fromJson (JsonStr "hello") :: JsonConversionReturn Int))

testIntegers = TestList
  [ testIntSuccess
  , testIntFailureFloat
  , testIntFailure
  ]

testFloatSuccess    = TestCase (assertEqual "get a float from a JsonFloat"
  (Right 3.14)
  (fromJson (JsonNum $ JsonFloat 3.14) :: JsonConversionReturn Float))
testFloatFromInt    = TestCase (assertEqual "get a frloat from a JsonInt"
  (Right 3)
  (fromJson (JsonNum $ JsonInt 3) :: JsonConversionReturn Float))
testFloatFailure    = TestCase (assertEqual "fail on getting a float from not a JsonNum"
  (jsonConversionError "float")
  (fromJson (JsonStr "hello") :: JsonConversionReturn Float))

testFloats = TestList
  [ testFloatSuccess
  , testFloatFromInt
  , testFloatFailure
  ]

testDoubleSuccess    = TestCase (assertEqual "get a double from a JsonFloat"
  (Right 3.14)
  (fromJson (JsonNum $ JsonFloat 3.14) :: JsonConversionReturn Double))
testDoubleFromInt    = TestCase (assertEqual "get a frloat from a JsonInt"
  (Right 3)
  (fromJson (JsonNum $ JsonInt 3) :: JsonConversionReturn Double))
testDoubleFailure    = TestCase (assertEqual "fail on getting a double from not a JsonNum"
  (jsonConversionError "double")
  (fromJson (JsonStr "hello") :: JsonConversionReturn Double))

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
  (fromJson (JsonBool True) :: JsonConversionReturn Bool))
testFalse       = TestCase (assertEqual "get a false value"
  (Right False)
  (fromJson (JsonBool False) :: JsonConversionReturn Bool))
testBoolFailure = TestCase (assertEqual "fail on getting a boolean from not a JsonBool"
  (jsonConversionError "boolean")
  (fromJson (JsonStr "hello") :: JsonConversionReturn Bool))

testBoolean = TestList
  [ testTrue
  , testFalse
  , testBoolFailure
  ]

-- test string conversions
testStrSuccess    = TestCase (assertEqual "get a String from a JsonStr"
  (Right "hello")
  (fromJson (JsonStr "hello") :: JsonConversionReturn String))
testStrFromInt    = TestCase (assertEqual "get a String from a JsonInt"
  (Right "1")
  (fromJson (JsonNum $ JsonInt 1) :: JsonConversionReturn String))
testStrFromFloat  = TestCase (assertEqual "get a String from a JsonInt"
  (Right "3.14")
  (fromJson (JsonNum $ JsonFloat 3.14) :: JsonConversionReturn String))
testStrFromBool   = TestCase (assertEqual "get a String from a JsonBool"
  (Right "True")
  (fromJson (JsonBool True) :: JsonConversionReturn String))

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
  fromJson (JsonObject value) = case (Map.lookup "str" value, Map.lookup "int" value, Map.lookup "bool" value) of
    (Just str, Just int, Just bool) -> case (fromJson str :: JsonConversionReturn String, fromJson int :: JsonConversionReturn Int, fromJson bool :: JsonConversionReturn Bool) of
      (Right s, Right i, Right b) -> Right $ TestObject s i b
      _                           -> jsonConversionError "TestObject"
    _                               -> jsonConversionError "TestObject"
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
    Just (JsonStr "A") -> case (Map.lookup "str" value) of
      Just str -> case (fromJson str :: JsonConversionReturn String) of
        Right s   -> Right $ A s
        _         -> jsonConversionError "TestObject'"
      _         -> jsonConversionError "TestObject'"
    Just (JsonStr "B") -> case (Map.lookup "bool" value) of
      Just bool -> case (fromJson bool :: JsonConversionReturn Bool) of
        Right b   -> Right $ B b
        _         -> jsonConversionError "TestObject'"
      _          -> jsonConversionError "TestObject'"
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
