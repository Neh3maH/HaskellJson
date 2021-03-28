module Json.ConversionTest
( testJsonConversion
) where

import Test.HUnit
import Json.Conversion
import Json.Types

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
testMap = TestList []


-- test object conversions
testObject = TestList []

testJsonConversion = TestList
  [ TestLabel "test numbers conversion" testNumber
  , TestLabel "test booleans conversion" testBoolean
  , TestLabel "test string conversion" testString
  , TestLabel "test array conversions" testArray
  , TestLabel "test map conversions" testMap
  , TestLabel "test object conversion" testObject
  ]
