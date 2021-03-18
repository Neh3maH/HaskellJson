module Json.JsonParserTest
( testJsonParser
) where

import Test.HUnit
import Text.ParserCombinators.ReadP
import Json.JsonParser
import Json.JsonTypes
import Data.Map as Map

-- test numbers
producerNum = readP_to_S number

testDigit  = TestCase (assertEqual "test a single digit" [(JsonNum $ JsonInt 1, "")] (producerNum "1"))
testDigits = TestCase (assertEqual "test multiple digits" [(JsonNum $ JsonInt 123, "")] (producerNum "123"))
testNeg = TestCase (assertEqual "test a negative number" [(JsonNum $ JsonInt $ -9, "")] (producerNum "-9"))
testFloat = TestCase (assertEqual "test a floating point number" [(JsonNum $ JsonFloat 3.14, "")] (producerNum "3.14"))
testFloatNoDec = TestCase (assertEqual "test a floating point number with no decimal part" [(JsonNum $ JsonFloat 7.0, "")] (producerNum "7."))
testFloatNeg = TestCase (assertEqual "test a negative floating point number" [(JsonNum $ JsonFloat $ -3.14, "")] (producerNum "-3.14"))

testNumber = TestList
  [ testDigit
  , testDigits
  , testNeg
  , testFloat
  , testFloatNoDec
  , testFloatNeg
  ]


-- test booleans
producerBool = readP_to_S boolean

testTrueLC  = TestCase (assertEqual "test true" [(JsonBool True, "")] (producerBool "true"))
testTrueUC  = TestCase (assertEqual "test True" [(JsonBool True, "")] (producerBool "True"))
testFalseLC = TestCase (assertEqual "test false" [(JsonBool False, "")] (producerBool "false"))
testFalseUC = TestCase (assertEqual "test False" [(JsonBool False, "")] (producerBool "False"))

testBool = TestList
  [ testTrueLC
  , testTrueUC
  , testFalseLC
  , testFalseUC
  ]


-- test strings
producerStr = readP_to_S str

testEmptyStr      = TestCase (assertEqual "test an empty string" [(JsonStr "", "")] (producerStr "\"\""))
testStdStr        = TestCase (assertEqual "test a simple string" [(JsonStr "abcde0123", "")] (producerStr "\"abcde0123\""))
testEscStr        = TestCase (assertEqual "test a string with some escape chars" [(JsonStr "abc\ndef\t", "")] (producerStr "\"abc\ndef\t\""))
testBackslashStr  = TestCase (assertEqual "test a string with a backslash" [(JsonStr "hello\\world", "")] (producerStr "\"hello\\world\""))
testEscQuoteStr   = TestCase (assertEqual "test a string with an escaped quote" [(JsonStr "name: \"Sasha\"", "")] (producerStr "\"name: \\\"Sasha\\\"\""))

testStr = TestList
  [ testEmptyStr
  , testStdStr
  , testEscStr
  , testBackslashStr
  , testEscQuoteStr
  ]


-- test arrays
producerArray = readP_to_S array

testEmptyArray = TestCase (assertEqual "test an empty array" [(JsonArray [], "")] (producerArray "[]"))
testSingleElemArray = TestCase (assertEqual "test an array with a single element" [(JsonArray [JsonNum $ JsonInt 1], "")] (producerArray "[1]"))
testMultipleElemsArray = TestCase (assertEqual "test an array with multiple elements" [(JsonArray [JsonNum $ JsonInt 1, JsonNum $ JsonInt 2, JsonNum $ JsonInt 3], "")] (producerArray "[1,2,3]"))
testSpacedArray = TestCase (assertEqual "test an array with some spaces" [(JsonArray [JsonNum $ JsonInt 1, JsonNum $ JsonInt 2, JsonNum $ JsonInt 3], "")] (producerArray "[  1, 2  , 3  ]"))
testTrailingComaArray = TestCase (assertEqual "test an array with a trailing coma" [(JsonArray [JsonNum $ JsonInt 1], "")] (producerArray "[1,]"))
testMultipleTypesArray = TestCase (assertEqual "test an array with heterogenous types" [(JsonArray [JsonBool True, JsonNum $ JsonInt 2, JsonStr "3"], "")] (producerArray "[true,2,\"3\"]"))

testArray = TestList
  [ testEmptyArray
  , testSingleElemArray
  , testMultipleElemsArray
  , testSpacedArray
  , testTrailingComaArray
  , testMultipleTypesArray
  ]

-- test objects
producerObj = readP_to_S object

testEmptyObject = TestCase (assertEqual "test an empty object" [(JsonObject Map.empty, "")] (producerObj "{}"))
testObjectWithAtoms = TestCase (assertEqual "test an object with atoms" [(JsonObject $ Map.fromList [("a", JsonNum $ JsonInt 12), ("b", JsonBool True)], "")] (producerObj "{\"a\":12,\"b\":true}"))
testObjectWithNestedArrays = TestCase (assertEqual "test an object with nested arrays"
  [(JsonObject $ Map.fromList [("x", JsonArray []), ("y", JsonArray [JsonNum $ JsonInt 1, JsonNum $ JsonInt 2, JsonNum $ JsonInt 3])], "")]
  (producerObj "{\"x\":[],\"y\":[1,2,3]}"))
testObjectWithNestedObjects = TestCase (assertEqual "test an object with nested objects"
  [(JsonObject $ Map.fromList [("i", JsonObject Map.empty), ("j", JsonObject $ Map.fromList [("u", JsonNum $ JsonInt 2)])], "")] (producerObj "{\"i\":{},\"j\":{\"u\":2}}"))
testObjectWithTrailingComa = TestCase (assertEqual "test an object with a trailing coma" [(JsonObject $ Map.fromList [("k", JsonStr "v")], "")] (producerObj "{\"k\":\"v\",}"))
testSpacedObject = TestCase (assertEqual "test an object with some spaces" [(JsonObject $ Map.fromList [("1", JsonNum $ JsonInt 1), ("2", JsonNum $ JsonInt 2)], "")] (producerObj "{   \"1\"  :   1 , \"2\"  : 2,  }"))

testObject = TestList
  [ testEmptyObject
  , testObjectWithAtoms
  , testObjectWithNestedArrays
  , testObjectWithNestedObjects
  , testObjectWithTrailingComa
  , testSpacedObject
  ]


-- all tests
testJsonParser = TestList
  [ TestLabel "test numbers parsing" testNumber
  , TestLabel "test booleans parsing" testBool
  , TestLabel "test strings parsing" testStr
  , TestLabel "test arrays parsing" testArray
  , TestLabel "test objects parsing" testObject
  ]
