module Json.JsonParserTest
( testJsonParser
) where

import Test.HUnit
import Text.ParserCombinators.ReadP
import Json.JsonParser
import Json.JsonTypes
import Data.Semigroup

-- test numbers
producerNum = readP_to_S number

testDigit  = TestCase (assertEqual "test a single digit" [(JsonNum 1, "")] (producerNum "1"))
testDigits = TestCase (assertEqual "test multiple digits" [(JsonNum 123, "")] (producerNum "123"))

testNumber = TestList
  [ TestLabel "test digit" testDigit
  , TestLabel "test digits" testDigits
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
testSingleElemArray = TestCase (assertEqual "test an array with a single element" [(JsonArray [JsonNum 1], "")] (producerArray "[1]"))
testMultipleElemsArray = TestCase (assertEqual "test an array with multiple elements" [(JsonArray [JsonNum 1, JsonNum 2, JsonNum 3], "")] (producerArray "[1,2,3]"))
testSpacedArray = TestCase (assertEqual "test an array with some spaces" [(JsonArray [JsonNum 1, JsonNum 2, JsonNum 3], "")] (producerArray "[  1, 2  , 3  ]"))
testTrailingComaArray = TestCase (assertEqual "test an array with a trailing coma" [(JsonArray [JsonNum 1], "")] (producerArray "[1,]"))
testMultipleTypesArray = TestCase (assertEqual "test an array with heterogenous types" [(JsonArray [JsonBool True, JsonNum 2, JsonStr "3"], "")] (producerArray "[true,2,\"3\"]"))

testArray = TestList
  [ testEmptyArray
  , testSingleElemArray
  , testMultipleElemsArray
  , testSpacedArray
  , testTrailingComaArray
  , testMultipleTypesArray
  ]

-- all tests
testJsonParser = TestList [testNumber, testBool, testStr, testArray]
