import Test.HUnit
import Json.ParserTest
import Json.ConversionTest

tests = TestList
  [ testJsonParser
  , testJsonConversion
  ]

main :: IO ()
main = do
  runTestTT tests
  return ()
