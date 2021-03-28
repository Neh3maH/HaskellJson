import Test.HUnit
import Json.JsonParserTest

tests = testJsonParser

main :: IO ()
main = do
  runTestTT tests
  return ()
