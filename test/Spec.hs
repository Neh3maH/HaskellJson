import Test.HUnit
import Json.ParserTest

tests = testJsonParser

main :: IO ()
main = do
  runTestTT tests
  return ()
