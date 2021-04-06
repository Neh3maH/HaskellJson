import Test.HUnit
import Json.ParserTest
import Json.ConversionTest
import Json.E2ERoundTripsTest

tests = TestList
  [ testJsonParser
  , testJsonConversion
  , testJsonE2ERoundTrips
  ]

main :: IO ()
main = do
  runTestTT tests
  return ()
