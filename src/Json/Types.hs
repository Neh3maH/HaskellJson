module Json.Types
( Json(JsonObject, JsonArray, JsonStr, JsonNum, JsonBool)
, JNum(JsonInt, JsonFloat)
, prettyprint
) where

import Data.Map(Map)
import Data.Map as Map hiding (Map)
import Data.List as List
import GHC.Float as Float

data JNum = JsonInt Int | JsonFloat Double
data Json = JsonObject (Map String Json) | JsonArray [Json] | JsonStr String | JsonNum JNum | JsonBool Bool


instance Show Json where
  show (JsonStr value)    = '"' : value ++ ['"']
  show (JsonNum value)    = show value
  show (JsonBool value)   = show value
  show (JsonArray [])     = "[]"
  show (JsonArray value)  =
    let valueStr = List.concat $ List.intersperse "," $ List.map (\e -> (show e)) value in
    "[" ++ valueStr ++ "]"
  show (JsonObject value) | Map.null value = "{}"
  show (JsonObject value) =
    let showKV (k, v) = (show k) ++ ":" ++ (show v) in
    let valueStr = List.concat $ List.intersperse "," $ List.map showKV $ Map.toList value in
    "{" ++ valueStr ++ "}"

instance Eq Json where
  (JsonStr a)    == (JsonStr b)    = a == b
  (JsonNum a)    == (JsonNum b)    = a == b
  (JsonBool a )  == (JsonBool b)   = a == b
  (JsonArray a)  == (JsonArray b)  = a == b
  (JsonObject a) == (JsonObject b) = a == b
  _ == _ = False
  x /= y = not (x == y)

instance Show JNum where
  show (JsonInt value) = show value
  show (JsonFloat value) = show value

instance Eq JNum where
  (JsonInt a)   == (JsonInt b)   = a == b
  (JsonFloat a) == (JsonFloat b) = a == b
  (JsonFloat a) == (JsonInt b)   = a == (Float.int2Double b)
  (JsonInt a)   == (JsonFloat b) = b == (Float.int2Double a)
  x /= y = not (x == y)


-- Todo padding
prettyprint :: Json -> String
prettyprint = paddedprettyprint 0

padding :: Int -> String
padding x = List.take x (List.repeat '\t')

paddedprettyprint :: Int -> Json -> String
paddedprettyprint _ (JsonStr value)    = value
paddedprettyprint _ (JsonNum value)    = show value
paddedprettyprint _ (JsonBool value)   = show value
paddedprettyprint _ (JsonArray [])     = "[]"
paddedprettyprint depth (JsonArray value)  =
  let valueStr = List.concat $ List.map (\e -> padding (depth + 1) ++ ('\n' : (prettyprint e))) value in
  "[" ++ valueStr ++ "\n]"
paddedprettyprint depth (JsonObject value) =
  let padElems = padding $ depth + 1 in
  let pad = padding depth in
  let prettyprintKV (k, v) = padElems ++ k ++ ": " ++ (prettyprint v) in
  let valueStr = List.concat $ List.intersperse ",\n" $ List.map prettyprintKV $ Map.toList value in
  "{\n" ++ valueStr ++ "}"
