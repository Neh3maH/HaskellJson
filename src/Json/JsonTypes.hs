module Json.JsonTypes
( Json(JsonObject, JsonArray, JsonStr, JsonNum, JsonBool)
, JNum(JsonInt, JsonFloat)
) where

import Data.Map(Map)
import Data.Map as Map hiding (Map)
import Data.List as List
import GHC.Float as Float

data JNum = JsonInt Int | JsonFloat Double
data Json = JsonObject (Map String Json) | JsonArray [Json] | JsonStr String | JsonNum JNum | JsonBool Bool

instance Show Json where
  show (JsonStr value)    = value
  show (JsonNum value)    = show value
  show (JsonBool value)   = show value
  show (JsonArray [])     = "[]"
  show (JsonArray value)  =
    let valueStr = List.concat $ List.map (\e -> '\n' : (show e)) value in
    "[\n" ++ valueStr ++ "\n]"
  show (JsonObject value) | Map.null value = "{}"
  show (JsonObject value) =
    let showKV k v = (show k) ++ ": " ++ (show v) in
    let valueStr = Map.foldlWithKey (\acc k v -> acc ++ (showKV k v) ++ "\n") "" value in
    "{\n" ++ valueStr ++ "}"

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
