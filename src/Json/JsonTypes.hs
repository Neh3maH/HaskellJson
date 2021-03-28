module Json.JsonTypes
( Json(JsonObject, JsonArray, JsonStr, JsonNum, JsonBool)
) where

import Data.Map(Map)
import Data.Map as Map hiding (Map)
import Data.List as List

data Json = JsonObject (Map String Json) | JsonArray [Json] | JsonStr String | JsonNum Int | JsonBool Bool

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
