module Json.Conversion
( JsonConvertible(..)
, JsonConversionReturn
, jsonConversionError
) where

import Data.Map(Map)
import Data.Map as Map
import Data.List as List
import Data.Either as Either
import GHC.Float as Float
import Data.Data
import Json.Types

import Data.Char(Char)

type JsonConversionReturn a = Either String a

class JsonConvertible a where
  fromJson :: Json -> JsonConversionReturn a
  toJson :: a -> Json

jsonConversionError :: String -> JsonConversionReturn a
jsonConversionError s = Left $ "ERROR: Not a " ++ s

instance {-# OVERLAPPING #-} JsonConvertible String where
  fromJson (JsonStr str)               = Right str
  fromJson (JsonNum (JsonFloat value)) = Right $ show value
  fromJson (JsonNum (JsonInt value))   = Right $ show value
  fromJson (JsonBool value)            = Right $ show value
  fromJson _ = jsonConversionError "string"
  toJson str = JsonStr str

instance JsonConvertible Int where
  fromJson (JsonNum (JsonInt value)) = Right value
  fromJson _ = jsonConversionError "int"
  toJson value = JsonNum $ JsonInt value

instance JsonConvertible Double where
  fromJson (JsonNum (JsonFloat value)) = Right value
  fromJson (JsonNum (JsonInt value))   = Right $ Float.int2Double value
  fromJson _ = jsonConversionError "double"
  toJson value = JsonNum $ JsonFloat value

instance JsonConvertible Float where
  fromJson (JsonNum (JsonFloat value)) = Right $ Float.double2Float value
  fromJson (JsonNum (JsonInt value))   = Right $ Float.int2Float value
  fromJson _ = jsonConversionError "float"
  toJson value = JsonNum $ JsonFloat $ Float.float2Double value

instance JsonConvertible Bool where
  fromJson (JsonBool value) = Right value
  fromJson _ = jsonConversionError "boolean"
  toJson value = JsonBool value

instance {-# OVERLAPPABLE #-} (JsonConvertible a) => JsonConvertible [a] where
  fromJson (JsonArray values) =
    let eitherValues = List.map fromJson values in
    case Either.partitionEithers eitherValues of
      ([], ret)   -> Right ret
      (err:xs, _) -> Left err
  fromJson _ = jsonConversionError "list"
  toJson values = JsonArray $ List.map toJson values

instance (k ~ [Char], JsonConvertible v) => JsonConvertible (Map k v) where
  fromJson (JsonObject value) = case Map.mapEither fromJson value of
    (error, ret) | Map.null error -> Right ret
    (_, _)                        -> jsonConversionError "map"
  fromJson _ = jsonConversionError "map"
  toJson value = JsonObject $ Map.map toJson value
