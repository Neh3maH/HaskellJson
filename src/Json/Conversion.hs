module Json.Conversion
( JsonRead(..)
, JsonConversionReturn
, jsonConversionError
) where

import Data.Map(Map)
import Data.Map as Map
import Data.List as List
import Data.Either as Either
import GHC.Float as Float
import Json.Types

import Data.Char(Char)

type JsonConversionReturn a = Either String a

class JsonRead a where
  fromJson :: Json -> JsonConversionReturn a

jsonConversionError :: String -> JsonConversionReturn a
jsonConversionError s = Left $ "ERROR: Not a " ++ s

instance {-# OVERLAPPING #-} JsonRead String where
  fromJson (JsonStr str) = Right str
  fromJson (JsonNum (JsonFloat value)) = Right $ show value
  fromJson (JsonNum (JsonInt value))   = Right $ show value
  fromJson (JsonBool value)            = Right $ show value
  fromJson _ = jsonConversionError "string"

instance JsonRead Int where
  fromJson (JsonNum (JsonInt value)) = Right value
  fromJson _ = jsonConversionError "int"

instance JsonRead Double where
  fromJson (JsonNum (JsonFloat value)) = Right value
  fromJson (JsonNum (JsonInt value))   = Right $ Float.int2Double value
  fromJson _ = jsonConversionError "double"

instance JsonRead Float where
  fromJson (JsonNum (JsonFloat value)) = Right $ Float.double2Float value
  fromJson (JsonNum (JsonInt value))   = Right $ Float.int2Float value
  fromJson _ = jsonConversionError "float"

instance JsonRead Bool where
  fromJson (JsonBool value) = Right value
  fromJson _ = jsonConversionError "boolean"

instance {-# OVERLAPPABLE #-} (JsonRead a) => JsonRead [a] where
  fromJson (JsonArray values) =
    let eitherValues = List.map fromJson values in
    case Either.partitionEithers eitherValues of
      ([], ret)   -> Right ret
      (err:xs, _) -> Left err
  fromJson _ = jsonConversionError "list"

instance (k ~ [Char], JsonRead v) => JsonRead (Map k v) where
  fromJson (JsonObject value) = case Map.mapEither fromJson value of
    (error, ret) | Map.null error -> Right ret
    (_, _)                        -> jsonConversionError "map"
  fromJson _ = jsonConversionError "map"
