module Json.Conversion
( JsonConvertible(..)
, JsonConversionReturn
, JsonConversionError
, JsonConversionErrLog(..)
, jsonConversionError
, jsonConversionErrorObj
, lookupJsonFields
, json2Str
, json2Int
, json2Double
, json2Float
, json2Bool
) where

import Data.Map(Map)
import Data.Map as Map
import Data.List as List
import Data.Either as Either
import GHC.Float as Float
import Data.Data
import Json.Types

import Data.Char(Char)

data JsonConversionErrLog = ErrorCollection String [JsonConversionError] | WrongType String | MissingKey String String | AbsentKeySet String [String] deriving Eq
type JsonConversionError = (Json, JsonConversionErrLog)
type JsonConversionReturn a = Either JsonConversionError a

instance Show JsonConversionErrLog where
  show (WrongType t)            = "ERROR: expected a" ++ t
  show (MissingKey t k)         = "ERROR: missing key " ++ k ++ " in " ++ t
  show (AbsentKeySet t k)       = "ERROR: in " ++ t ++ " missing keys " ++ (show k)
  -- TODO: Fix padding?
  show (ErrorCollection t errs) = "ERROR: in " ++ t ++ ":\n\t" ++ (List.concat $ List.intersperse "\n\t" $ List.map show errs)

class JsonConvertible a where
  fromJson :: Json -> JsonConversionReturn a
  toJson :: a -> Json

jsonConversionError :: Json -> JsonConversionErrLog -> JsonConversionReturn a
jsonConversionError json log = Left $ (json, log)

jsonConversionErrorObj :: Map String Json -> JsonConversionErrLog -> JsonConversionReturn a
jsonConversionErrorObj map log = Left $ (JsonObject map, log)

instance {-# OVERLAPPING #-} JsonConvertible String where
  fromJson (JsonStr str)               = Right str
  fromJson (JsonNum (JsonFloat value)) = Right $ show value
  fromJson (JsonNum (JsonInt value))   = Right $ show value
  fromJson (JsonBool value)            = Right $ show value
  fromJson json = jsonConversionError json (WrongType "string")
  toJson str = JsonStr str

instance JsonConvertible Int where
  fromJson (JsonNum (JsonInt value)) = Right value
  fromJson json = jsonConversionError json (WrongType "int")
  toJson value = JsonNum $ JsonInt value

instance JsonConvertible Double where
  fromJson (JsonNum (JsonFloat value)) = Right value
  fromJson (JsonNum (JsonInt value))   = Right $ Float.int2Double value
  fromJson json = jsonConversionError json (WrongType "float")
  toJson value = JsonNum $ JsonFloat value

instance JsonConvertible Float where
  fromJson (JsonNum (JsonFloat value)) = Right $ Float.double2Float value
  fromJson (JsonNum (JsonInt value))   = Right $ Float.int2Float value
  fromJson json = jsonConversionError json (WrongType "float")
  toJson value = JsonNum $ JsonFloat $ Float.float2Double value

instance JsonConvertible Bool where
  fromJson (JsonBool value) = Right value
  fromJson json = jsonConversionError json (WrongType "boolean")
  toJson value = JsonBool value

instance {-# OVERLAPPABLE #-} (JsonConvertible a) => JsonConvertible [a] where
  fromJson (JsonArray values) =
    let eitherValues = List.map fromJson values in
    case Either.partitionEithers eitherValues of
      ([], ret)   -> Right ret
      (err:xs, _) -> Left err
  fromJson json = jsonConversionError json (WrongType "list")
  toJson values = JsonArray $ List.map toJson values

instance (k ~ [Char], JsonConvertible v) => JsonConvertible (Map k v) where
  fromJson (JsonObject value) = case Map.mapEither fromJson value of
    (error, ret) | Map.null error -> Right ret
    (error, _)                        -> jsonConversionError (JsonObject value) (ErrorCollection "map" $ Map.elems error)
  fromJson json = jsonConversionError json (WrongType "map")
  toJson value = JsonObject $ Map.map toJson value

lookupJsonFields :: Map String a -> [String] -> Maybe [a]
lookupJsonFields map keys = sequence $ List.map (flip Map.lookup $ map) keys

json2Str :: Json -> JsonConversionReturn String
json2Str = fromJson :: Json -> JsonConversionReturn String

json2Int :: Json -> JsonConversionReturn Int
json2Int = fromJson :: Json -> JsonConversionReturn Int

json2Double :: Json -> JsonConversionReturn Double
json2Double = fromJson :: Json -> JsonConversionReturn Double

json2Float :: Json -> JsonConversionReturn Float
json2Float = fromJson :: Json -> JsonConversionReturn Float

json2Bool :: Json -> JsonConversionReturn Bool
json2Bool = fromJson :: Json -> JsonConversionReturn Bool

-- TODO: Add implem for polymorphic types List and Map
