module Json.JsonParser
( parse
, int
, float
, number
, boolean
, str
, array
, object
) where

import Data.Char as Char
import Text.ParserCombinators.ReadP
import Control.Applicative hiding (many, optional)
import Json.JsonTypes
import Json.JsonChars
import Data.List as List
import Data.Map as Map

-- Basic matches
anyChar :: ReadP Char
anyChar = satisfy (\_ -> True)

digits :: ReadP String
digits = munch1 Char.isDigit

whiteSpace :: ReadP ()
whiteSpace = munch Char.isSpace *> (pure ())

-- Atomic types

-- number
sign :: (Num a) => ReadP (a -> a)
sign = minus *> (return (\x -> -x)) <|> return (\x -> x)

int :: ReadP JNum
int = JsonInt <$> (sign <*> (read <$> digits))

float :: ReadP JNum
float = do
  signFlip <- sign
  base <- digits <* dot
  decimal <- digits <++ return "0"
  return $ (JsonFloat . signFlip . read . List.concat) [base, ".", decimal]

number :: ReadP Json
number = fmap JsonNum (float <++ int)


-- boolean
true :: ReadP Json
true =
  let matcher = (string "True") <|> (string "true") in
  let cont = pure (JsonBool True) in
  matcher *> cont

false :: ReadP Json
false =
  let matcher = (string "False") <|> (string "false") in
  let cont = pure (JsonBool False) in
  matcher *> cont

boolean :: ReadP Json
boolean = true <|> false


-- string
notQuote :: ReadP String
notQuote = munch1 (\char -> char /= '\\' && char /= '"')

escChar :: ReadP String
escChar =
  let escQuote = fmap (\c -> [c]) $ char '\\' *> char '"' in
  let escChar  = fmap (\c -> ['\\', c]) $ satisfy (\c -> c /= '"') *> anyChar in
  escQuote <++ escChar

strPattern :: ReadP String
strPattern =
  let content = fmap List.concat $ many (notQuote <++ escChar) in
  dQuote *> content <* dQuote

str :: ReadP Json
str = fmap JsonStr strPattern


-- All Atomics
spaced :: ReadP a -> ReadP a
spaced p = whiteSpace *> p <* whiteSpace


anyJson :: ReadP Json
anyJson = object <|> array <|> str <|> boolean <|> number

spacedAny :: ReadP Json
spacedAny = spaced anyJson


-- Array
array :: ReadP Json
array =
  let matcher = oBracket *> (sepBy spacedAny coma) <* (optional $ spaced coma) <* cBracket in
  fmap JsonArray matcher


-- Object
objectField :: ReadP (String, Json)
objectField = do
  tag <- (spaced strPattern) <* colon
  value <- spacedAny
  return (tag, value)

object :: ReadP Json
object = do
  fields <- oBrace *> (sepBy objectField coma) <* (optional $ spaced coma) <* cBrace
  return $ JsonObject (Map.fromList fields)

-- parse
parseOptions :: String -> [(Json, String)]
parseOptions = readP_to_S object

parse :: String -> Maybe Json
parse string = case (parseOptions string) of
  ((json, _): _) -> Just json
  _ -> Nothing
