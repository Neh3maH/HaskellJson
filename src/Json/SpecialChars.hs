module Json.SpecialChars
( dQuote
, oBracket
, cBracket
, oBrace
, cBrace
, colon
, coma
, dot
, minus
) where

import Text.ParserCombinators.ReadP

dQuote :: ReadP Char
dQuote = char '"'

oBrace :: ReadP Char
oBrace = char '{'

cBrace :: ReadP Char
cBrace = char '}'

oBracket :: ReadP Char
oBracket = char '['

cBracket :: ReadP Char
cBracket = char ']'

colon :: ReadP Char
colon = char ':'

coma :: ReadP Char
coma = char ','

-- floating point numbers
dot :: ReadP Char
dot = char '.'

minus :: ReadP Char
minus = char '-'
