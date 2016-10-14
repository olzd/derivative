module Derivative ( Lang()
                  , empty
                  , null
                  , char
                  , cat
                  , alt
                  , many
                  , (<+>)
                  , (<|>)
                  , oneOf
                  , str
                  , match
                  , digit
                  , alpha
                  , alphanum
                  )
                  where

import Prelude hiding (null)

data Lang = Empty         -- the empty language
          | Null          -- the null language (contains only the empty string)
          | Char !Char    -- the language containing one character
          | Cat Lang Lang -- the concatenation of languages
          | Alt Lang Lang -- the union of languages
          | Rep Lang      -- the repetition (Kleene star) of a language
          deriving (Eq, Show)

-- Constructors

empty :: Lang
empty = Empty

null :: Lang
null = Null

char :: Char -> Lang
char = Char

cat :: Foldable t => t Lang -> Lang
cat = foldl1 (\acc x -> Cat acc x)

alt :: Foldable t => t Lang -> Lang
alt = foldl1 (\acc x -> Alt acc x)

many :: Lang -> Lang
many = Rep

(<+>) :: Lang -> Lang -> Lang
(<+>) = Cat

(<|>) :: Lang -> Lang -> Lang
(<|>) = Alt

oneOf :: String -> Lang
oneOf = alt . map char

str :: String -> Lang
str = cat . map char

-- Checks if a language contains the empty string
delta :: Lang -> Bool
delta Empty     = False
delta Null      = True
delta (Char _)  = False
delta (Rep _)   = True
delta (Cat l r) = delta l && delta r
delta (Alt l r) = delta l || delta r

-- Returns the derivative of a language with respect to a given character
derivative :: Char -> Lang -> Lang
derivative _ Empty     = Empty
derivative _ Null      = Empty
derivative c (Char c') = if c == c' then Null else Empty
derivative c (Alt l r) = alt [derivative c l, derivative c r]
derivative c (Rep l)   = cat [derivative c l, many l]
derivative c (Cat l r) = if (delta l) then
                            alt [cat [derivative c l, r], derivative c r]
                         else
                            cat [derivative c l, r]

-- Checks if a given string belongs to the language:
-- Derive the language with respect to each character of the string and check if
-- the final language contains the null string
match :: String -> Lang -> Bool
match [] lang = delta lang
match s lang  = match (tail s) (derivative (head s) lang)

-- Some language definitions

digit :: Lang
digit = oneOf ['0'..'9']

alpha :: Lang
alpha = oneOf ['a'..'z'] <|> oneOf ['A'..'Z']

alphanum :: Lang
alphanum = alpha <|> digit
