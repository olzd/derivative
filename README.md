### Description

A simple (and inefficient) implementation of a regular expression matcher using Brzozowski's derivative. The theory is explained in the article [Parsing with derivatives - a Functional Pearl](http://matt.might.net/papers/might2011derivatives.pdf) which actually extends this concept to context-free grammars.

### Example

```haskell
-- a language to describe floats
float :: Lang
float = prefix <+> many digit
               <+> char '.'
               <+> digit
               <+> many digit
  where
    prefix = null <|> oneOf "+-"
    digit  = oneOf ['0'..'9']

-- prints [True,False,False,True,False]
main :: IO ()
main = print $ map (\w -> w `match` float) ["-1.337", "", "12", "2.2", "+1"]
```
