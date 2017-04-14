{-# OPTIONS -Wall -fwarn-tabs -fno-warn-type-defaults  #-}

-- The basic definition of the parsing library as developed in lecture.
-- Operations for building composite parsers are in the module
-- ParserCombinators.

module Parser (Parser,
                   get,
                   choose,
                   eof,
                   ensure,
                   doParse,
                   ) where

import Control.Applicative

newtype Parser a = P (String -> [(a, String)])

doParse :: Parser a -> String -> [(a, String)]
doParse (P p) s = p s

-- | Return the next character from the input
get :: Parser Char
get = P (\cs -> case cs of
                (x:xs) -> [ (x,xs) ]
                []     -> [])

-- | This parser *only* succeeds at the end of the input.
eof :: Parser ()
eof = P $ \cs -> case cs of
                  []  -> [((),[])]
                  _:_ -> []

-- | Filter the parsing results by a predicate
ensure :: (a -> Bool) -> Parser a -> Parser a
ensure f p = P $ \s -> [ (x,s') | (x,s') <- doParse p s, f x ]


-- | Combine two parsers together in parallel, producing all
-- possible results from either parser.
choose :: Parser a -> Parser a -> Parser a
p1 `choose` p2 = P (\cs -> doParse p1 cs ++ doParse p2 cs)


instance Functor Parser where
   fmap = liftA

instance Applicative Parser where
   pure x    = P (\cs -> [ (x,cs) ])
   p1 <*> p2 = P (\cs -> do (f, cs')  <- doParse p1 cs
                            (x, cs'') <- doParse p2 cs'
                            return (f x, cs''))

instance Alternative Parser where
  -- the parser that always fails
  empty     = P $ \ _ -> []
  -- | Combine two parsers together in parallel, but only use the
  -- first result. This means that the second parser is used only
  -- if the first parser completely fails.
  p1 <|> p2 = P $ \cs -> case doParse (p1 `choose` p2) cs of
                          []   -> []
                          x:_ -> [x]
