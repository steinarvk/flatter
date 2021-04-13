{-# LANGUAGE OverloadedStrings #-}

module Path
    ( PathComponent(..)
    , Path(..)
    , pathComponentString
    , pathToString
    , pathParser
    ) where

import qualified Data.Text as T

import Text.ParserCombinators.Parsec

import qualified Text.ParserCombinators.Parsec as P
import qualified Text.Parsec.Number as PN

data PathComponent =
    Root
  | Key T.Text
  | Index Int
  | OmittedIndex
    deriving (Show, Eq, Ord)

type Path = [PathComponent]

pathComponentString :: PathComponent -> String
pathComponentString (Index i) = "[" ++ show i ++ "]"
pathComponentString OmittedIndex = "[]"
pathComponentString (Key k) = T.unpack k
pathComponentString Root = "$"

pathToString :: Path -> String
pathToString [] = ""
pathToString (x:[]) = pathComponentString x
pathToString (x:xs@(Index _:_)) = pathComponentString x ++ pathToString xs
pathToString (x:xs@(OmittedIndex:_)) = pathComponentString x ++ pathToString xs
pathToString (x:xs) = pathComponentString x ++ "." ++ pathToString xs


pRoot :: P.Parser PathComponent
pRoot = P.string "$" >> return Root

pOmittedIndex :: P.Parser PathComponent
pOmittedIndex = P.string "[]" >> return OmittedIndex

pKey :: P.Parser PathComponent
pKey = do
  s <- P.many1 (P.alphaNum <|> P.char '_')
  return $ Key (T.pack s)

pIndex :: P.Parser PathComponent
pIndex = do
  P.string "["
  i <- PN.int
  P.string "]"
  return $ Index i

dotted :: P.Parser Path
dotted = do
  x <- pathComponentParser
  P.string "."
  t <- pathParser
  P.eof
  return (x : t)

undotted :: P.Parser Path
undotted = do
  x <- pathComponentParser
  t <- pathParser
  P.eof
  return (x : t)

final :: P.Parser Path
final = do
  x <- pathComponentParser
  P.eof
  return [x]

pathParser :: P.Parser Path
pathParser = try dotted
         <|> try undotted
         <|> try final

pathComponentParser :: P.Parser PathComponent
pathComponentParser = try pRoot
                    <|> try pOmittedIndex
                    <|> try pIndex
                    <|> try pKey
