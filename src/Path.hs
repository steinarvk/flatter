module Path where

import qualified Data.Text as T

import Text.ParserCombinators.Parsec

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
pathToString (x:xs) = pathComponentString x ++ "." ++ pathToString xs


-- root :: Parser 

-- pathComponent = root <|> index <|> omittedIndex <|> key 
