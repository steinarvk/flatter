{-# LANGUAGE OverloadedStrings #-}

module Path
    ( PathComponent(..)
    , Path(..)
    , PathGroup(..)
    , groupPaths
    , enroot
    , unroot
    , rootPath
    , pathComponentString
    , pathToString
    , pathParser
    ) where

import Data.Maybe
import Text.ParserCombinators.Parsec

import qualified Data.Text as T
import qualified Text.ParserCombinators.Parsec as P
import qualified Text.Parsec.Number as PN

rootPath :: Path
rootPath = [Root]

data PathComponent =
    Root
  | Key T.Text
  | Index Int
    deriving (Show, Eq, Ord)

data GroupError = GroupError String
    deriving (Show, Eq, Ord)

data PathGroup a =
    RootGroup a
  | ObjectGroup [(T.Text, Path, a)]
  | ArrayGroup [(Int, Path, a)]

isRoot :: Path -> Bool
isRoot [Root] = True
isRoot [] = True
isRoot _ = False

entuple21 :: (a, b) -> c -> (a, b, c)
entuple21 (x, y) z = (x, y, z)

splitAsObjectGroup :: Path -> Maybe (T.Text, Path)
splitAsObjectGroup (Key k:p) = Just (k, p)
splitAsObjectGroup _ = Nothing

splitAsArrayGroup :: Path -> Maybe (Int, Path)
splitAsArrayGroup (Index i:p) = Just (i, p)
splitAsArrayGroup _ = Nothing

mapFst f xys = [(f x, y) | (x,y) <- xys]

groupPaths :: [(Path, a)] -> Either GroupError (PathGroup a)
groupPaths xs = groupPaths' $ mapFst unroot xs

groupPaths' :: [(Path, a)] -> Either GroupError (PathGroup a)
groupPaths' [] = Left $ GroupError "nothing to group"
groupPaths' ((p, x):xs) | isRoot p =
    if (null xs)
    then Right $ RootGroup x
    else Left $ GroupError "path collision (root with other)"
groupPaths' xs@((p, x):_) | isJust (splitAsObjectGroup p) =
    if (length objs) == (length xs)
    then Right $ ObjectGroup $ zipped
    else Left $ GroupError "inconsistent paths (object with other)"
  where
    zipped = zipWith entuple21 objs (map snd xs)
    objs = catMaybes $ map (splitAsObjectGroup . fst) xs
groupPaths' xs@((p, x):_) | isJust (splitAsArrayGroup p) =
    if (length objs) == (length xs)
    then Right $ ArrayGroup $ zipped
    else Left $ GroupError "inconsistent paths (array with other)"
  where
    zipped = zipWith entuple21 objs (map snd xs)
    objs = catMaybes $ map (splitAsArrayGroup . fst) xs

type Path = [PathComponent]

unroot :: Path -> Path
unroot (Root:p) = p
unroot p = p

enroot :: Path -> Path
enroot x@(Root:p) = x
enroot p = Root:p

pathComponentString :: PathComponent -> String
pathComponentString (Index i) = "[" ++ show i ++ "]"
pathComponentString (Key k) = T.unpack k
pathComponentString Root = "$"

pathToString :: Path -> String
pathToString [] = ""
pathToString (x:[]) = pathComponentString x
pathToString (x:xs@(Index _:_)) = pathComponentString x ++ pathToString xs
pathToString (x:xs) = pathComponentString x ++ "." ++ pathToString xs


pRoot :: P.Parser PathComponent
pRoot = P.string "$" >> return Root

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
                    <|> try pIndex
                    <|> try pKey
