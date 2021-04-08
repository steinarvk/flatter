module Main where

import Flatter

import qualified Data.Yaml as Y
import qualified Data.List as L
import qualified Data.ByteString as BL

formatFlatEntry :: (Int, (Path, AtomicValue)) -> String
formatFlatEntry (i, (p, a)) = concat $ L.intersperse "\t" [show i, pathToString (Root:p), atomicToString a]

zipWithConst :: a -> [b] -> [(a, b)]
zipWithConst a bs = [(a, b) | b <- bs]

flattenMain = do
  s <- BL.getContents
  docs <- Y.decodeAllThrow s
  putStr $ unlines $ map formatFlatEntry $ concat $ zipWith zipWithConst [0..] (map flatten docs)

main :: IO ()
main = flattenMain
