{-# LANGUAGE OverloadedStrings #-}
module Main where

import Flatter

import Options.Applicative

import qualified Data.Yaml as Y
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString as BL

formatFlatEntry :: (Int, (Path, AtomicValue)) -> String
formatFlatEntry (i, (p, a)) = concat $ L.intersperse "\t" [show i, pathToString (Root:p), atomicToString a]

zipWithConst :: a -> [b] -> [(a, b)]
zipWithConst a bs = [(a, b) | b <- bs]

flattenMain :: IO ()
flattenMain = do
  s <- BL.getContents
  docs <- Y.decodeAllThrow s
  putStr $ unlines $ map formatFlatEntry $ concat $ zipWith zipWithConst [0..] (map flatten docs)

scratchMain :: IO ()
scratchMain = do
   results <- return $ unflatten [
       (0, [Key "foo"], String "one")
     , (0, [Key "bar"], String "two")
     , (0, [Key "quux", Key "k"], String "jo")
     , (0, [Key "quux", Key "v"], String "x")
     , (1, [Key "env"], String "lol")
     ]
   sequence $ map f results
   return ()
     where
       f result = case result of
         Left err -> putStrLn $ show $ err
         Right x -> do
           putStrLn "---"
           BL.putStr $ Y.encode x

data Options = Options
  { optReverse :: Bool
  }

argparser = info opts (progDesc "Tool to convert YAML-ish data to TSV and back." <> fullDesc)
  where
    opts = Options <$>
      switch (long "reverse" <>
              short 'r' <>
              help "Un-flatten instead of flatten.") <*
      abortOption ShowHelpText (long "help" <>
                                help "Display this message.")
main :: IO ()
main = do
  args <- execParser argparser
  case (optReverse args) of
    True -> scratchMain
    False -> flattenMain
