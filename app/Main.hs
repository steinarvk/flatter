{-# LANGUAGE OverloadedStrings #-}
module Main where

import Flatter
import Path

import Data.Either
import Options.Applicative
import Text.Parsec
import Data.Maybe

import qualified Data.Yaml as Y
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString as BL
import qualified Data.ByteString.UTF8 as UTF8

flattenMain :: IO ()
flattenMain = do
  s <- BL.getContents
  docs <- Y.decodeAllThrow s
  putStr $ unlines $ map formatFlattened $ flatten docs

unflattenMain :: IO ()
unflattenMain = do
   s <- getContents
   --- XXX: note "rights" and "catMaybes" swallow errors
   sequence $ map f $ unflatten $ catMaybes $ rights $ map parseFlattened (lines s)
   return ()
     where
       f result = case result of
         Left err -> putStrLn $ show $ err
         Right x -> do
           putStrLn "---"
           BL.putStr $ Y.encode x

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
    True -> unflattenMain
    False -> flattenMain
