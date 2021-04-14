{-# LANGUAGE OverloadedStrings #-}
module Main where

import Flatter
import Path

import Control.Exception
import Data.Either
import Options.Applicative
import Text.Parsec
import Data.Maybe

import qualified Data.Yaml as Y
import qualified Data.List as L
import qualified Data.Text as T
import qualified Data.ByteString as BL
import qualified Data.ByteString.UTF8 as UTF8

data ParseException = ParseException String
  deriving (Show)

instance Exception ParseException

flattenMain :: IO ()
flattenMain = do
  s <- BL.getContents
  docs <- Y.decodeAllThrow s
  putStr $ unlines $ map formatFlattened $ flatten docs

partitionEithersTerminating :: [Either c a] -> (Maybe c, [a])
partitionEithersTerminating [] = (Nothing, [])
partitionEithersTerminating (Left err : xs) = (Just err, [])
partitionEithersTerminating (Right x' : xs) =
    let (me, xs') = partitionEithersTerminating xs
    in (me, x' : xs')

showResultOrDie result = case result of
       Left err -> throwIO (ParseException $ show err)
       Right x -> do
         putStrLn "---"
         BL.putStr $ Y.encode x

unflattenMain :: IO ()
unflattenMain = do
   s <- getContents
   (maybeErr, flats) <- return $ partitionEithersTerminating $ parseManyFlattened (lines s)
   sequence $ map showResultOrDie $ unflatten flats
   case maybeErr of
     Nothing -> return ()
     Just err -> throwIO (ParseException $ show err)
       where

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
