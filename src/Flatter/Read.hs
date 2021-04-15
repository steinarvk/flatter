{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveDataTypeable #-}
module Flatter.Read
    ( readRecordsYAML
    , readRecordJSON
    , readRecordsJSONLines
    ) where

import Data.Typeable
import Control.Exception
import Control.Monad (liftM, join)
import qualified Data.Text as T
import Data.Maybe (catMaybes)

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.Yaml as Y
import qualified Data.Aeson as AE

data ParseException = ParseException String
  deriving (Show, Typeable)

instance Exception ParseException

readRecordsYAML :: String -> IO [AE.Value]
readRecordsYAML s = do
  rv <- Y.decodeAllThrow $ UTF8.fromString s
  return rv

readRecordJSON :: String -> IO [AE.Value]
readRecordJSON s = do
  case (AE.decode $ LUTF8.fromString s) of
    Nothing -> throwIO $ ParseException "bad JSON data"
    Just rv -> return [rv]

readRecordsJSONLines :: String -> IO [AE.Value]
readRecordsJSONLines s = do
  h
    where
      g :: IO [Maybe AE.Value]
      g = sequence $ map (f . T.unpack . T.strip . T.pack) $ lines s

      h :: IO [AE.Value]
      h = liftM catMaybes g

      f :: String -> IO (Maybe AE.Value)
      f x = if (null x)
            then return Nothing
            else case (AE.decode $ LUTF8.fromString x) of
              Nothing -> throwIO $ ParseException $ "bad JSON data: " ++ x
              Just rv -> return $ Just rv
