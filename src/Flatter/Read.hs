module Flatter.Read
    ( readYAMLRecords
    ) where

import qualified Data.ByteString.UTF8 as UTF8
import qualified Data.Yaml as Y

readYAMLRecords :: String -> IO [Y.Value]
readYAMLRecords s = do
  rv <- Y.decodeAllThrow $ UTF8.fromString s
  return rv

