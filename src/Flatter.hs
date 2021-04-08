module Flatter
    ( flatten
    , pathToString
    , atomicToString
    , Path(..)
    , PathComponent(..)
    , AtomicValue(..)
    ) where

import Data.Either

import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString as BL
import qualified Data.Aeson as AE
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as SN

data PathComponent =
    Root
  | Key T.Text
  | Index Int
  | OmittedIndex
    deriving (Show, Eq)

type Path = [PathComponent]

data AtomicValue =
    String T.Text
  | Floating Double
  | Integer Integer
  | Bool Bool
  | Null
    deriving (Show, Eq)

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

atomicToString :: AtomicValue -> String
atomicToString (String t) = LUTF8.toString $ AE.encode (AE.String t)
atomicToString Null = "null"
atomicToString (Integer i) = show i
atomicToString (Floating x) = show x
atomicToString (Bool True) = "true"
atomicToString (Bool False) = "false"

flatten :: AE.Value -> [(Path, AtomicValue)]
flatten val = case val of
    AE.Object m -> concat $ [g (Key k) v | (k, v) <- HM.toList m]
    AE.Array xs -> concat $ zipWith g (map Index [0..]) (V.toList xs)
    AE.String s -> [([], String s)]
    AE.Null -> [([], Null)]
    AE.Bool v -> [([], Bool v)]
    AE.Number n -> [([], scientificToAtomic n)]
  where
    g :: PathComponent -> AE.Value -> [(Path, AtomicValue)]
    g pc v = [(pc : p, v) | (p, v) <- flatten v]

    scientificToAtomic n = case SN.floatingOrInteger n of
      Left x -> Floating x
      Right m -> Integer m
