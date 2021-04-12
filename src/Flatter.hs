module Flatter
    ( flatten
    , unflatten
    , pathToString
    , atomicToString
    , Path(..)
    , PathComponent(..)
    , AtomicValue(..)
    ) where

import Path
    ( Path
    , PathComponent(..)
    , pathToString
    )

import Data.Either
import Data.List.Extra (groupSort)

import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString as BL
import qualified Data.Aeson as AE
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as SN

data AtomicValue =
    String T.Text
  | Floating Double
  | Integer Integer
  | Bool Bool
  | Null
    deriving (Show, Eq)

atomicToString :: AtomicValue -> String
atomicToString (String t) = LUTF8.toString $ AE.encode (AE.String t)
atomicToString Null = "null"
atomicToString (Integer i) = show i
atomicToString (Floating x) = show x
atomicToString (Bool True) = "true"
atomicToString (Bool False) = "false"

toValue :: AtomicValue -> AE.Value
toValue (String s) = AE.String s
toValue Null = AE.Null
toValue (Bool True) = AE.Bool True
toValue (Bool False) = AE.Bool False
-- TODO

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

unrollHere :: [(Path, AtomicValue)] -> [AtomicValue]
unrollHere = map snd . filter (\(p, _) -> p == [])

unrollPathStep :: [(Path, AtomicValue)] -> [(PathComponent, [(Path, AtomicValue)])]
unrollPathStep xs = mapSnd concat $ groupSort $ unrollPathStep' xs
  where
    unrollPathStep' [] = []
    unrollPathStep' (([], _):xs) = unrollPathStep xs
    unrollPathStep' (x:xs) = g x : unrollPathStep xs
    g ((pc:p), v) = (pc, [(p, v)])

objectKeyValues :: [(PathComponent, AE.Value)] -> [(T.Text, AE.Value)]
objectKeyValues [] = []
objectKeyValues ((Key k, v):xs) = (k, v) : objectKeyValues xs
objectKeyValues (_:xs) = objectKeyValues xs

mapSnd f xys = [(x, f y) | (x,y) <- xys]

unflattenOne :: [(Path, AtomicValue)] -> Either ParseError AE.Value
unflattenOne items =
    case errors of
      (err:_) -> Left err
      [] -> case valuesHere of
        [x] -> Right $ toValue x
        [] -> Right $ AE.Object $ HM.fromList objkeys -- XXX TODO
  where
    unrolled = mapSnd unflattenOne $ unrollPathStep items 
    errors = lefts $ map snd unrolled
    okays = zip (map fst unrolled) $ rights $ map snd unrolled
    objkeys = objectKeyValues okays
    valuesHere = unrollHere items

byEntityId :: (Eq a) => [(a, b)] -> [[b]]
byEntityId xs = f xs Nothing
  where
    f :: (Eq a) => [(a, b)] -> Maybe (a, [b]) -> [[b]]
    f [] (Just (_, acc)) = [acc]
    f [] Nothing = []
    f ((i, x):xs) (Just (j, acc)) | i == j = f xs (Just (j, x:acc))
    f ((i, x):xs) (Just (j, acc)) = acc : (f xs (Just (i, [x])))
    f ((i, x):xs) _ = f xs (Just (i, [x]))

unflatten :: [(Integer, Path, AtomicValue)] -> [Either ParseError AE.Value]
unflatten items = map unflattenOne (byEntityId tupled) 
  where
    tupled = [(i, (p, a)) | (i, p, a) <- items ]

data ParseError = ParseError String deriving (Show, Eq)

