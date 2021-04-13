module Flatter
    ( flatten
    , unflatten
    , parseFlattened
    , formatFlattened
    , Path(..)
    , PathComponent(..)
    , AtomicValue(..)
    ) where

import Path
    ( Path
    , PathComponent(..)
    , pathToString
    , pathParser
    )

import Data.Either
import Data.List.Extra (groupSort)
import Data.List.Split (splitOn)
import Data.Sort (sortOn)
import Text.Read (readMaybe)

import qualified Data.ByteString.Lazy.UTF8 as LUTF8
import qualified Data.ByteString as BL
import qualified Data.Aeson as AE
import qualified Data.Yaml as Y
import qualified Data.Text as T
import qualified Data.List as L
import qualified Data.Vector as V
import qualified Data.HashMap.Strict as HM
import qualified Data.Scientific as SN
import qualified Text.Parsec as P

data AtomicValue =
    String T.Text
  | Floating Double
  | Integer Integer
  | Bool Bool
  | Null
    deriving (Show, Eq)

data Flattened = Flattened Integer Path AtomicValue deriving (Show, Eq)

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
toValue (Integer x) = AE.Number $ SN.scientific x 0
-- TODO numbers


fromValue :: AE.Value -> Either ParseError AtomicValue
fromValue (AE.String s) = Right $ String s
fromValue AE.Null = Right $ Null
fromValue (AE.Bool v) = Right $ Bool v
fromValue (AE.Number x) = case SN.floatingOrInteger x of
                            Left x -> Left $ ParseError "xxx"
                            Right n -> Right $ Integer n
-- TODO numbers

flattenOne :: AE.Value -> [(Path, AtomicValue)]
flattenOne val = case val of
    AE.Object m -> concat $ [g (Key k) v | (k, v) <- HM.toList m]
    AE.Array xs -> concat $ zipWith g (map Index [0..]) (V.toList xs)
    AE.String s -> [([], String s)]
    AE.Null -> [([], Null)]
    AE.Bool v -> [([], Bool v)]
    AE.Number n -> [([], scientificToAtomic n)]
  where
    g :: PathComponent -> AE.Value -> [(Path, AtomicValue)]
    g pc v = [(pc : p, v) | (p, v) <- flattenOne v]

    scientificToAtomic n = case SN.floatingOrInteger n of
      Left x -> Floating x
      Right m -> Integer m

formatFlattened :: Flattened -> String
formatFlattened (Flattened i p a) = concat $ L.intersperse "\t" [show i, pathToString (Root:p), atomicToString a]

zipWithConst :: a -> [b] -> [(a, b)]
zipWithConst a bs = [(a, b) | b <- bs]

addIndex :: Integer -> [(Path, AtomicValue)] -> [Flattened]
addIndex i [] = []
addIndex i ((p, a):xs) = (Flattened i p a) : addIndex i xs

flatten :: [AE.Value] -> [Flattened]
flatten xs = concat $ zipWith addIndex [1..] $ map flattenOne xs

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

arrayIndexValues :: [(PathComponent, AE.Value)] -> [(Int, AE.Value)]
arrayIndexValues [] = []
arrayIndexValues ((Index i, v):xs) = (i, v) : arrayIndexValues xs
arrayIndexValues ((OmittedIndex, v):xs) = (0, v) : arrayIndexValues xs
arrayIndexValues (_:xs) = arrayIndexValues xs

mapFst f xys = [(f x, y) | (x,y) <- xys]
mapSnd f xys = [(x, f y) | (x,y) <- xys]

unroot :: Path -> Path
unroot (Root:p) = p
unroot p = p

data Semiflat = SemiflatObject [(String, Semiflat)]
              | SemiflatArray [(Int, Semiflat)]
              | SemiflatValue AtomicValue

unindexStream :: (Num a, Eq a) => [(a, b)] -> [[b]]
unindexStream [] = []
unindexStream ((i, x):xs) = map reverse $ f i [x] xs
  where
    f :: (Num a, Eq a) => a -> [b] -> [(a, b)] -> [[b]]
    f _ acc [] = [acc]
    f i acc ((j, x):xs) | i == j = f i (x:acc) xs
    f i acc w@((j, x):xs) = acc : f (i+1) [] w


unflattenOne :: [(Path, AtomicValue)] -> Either ParseError AE.Value
unflattenOne xs = unflattenOne' unrooted
  where
    unrooted = mapFst unroot xs


unflattenOne' :: [(Path, AtomicValue)] -> Either ParseError AE.Value
unflattenOne' items =
    case errors of
      (err:_) -> Left err
      [] -> case valuesHere of
        [x] -> Right $ toValue x
        [] -> case objkeys of
          [] -> Right $ AE.Array $ V.fromList $ map snd $ sortOn fst arrkeys
          objkeys -> Right $ AE.Object $ HM.fromList $ sortOn fst objkeys
  where
    unrolled = mapSnd unflattenOne $ unrollPathStep items 
    errors = lefts $ map snd unrolled
    okays = zip (map fst unrolled) $ rights $ map snd unrolled
    objkeys = objectKeyValues okays
    arrkeys = arrayIndexValues okays
    valuesHere = unrollHere items

chunkById :: (Eq a) => [(a, b)] -> [[b]]
chunkById xs = f xs Nothing
  where
    f :: (Eq a) => [(a, b)] -> Maybe (a, [b]) -> [[b]]
    f [] (Just (_, acc)) = [acc]
    f [] Nothing = []
    f ((i, x):xs) (Just (j, acc)) | i == j = f xs (Just (j, x:acc))
    f ((i, x):xs) (Just (j, acc)) = acc : (f xs (Just (i, [x])))
    f ((i, x):xs) _ = f xs (Just (i, [x]))

data ParseError = ParseError String deriving (Show, Eq)

unflatten :: [Flattened] -> [Either ParseError AE.Value]
unflatten items = map unflattenOne (chunkById tupled) 
  where
    tupled = [(i, (p, a)) | Flattened i p a <- items ]

parseFlattened :: String -> Either ParseError (Maybe Flattened)
parseFlattened s =
    case columns of
      [] -> Right Nothing
      [i, p, a] -> do
         case (readMaybe i :: Maybe Integer) of
           Nothing -> Left $ ParseError ("bad integer: " ++ i)
           Just idx -> do
             case (AE.decode (LUTF8.fromString a) :: Maybe AE.Value) of
               Nothing -> Left $ ParseError ("bad atomic JSON value: " ++ a)
               Just fj -> case (fromValue fj) of
                 Left pe -> Left pe
                 Right av -> do
                   case P.runParser pathParser () "input" p of
                     Left err -> Left $ ParseError ("bad path: " ++ p ++ ", " ++ (show err))
                     Right pp -> Right $ Just $ Flattened idx pp av
      _ -> Left $ ParseError "wrong number of columns"
  where
    columns = splitOn "\t" s
