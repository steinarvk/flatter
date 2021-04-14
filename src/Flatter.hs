module Flatter
    ( flatten
    , unflatten
    , Flattened(..)
    , flattenOne
    , parseFlattened
    , parseManyFlattened
    , formatFlattened
    , Path(..)
    , atomicToString
    , pathToString
    , PathComponent(..)
    , AtomicValue(..)
    ) where

import Path
    ( Path
    , PathComponent(..)
    , PathGroup(..)
    , pathToString
    , pathParser
    , groupPaths
    , rootPath
    , unroot
    , enroot
    )

import Data.Either
import Data.List.Extra (groupSort, groupSortOn)
import Data.Either.Extra (mapRight)
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
  | Floating SN.Scientific
  | Integer Integer
  | Bool Bool
  | EmptyArray
  | EmptyObject
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
atomicToString EmptyArray = "[]"
atomicToString EmptyObject = "{}"

toValue :: AtomicValue -> AE.Value
toValue (String s) = AE.String s
toValue Null = AE.Null
toValue (Bool True) = AE.Bool True
toValue (Bool False) = AE.Bool False
toValue (Integer x) = AE.Number $ SN.scientific x 0
toValue (Floating x) = AE.Number $ x
toValue EmptyArray = AE.Array $ V.fromList []
toValue EmptyObject = AE.Object $ HM.fromList []

fromValue :: AE.Value -> Either ParseError AtomicValue
fromValue (AE.String s) = Right $ String s
fromValue AE.Null = Right $ Null
fromValue (AE.Bool v) = Right $ Bool v
fromValue (AE.Number x) = case SN.floatingOrInteger x of
                            Left _ -> Right $ Floating x
                            Right n -> Right $ Integer n
fromValue (AE.Object x) | HM.null x = Right $ EmptyObject
fromValue (AE.Array x) | V.null x = Right $ EmptyArray

flattenOne :: AE.Value -> [(Path, AtomicValue)]
flattenOne val = case val of
    AE.Object m -> if HM.null m
                   then [(rootPath, EmptyObject)]
                   else concat $ [g (Key k) v | (k, v) <- HM.toList m]
    AE.Array xs -> if V.null xs
                   then [(rootPath, EmptyArray)]
                   else concat $ zipWith g (map Index [0..]) (V.toList xs)
    AE.String s -> [([], String s)]
    AE.Null -> [([], Null)]
    AE.Bool v -> [([], Bool v)]
    AE.Number n -> [([], scientificToAtomic n)]
  where
    g :: PathComponent -> AE.Value -> [(Path, AtomicValue)]
    g pc v = [(pc : p, v) | (p, v) <- flattenOne v]

    scientificToAtomic n = case SN.floatingOrInteger n of
      Left x -> Floating n
      Right m -> Integer m

formatFlattened :: Flattened -> String
formatFlattened (Flattened i p a) = concat $ L.intersperse "\t" [show i, pathToString (enroot p), atomicToString a]

zipWithConst :: a -> [b] -> [(a, b)]
zipWithConst a bs = [(a, b) | b <- bs]

addIndex :: Integer -> [(Path, AtomicValue)] -> [Flattened]
addIndex i [] = []
addIndex i ((p, a):xs) = (Flattened i p a) : addIndex i xs

flatten :: [AE.Value] -> [Flattened]
flatten xs = concat $ zipWith addIndex [1..] $ map flattenOne xs

mapFst f xys = [(f x, y) | (x,y) <- xys]
mapSnd f xys = [(x, f y) | (x,y) <- xys]

dropFst3 (x, y, z) = (y, z)
fst3 (x, y, z) = x

rejigger (x, y, z) = (x, (y, z))

unwrapEither :: [Either a b] -> Either a [b]
unwrapEither (Left err : _) = Left err
unwrapEither (Right x : xs) =
    case unwrapEither xs of
      Left err -> Left err
      Right xs' -> Right $ x : xs'
unwrapEither [] = Right []

unflattenOne :: [(Path, AtomicValue)] -> Either ParseError AE.Value
unflattenOne xs = f (mapSnd toValue xs)
  where
    objectify :: [(T.Text, Path, AE.Value)] -> Either ParseError AE.Value
    objectify xs =
        case unwrapEither (map snd pairs) of
          Left err -> Left err
          Right values -> Right $ AE.Object $ HM.fromList (zip (map fst pairs) values)
      where
        pairs = mapSnd f $ groupSort $ map rejigger xs

    arrayify :: [(Int, Path, AE.Value)] -> Either ParseError AE.Value
    arrayify xs =
        case unwrapEither (map snd pairs) of
          Left err -> Left err
          Right values -> Right $ AE.Array $ V.fromList values -- ? sort?
      where
        pairs = mapSnd f $ groupSort $ map rejigger xs

    f :: [(Path, AE.Value)] -> Either ParseError AE.Value
    f xs = if (null xs) then (Right AE.Null) else g xs

    g :: [(Path, AE.Value)] -> Either ParseError AE.Value
    g xs = case groupPaths xs of
        Left err -> Left $ ParseError $ show err
        Right pg -> case pg of
          RootGroup av -> Right $ av
          ObjectGroup kpas -> objectify kpas
          ArrayGroup ipas -> arrayify ipas

unindexStream :: (Num a, Eq a) => [(a, b)] -> [[b]]
unindexStream [] = []
unindexStream ((i, x):xs) = map reverse $ f i [x] xs
  where
    f :: (Num a, Eq a) => a -> [b] -> [(a, b)] -> [[b]]
    f _ acc [] = [acc]
    f i acc ((j, x):xs) | i == j = f i (x:acc) xs
    f i acc w@((j, x):xs) = acc : f (i+1) [] w

data ParseError = ParseError String deriving (Show, Eq)

unflatten :: [Flattened] -> [Either ParseError AE.Value]
unflatten items = map unflattenOne (unindexStream tupled) 
  where
    tupled = [(i, (p, a)) | Flattened i p a <- items ]

parseManyFlattened :: [String] -> [Either ParseError Flattened]
parseManyFlattened xs = filterRightNothing (map parseFlattened xs)
  where
    filterRightNothing [] = []
    filterRightNothing ((Right Nothing):xs) = filterRightNothing xs
    filterRightNothing ((Right (Just y)):xs) = (Right y) : filterRightNothing xs
    filterRightNothing ((Left err):xs) = (Left err) : filterRightNothing xs

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
