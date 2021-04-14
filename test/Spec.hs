{-# LANGUAGE OverloadedStrings #-}

import Flatter

import Test.Hspec
import Test.QuickCheck

import Rebase.Prelude hiding ((.))
import qualified Data.Aeson as AE
import qualified Data.HashMap.Strict as HM
import qualified Data.Text as T
import qualified Data.Vector as V

instance Arbitrary AE.Value where
  arbitrary = sized sizedArbitraryValue

sizedArbitraryValue :: Int -> Gen AE.Value
sizedArbitraryValue n
  | n <= 0 = oneof [pure AE.Null, bool, number, string]
  | otherwise = resize n' $ oneof [pure AE.Null, bool, number, string, array, object']
  where
    n' = n `div` 2
    bool = AE.Bool <$> arbitrary
    number = (AE.Number . fromRational . toRational :: Double -> AE.Value) <$> arbitrary
    string = (AE.String . T.pack) <$> arbitrary
    array = (AE.Array . V.fromList) <$> arbitrary
    object' = (AE.Object . HM.fromList . map (first T.pack)) <$> arbitrary

roundtrips :: AE.Value -> Bool
roundtrips x =
    case (unflatten . flatten) [x] of
      [(Right v)] -> v == x
      _ -> False

roundtripsMany :: [AE.Value] -> Bool
roundtripsMany xs =
    let results = (unflatten . flatten) xs
    in xs == (rights results)

main :: IO ()
main = hspec $ do
  describe "pathToString" $
    do it "empty" $ shouldBe (pathToString []) ""
       it "root" $ shouldBe (pathToString [Root]) "$"
       it "$.foo.bar" $ shouldBe (pathToString [Root, Key "foo", Key "bar"]) "$.foo.bar"
       it "$.foo[1]" $ shouldBe (pathToString [Root, Key "foo", Index 1]) "$.foo[1]"
       it "$[3].foo" $ shouldBe (pathToString [Root, Index 3, Key "foo"]) "$[3].foo"
       it "$.foo[1][2]" $ shouldBe (pathToString [Root, Key "foo", Index 1, Index 2]) "$.foo[1][2]"
  describe "atomicToString" $
    do it "string" $ shouldBe (atomicToString (String "hello")) "\"hello\""
       it "floating" $ shouldBe (atomicToString (Floating 2.5)) "2.5"
       it "integer" $ shouldBe (atomicToString (Integer 42)) "42"
       it "bool:true" $ shouldBe (atomicToString (Bool True)) "true"
       it "bool:false" $ shouldBe (atomicToString (Bool False)) "false"
       it "null" $ shouldBe (atomicToString Null) "null"
  describe "flattenOne" $
    do it "string" $ shouldBe (flattenOne $ AE.Bool True) [([], Bool True)]
  describe "flatten" $
    do it "round-trips with unflatten on single item" $ property $ roundtrips
       it "round-trips with unflatten on multiple items" $ property $ roundtripsMany
       it "works on infinite streams" $ shouldBe (listToMaybe $ flatten $ repeat $ AE.String "foo") (Just $ Flattened 1 [] (String "foo"))
  describe "unflatten" $
    do it "works on infinite streams" $ shouldBe (listToMaybe $ unflatten $ map (\n -> Flattened n [] (String "hello")) [1..]) (Just $ Right $ AE.String "hello")
