{-# LANGUAGE OverloadedStrings #-}

import Flatter

import Test.Hspec
import qualified Data.Aeson as AE

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
  describe "flatten" $
    do it "string" $ shouldBe (flatten $ AE.Bool True) [([], Bool True)]
