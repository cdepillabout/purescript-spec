module Test.Spec.RunnerSpec where

import Prelude

import Control.Monad.Aff (later')

import Test.Spec            (Group(..), Result(..), collect, await, describe, it)
import Test.Spec.Assertions (shouldEqual)

import Test.Spec.Fixtures (successTest, sharedDescribeTest)

runnerSpec =
  describe "Test" $
    describe "Spec" $
      describe "Runner" do
        it "collects \"it\" and \"pending\" in Describe groups" do
          results <- collect successTest >>= await
          results `shouldEqual` [Describe "a" [Describe "b" [It "works" Success]]]
        it "collects \"it\" and \"pending\" with shared Describes" do
          results <- collect sharedDescribeTest >>= await
          results `shouldEqual` [Describe "a" [Describe "b" [It "works" Success],
                                               Describe "c" [It "also works" Success]]]
        it "supports async" do
          res <- later' 10 $ return 1
          res `shouldEqual` 1
