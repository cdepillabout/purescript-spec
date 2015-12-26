module Test.Spec.SpecSpec where

import Prelude (Unit, bind, (<), (+), ($), (>))

import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Test.Spec (Spec, it, prop, describe)
-- import Test.Spec.Assertions
import Test.Spec.Assertions.Aff (expectErrorProp)

specSpec :: forall eff. Spec (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | eff) Unit
specSpec =
  describe "Test" $
    describe "Spec" $ do
        -- it "collects \"it\" and \"pending\" in Describe groups" do
          -- results <- collect successTest
          -- results `shouldEqual` [Describe "a" [Describe "b" [It "works" Success]]]
        prop "quickcheck properties work" \int -> int + 1 > int

        it "expectErrorProp succeeds when properties fail" $
            expectErrorProp \int -> int + 1 < int
