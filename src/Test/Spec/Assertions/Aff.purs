module Test.Spec.Assertions.Aff
    ( expectError
    , expectErrorProp
    ) where

import Prelude

import Control.Monad.Aff           (Aff, attempt)
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Eff.Console   (CONSOLE)
import Control.Monad.Eff.Exception (EXCEPTION, error)
import Control.Monad.Eff.Random    (RANDOM)
import Control.Monad.Error.Class   (throwError)
import Data.Either                 (Either(..))
import Test.QuickCheck             (class Testable, quickCheck)

expectError :: forall r t. Aff r t -> Aff r Unit
expectError a = do
  e <- attempt a
  case e of
    Left _ -> pure unit
    Right _ -> throwError $ error "Expected error"

expectErrorProp :: forall eff prop
                 . (Testable prop)
                => prop
                -> Aff (console :: CONSOLE, random :: RANDOM, err :: EXCEPTION | eff) Unit
expectErrorProp = expectError <<< liftEff <<< quickCheck
