module Test.Spec.Runner (
  Process(..),
  run
  ) where

import Prelude

import Control.Monad             (when)
import Control.Monad.Aff         (runAff)
import Control.Monad.Eff         (Eff())
import Control.Monad.Eff.Console (CONSOLE(), print)
import Data.Foldable             (sequence_)

import Test.Spec          (Spec(), collect, await)
import Test.Spec.Console  (withAttrs)
import Test.Spec.Summary  (successful)
import Test.Spec.Reporter (Reporter())

foreign import data Process :: !

foreign import exit :: forall eff. Int -> Eff (process :: Process | eff) Unit

-- Runs the tests and invoke all reporters.
-- If run in a NodeJS environment any failed test will cause the
-- process to exit with a non-zero exit code.
run :: forall e r.
    Array (Reporter (process :: Process, console :: CONSOLE | e))
    -> Spec (process :: Process, console :: CONSOLE | e) Unit
    -> Eff  (process :: Process, console :: CONSOLE | e) Unit
run rs spec =
  runAff onError onSuccess testRun
  where
  testRun = do
    groups <- collect spec
    sequence_ (map (\f -> f groups) rs)
    await groups
  onError err = do withAttrs [31] $ print err
                   exit 1
  onSuccess results = when (not $ successful results) $ exit 1
