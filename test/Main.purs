module Test.Main where

import Prelude (Unit, bind)

import Control.Monad.Eff         (Eff())
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Random (RANDOM)

import Test.Spec.Runner           (Process(), run)
import Test.Spec.Reporter.Console (consoleReporter)

import Test.Spec.SpecSpec   (specSpec)
import Test.Spec.ReporterSpec   (reporterSpec)
import Test.Spec.RunnerSpec     (runnerSpec)
import Test.Spec.AssertionSpec  (assertionSpec)

main :: forall eff. Eff (console :: CONSOLE, process :: Process, random :: RANDOM, err :: EXCEPTION | eff) Unit
main = run [consoleReporter] do
  specSpec
  runnerSpec
  reporterSpec
  assertionSpec
