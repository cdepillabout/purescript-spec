module Test.Spec.Reporter.Console (consoleReporter) where

import Prelude

import Control.Monad.Aff           (Aff())
import Control.Monad.Eff           (Eff())
import Control.Monad.Eff.Class     (liftEff)
import Control.Monad.Eff.Console   (CONSOLE(), log)
import Control.Monad.Eff.Exception (message)
import Data.Array                  (concatMap)
import Data.Foldable               (intercalate, traverse_)

import Test.Spec          (Group(), Result(..), await)
import Test.Spec.Console  (withAttrs, write, writeln)
import Test.Spec.Reporter (Entry(..), Reporter(), collapse)
import Test.Spec.Summary  (Summary(..), summarize)

pluralize :: String -> Int -> String
pluralize s 1 = s
pluralize s _ = s <> "s"

printPassedFailed :: Int -> Int -> Eff (console :: CONSOLE) Unit
printPassedFailed p f = do
  let total = p + f
      testStr = pluralize "test" total
      amount = show p ++ "/" ++ (show total) ++ " " ++ testStr ++ " passed"
      attrs = if f > 0 then [31] else [32]
  withAttrs attrs $ writeln amount

printPending :: Int -> Eff (console :: CONSOLE) Unit
printPending p =
  if p > 0 then withAttrs [33] do write $ show p
                                  write " "
                                  write (pluralize "test" p)
                                  writeln " pending"
           else return unit

printSummary' :: Summary -> Eff (console :: CONSOLE) Unit
printSummary' (Count passed failed pending) = do
  writeln ""
  withAttrs [1] $ writeln "Summary"
  printPassedFailed passed failed
  printPending pending
  writeln ""

printSummary :: Array (Group Result)
                -> Eff (console :: CONSOLE) Unit
printSummary groups = printSummary' $ summarize groups

printEntry :: Entry Result
           -> Eff (console :: CONSOLE) Unit
printEntry (It name Success) = do
  withAttrs [32] $ writeln $  "✓︎ " ++ name
printEntry (Pending name) = do
  withAttrs [33] $ writeln $  "~ " ++ name
printEntry (It name (Failure err)) = do
  withAttrs [31] $ writeln $ "✗ " ++ name ++ ":"
  log ""
  withAttrs [31] $ writeln $ "  " ++ message err
printEntry (Describe n) = do
  writeln ""
  printNames n
  where printNames ns = withAttrs [1, 35] $ writeln $ intercalate " » " ns

consoleReporter :: forall e. Array (Group (Aff e Unit))
                   -> Aff (console :: CONSOLE | e) Unit
consoleReporter groups = do
  results <- await groups
  let printAll :: Array (Group Result) -> Eff (console :: CONSOLE) Unit
      printAll r = do traverse_ printEntry (collapse r)
                      printSummary results
      a :: Aff (console :: CONSOLE) Unit
      a = liftEff $ printAll results
  a
