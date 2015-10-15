module Test.Spec (
  Name(..),
  Result(..),
  Group(..),
  Spec(..),
  describe,
  pending,
  it,
  collect,
  await
  ) where

import Prelude

import Control.Monad.Aff           (Aff(), attempt)
import Control.Monad.Eff.Exception (Error())
import Control.Monad.State.Class   (modify)
import Control.Monad.State.Trans   (StateT(), runStateT)
import Control.Monad.Trans         (lift)
import Data.Array                  (foldM, cons)
import Data.Either                 (either)
import Data.Foldable               (mconcat)
import Data.Monoid                 (Monoid)
import Data.Tuple                  (snd)

type Name = String

data Result = Success
            | Failure Error

instance showResult :: Show Result where
  show Success = "Success"
  show (Failure err) = "Failure (Error ...)"

instance eqResult :: Eq Result where
  eq Success Success = true
  eq (Failure _) (Failure _) = true
  eq _ _ = false

data Group t = Describe Name (Array (Group t))
             | It Name t
             | Pending Name

instance showGroup :: Show (Group Result) where
  show (Describe name groups) = "Describe \"" ++ name ++ "\" " ++ (show groups)
  show (It name result) = "It " ++ name ++ ": " ++ show result
  show (Pending name) = "Pending " ++ name

instance eqGroup :: Eq (Group Result) where
  eq (Describe n1 gs1) (Describe n2 gs2) = n1 == n2 && gs1 == gs2
  eq (It n1 r1) (It n2 r2) = n1 == n2 && r1 == r2
  eq (Pending n1) (Pending n2) = n1 == n2
  eq _ _ = false

type Spec r t = StateT (Array (Group (Aff r Unit))) (Aff r) t

describe :: forall r. String
         -> Spec r Unit
         -> Spec r Unit
describe name its = do
  results <- lift $ collect its
  modify $ \r -> r ++ [Describe name results]
  return unit

pending :: forall r. String
        -> Spec r Unit
pending name = modify $ \p -> p ++ [(Pending name :: Group (Aff r Unit))]

runCatch :: forall r.
            Aff r Unit
            -> Aff r Result
runCatch tests = do
  e <- attempt tests
  either onError onSuccess e
  where
  onError e = return (Failure e)
  onSuccess _ = return Success

it :: forall r. String
    -> Aff r Unit
    -> Spec r Unit
it description tests = modify $ \p -> p ++ [It description tests]

collect :: forall r. Spec r Unit
        -> Aff r (Array (Group (Aff r Unit)))
collect r = do
  c <- runStateT r []
  return $ snd c

await :: forall r.
         Array (Group (Aff r Unit))
         -> Aff r (Array (Group Result))
await toAwait = foldM f [] toAwait
  where f r (It name test) = do
          res <- runCatch test
          return ( r ++ [It name res])
        f r (Describe name sub) = do
          subResults <- await sub
          return $ r ++ [Describe name subResults]
        f r (Pending name) = return $ r ++ [Pending name]
