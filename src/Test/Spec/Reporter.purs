module Test.Spec.Reporter (
  Entry(..),
  Reporter(),
  collapse
  ) where

import Prelude

import Control.Monad.Aff           (Aff())
import Control.Monad.Eff           (Eff())
import Control.Monad.Eff.Exception (message)
import Data.Array                  (concatMap, cons, foldM)
import Data.Foldable               (foldl, intercalate, mconcat)

import qualified Test.Spec as S

data Entry t = Describe (Array S.Name)
             | It S.Name t
             | Pending S.Name

instance eqEntry :: Eq (Entry S.Result) where
  eq (Describe n1) (Describe n2) = n1 == n2
  eq (It n1 S.Success) (It n2 S.Success) = n1 == n2
  eq (It n1 (S.Failure e1)) (It n2 (S.Failure e2)) =
    n1 == n2 && (message e1) == (message e2)
  eq (Pending n1) (Pending n2) = n1 == n2
  eq _ _ = false

instance showEntry :: Show (Entry S.Result) where
  show (Describe names) = "Describe \"" ++ (intercalate " » " names) ++ "\""
  show (It name S.Success) = "It \"" ++ name ++ "\" Success"
  show (It name (S.Failure err)) = "It \"" ++ name ++ "\" (Failure \"" ++ message err ++ "\")"
  show (Pending name) = "Pending \"" ++ name ++ "\""

type Reporter e r = Array (S.Group (Aff e Unit)) -> Aff r Unit

countDescribes :: forall t r. Array (Entry t) -> Int
countDescribes groups = foldl f 0 groups
  where f c (Describe _) = c + 1
        f c _ = c

collapse :: forall t. Array (S.Group t) -> Array (Entry t)
collapse groups = concatMap collapseGroup groups
  where
  collapseGroup (S.It name test) = [It name test]
  collapseGroup (S.Pending name) = [Pending name]
  collapseGroup (S.Describe name groups) =
    let sub = collapse groups
        prependName (Describe names) = Describe $ cons name names
        prependName e = e
        c = countDescribes sub
    in if c == 0 then cons (Describe [name]) sub
                else map prependName sub
