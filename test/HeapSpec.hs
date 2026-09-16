{-# LANGUAGE ScopedTypeVariables #-}

module HeapSpec (spec) where

import Heap (Heap (..))
import Heap.LeftistHeap (LeftistHeap (..))
import Control.Exception (evaluate)
import Data.List (sort)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import qualified Heap.LeftistHeap as LeftistHeap

-- Structural checks, written here rather than in the module so they stay
-- independent of the implementation they are checking.

elems :: LeftistHeap x -> [x]
elems Empty = []
elems (Elem _ x a b) = x : elems a ++ elems b

size :: LeftistHeap x -> Int
size = length . elems

-- The rank of a node is the length of its right spine.
rightSpine :: LeftistHeap x -> Int
rightSpine Empty = 0
rightSpine (Elem _ _ _ b) = 1 + rightSpine b

-- Invariant 1: every node is <= everything below it.
heapOrdered :: Ord x => LeftistHeap x -> Bool
heapOrdered Empty = True
heapOrdered (Elem _ x a b) =
  all (x <=) (elems a) && all (x <=) (elems b) && heapOrdered a && heapOrdered b

-- Invariant 2 (leftist): the right spine is never longer than the left, and
-- the cached rank agrees with the actual right-spine length.
leftist :: LeftistHeap x -> Bool
leftist Empty = True
leftist h@(Elem r _ a b) =
  r == rightSpine h && LeftistHeap.rank a >= LeftistHeap.rank b && leftist a && leftist b

valid :: Ord x => LeftistHeap x -> Bool
valid h = heapOrdered h && leftist h

drain :: Ord x => LeftistHeap x -> [x]
drain h
  | isEmpty h = []
  | otherwise = findMin h : drain (deleteMin h)

viaInsert :: Ord x => [x] -> LeftistHeap x
viaInsert = foldl insert LeftistHeap.empty

viaInsert' :: Ord x => [x] -> LeftistHeap x
viaInsert' = foldl LeftistHeap.insert' LeftistHeap.empty

spec :: Spec
spec = do
  describe "empty / isEmpty" $ do
    it "reports the empty heap as empty" $
      isEmpty (LeftistHeap.empty :: LeftistHeap Int) `shouldBe` True

    prop "is empty exactly when nothing was inserted" $ \(xs :: [Int]) ->
      isEmpty (LeftistHeap.fromList xs) == null xs

  describe "findMin / deleteMin" $ do
    it "both error on the empty heap" $ do
      evaluate (findMin (LeftistHeap.empty :: LeftistHeap Int)) `shouldThrow` anyErrorCall
      evaluate (isEmpty (deleteMin (LeftistHeap.empty :: LeftistHeap Int)))
        `shouldThrow` anyErrorCall

    prop "findMin is the minimum" $ \(x :: Int) (xs :: [Int]) ->
      findMin (LeftistHeap.fromList (x : xs)) == minimum (x : xs)

    prop "deleteMin drops exactly one element" $ \(x :: Int) (xs :: [Int]) ->
      size (deleteMin (LeftistHeap.fromList (x : xs))) == length xs

  describe "invariants" $ do
    prop "hold after fromList" $ \(xs :: [Int]) ->
      valid (LeftistHeap.fromList xs)

    prop "hold after repeated insert" $ \(xs :: [Int]) ->
      valid (viaInsert xs)

    prop "hold after merge" $ \(xs :: [Int]) (ys :: [Int]) ->
      valid (merge (LeftistHeap.fromList xs) (LeftistHeap.fromList ys))

    prop "hold all the way down a drain" $ \(xs :: [Int]) ->
      all valid (iterateDeleteMin (LeftistHeap.fromList xs))

    -- Exercise 3.1 states the bound; this only checks it empirically.
    prop "keep the right spine within floor (log2 (n + 1))" $ \(xs :: [Int]) ->
      let h = LeftistHeap.fromList xs
      in (2 :: Integer) ^ rightSpine h <= fromIntegral (size h) + 1

  describe "sorted-list model" $ do
    prop "fromList drains in sorted order" $ \(xs :: [Int]) ->
      drain (LeftistHeap.fromList xs) == sort xs

    prop "repeated insert drains in sorted order" $ \(xs :: [Int]) ->
      drain (viaInsert xs) == sort xs

    prop "merge is union" $ \(xs :: [Int]) (ys :: [Int]) ->
      drain (merge (LeftistHeap.fromList xs) (LeftistHeap.fromList ys)) == sort (xs ++ ys)

    prop "keeps duplicates" $ \(xs :: [Int]) ->
      size (LeftistHeap.fromList (xs ++ xs)) == 2 * length xs

  describe "exercise answers agree with the book versions" $ do
    -- 3.2: insert' is defined directly; insert goes through merge.
    prop "insert' agrees with insert" $ \(xs :: [Int]) ->
      drain (viaInsert' xs) == drain (viaInsert xs)

    prop "insert' preserves the invariants" $ \(xs :: [Int]) ->
      valid (viaInsert' xs)

    -- 3.3: fromList merges in passes rather than one at a time.
    prop "fromList agrees with repeated insert" $ \(xs :: [Int]) ->
      drain (LeftistHeap.fromList xs) == drain (viaInsert xs)

iterateDeleteMin :: Ord x => LeftistHeap x -> [LeftistHeap x]
iterateDeleteMin h
  | isEmpty h = [h]
  | otherwise = h : iterateDeleteMin (deleteMin h)
