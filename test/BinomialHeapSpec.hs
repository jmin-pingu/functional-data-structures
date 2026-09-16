{-# LANGUAGE ScopedTypeVariables #-}

module BinomialHeapSpec (spec) where

import Heap.BinomialHeap (BinomialTree(..), BinomialHeap)
import Control.Exception (evaluate)
import Data.Bits (popCount)
import Data.List (sort)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import qualified Heap.BinomialHeap as BH

treeElems :: BinomialTree x -> [x]
treeElems (BH.Node _ x ts) = x : concatMap treeElems ts

heapElems :: BinomialHeap x -> [x]
heapElems = concatMap treeElems

-- A binomial tree of rank r has children of ranks r-1 .. 0 and 2^r nodes, and
-- every node is <= its children.
validTree :: Ord x => BinomialTree x -> Bool
validTree t@(BH.Node r x ts) =
  map BH.rank ts == [r - 1, r - 2 .. 0]
    && length (treeElems t) == 2 ^ r
    && all ((x <=) . BH.root) ts
    && all validTree ts

-- A binomial heap holds at most one tree per rank, in increasing rank order.
validHeap :: Ord x => BinomialHeap x -> Bool
validHeap h = all validTree h && increasing (map BH.rank h)
  where increasing rs = and (zipWith (<) rs (drop 1 rs))

drain :: Ord x => BinomialHeap x -> [x]
drain [] = []
drain h = BH.findMin h : drain (BH.deleteMin h)


fromL :: Ord x => [x] -> BinomialHeap x
fromL = foldl BH.insert []

singletonT :: x -> BinomialTree x
singletonT x = BH.Node 0 x []

spec :: Spec
spec = do
  describe "link" $ do
    prop "hangs the larger root under the smaller" $ \(x :: Int) (y :: Int) ->
      BH.root (BH.link (singletonT x) (singletonT y)) == min x y

    prop "raises the rank by one and doubles the size" $ \(x :: Int) (y :: Int) ->
      let t = BH.link (singletonT x) (singletonT y)
      in BH.rank t == 1 && length (treeElems t) == 2

    prop "produces a valid tree from two valid trees of equal rank" $
      \(x :: Int) (y :: Int) -> validTree (BH.link (singletonT x) (singletonT y))

  describe "insTree / insert'" $ do
    prop "keeps the heap valid" $ \(xs :: [Int]) ->
      validHeap (fromL xs)

    prop "keeps every element" $ \(xs :: [Int]) ->
      sort (heapElems (fromL xs)) == sort xs

    -- The tree ranks are the set bits of the element count.
    prop "holds popCount n trees for n elements" $ \(xs :: [Int]) ->
      length (fromL xs) == popCount (length xs)

    prop "insTree on an empty heap yields a singleton heap" $ \(x :: Int) ->
      map BH.rank (BH.insTree (singletonT x) []) == [0]

  describe "merge" $ do
    prop "keeps the heap valid" $ \(xs :: [Int]) (ys :: [Int]) ->
      validHeap (BH.merge (fromL xs) (fromL ys))

    prop "is union" $ \(xs :: [Int]) (ys :: [Int]) ->
      drain (BH.merge (fromL xs) (fromL ys)) == sort (xs ++ ys)

    prop "has the empty heap as an identity" $ \(xs :: [Int]) ->
      drain (BH.merge (fromL xs) []) == sort xs
        && drain (BH.merge [] (fromL xs)) == sort xs

  describe "findMin / deleteMin" $ do
    it "both error on the empty heap" $ do
      evaluate (BH.findMin ([] :: BinomialHeap Int)) `shouldThrow` anyErrorCall
      evaluate (null (BH.deleteMin ([] :: BinomialHeap Int)))
        `shouldThrow` anyErrorCall

    prop "findMin is the minimum" $ \(x :: Int) (xs :: [Int]) ->
      BH.findMin (fromL (x : xs)) == minimum (x : xs)

    prop "deleteMin drops exactly one element" $ \(x :: Int) (xs :: [Int]) ->
      length (heapElems (BH.deleteMin (fromL (x : xs)))) == length xs

    prop "deleteMin keeps the heap valid" $ \(x :: Int) (xs :: [Int]) ->
      validHeap (BH.deleteMin (fromL (x : xs)))

    prop "drains in sorted order" $ \(xs :: [Int]) ->
      drain (fromL xs) == sort xs

  describe "findMinDirect" $ do
    it "both error on the empty heap" $ do
      evaluate (BH.findMinDirect ([] :: BinomialHeap Int)) `shouldThrow` anyErrorCall

    prop "findMinDirect is the minimum" $ \(x :: Int) (xs :: [Int]) ->
      BH.findMinDirect (fromL (x : xs)) == minimum (x : xs)

  describe "removeMinTree" $
    prop "splits off the tree holding the minimum" $ \(x :: Int) (xs :: [Int]) ->
      let h = fromL (x : xs)
          (t, rest) = BH.removeMinTree h
      in BH.root t == minimum (x : xs)
           && sort (treeElems t ++ heapElems rest) == sort (x : xs)

