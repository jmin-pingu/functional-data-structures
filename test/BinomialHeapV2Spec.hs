{-# LANGUAGE ScopedTypeVariables #-}

module BinomialHeapV2Spec (spec) where

import Heap.BinomialHeapV2 (BinomialTreeV2(..), BinomialHeapV2)
import Control.Exception (evaluate)
import Data.Bits (popCount)
import Data.List (sort)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import qualified Heap.BinomialHeapV2 as BH

treeElems :: BinomialTreeV2 x -> [x]
treeElems (BH.Node x ts) = x : concatMap treeElems ts

-- A binomial tree of rank r has children of ranks r-1 .. 0 and 2^r nodes, and
-- every node is <= its children. The children no longer carry a rank
-- annotation, so their ranks are checked by position instead.
validTree :: Ord x => (Int, BinomialTreeV2 x) -> Bool
validTree (r, t@(BH.Node x ts)) =
  length ts == r
    && length (treeElems t) == 2 ^ r
    && all ((x <=) . BH.root) ts
    && all validTree (zip [r-1, r-2..0] ts)

-- A binomial heap holds at most one tree per rank, in increasing rank order.
validHeap :: Ord x => BinomialHeapV2 x -> Bool
validHeap h = all validTree h && increasing (map BH.rank h)
  where increasing rs = and (zipWith (<) rs (drop 1 rs))

drain :: Ord x => BinomialHeapV2 x -> [x]
drain [] = []
drain h = BH.findMin h : drain (BH.deleteMin h)

fromL :: Ord x => [x] -> BinomialHeapV2 x
fromL = foldl BH.insert []

singletonT :: x -> BinomialTreeV2 x
singletonT x = BH.Node x []

spec :: Spec
spec = do
  describe "link" $ do
    prop "hangs the larger root under the smaller" $ \(x :: Int) (y :: Int) ->
      BH.root (BH.link (singletonT x) (singletonT y)) == min x y

    prop "raises the rank by one and doubles the size" $ \(x :: Int) (y :: Int) ->
      let t = BH.link (singletonT x) (singletonT y)
      in length (BH.tree t) == 1 && length (treeElems t) == 2

    prop "produces a valid tree from two valid trees of equal rank" $
      \(x :: Int) (y :: Int) -> validTree (1, BH.link (singletonT x) (singletonT y))

  describe "insTree / insert'" $ do
    prop "keeps the heap valid" $ \(xs :: [Int]) ->
      validHeap (fromL xs)

    -- The tree ranks are the set bits of the element count.
    prop "holds popCount n trees for n elements" $ \(xs :: [Int]) ->
      length (fromL xs) == popCount (length xs)

    prop "insTree on an empty heap yields a singleton heap" $ \(x :: Int) ->
      map BH.rank (BH.insert [] x) == [0]

  describe "merge" $ do
    prop "keeps the heap valid" $ \(xs :: [Int]) (ys :: [Int]) ->
      validHeap (BH.merge (fromL xs) (fromL ys))

    prop "is union" $ \(xs :: [Int]) (ys :: [Int]) ->
      drain (BH.merge (fromL xs) (fromL ys)) == sort (xs ++ ys)

    -- NOTE:  
    prop "has the empty heap as an identity" $ \(xs :: [Int]) ->
      drain (BH.merge (fromL xs) []) == sort xs
        && drain (BH.merge [] (fromL xs)) == sort xs

  describe "findMin / deleteMin" $ do
    it "both error on the empty heap" $ do
      evaluate (BH.findMin ([] :: BinomialHeapV2 Int)) `shouldThrow` anyErrorCall
      evaluate (null (BH.deleteMin ([] :: BinomialHeapV2 Int)))
        `shouldThrow` anyErrorCall

    prop "findMin is the minimum" $ \(x :: Int) (xs :: [Int]) ->
      BH.findMin (fromL (x : xs)) == minimum (x : xs)

    prop "deleteMin keeps the heap valid" $ \(x :: Int) (xs :: [Int]) ->
      validHeap (BH.deleteMin (fromL (x : xs)))

    prop "drains in sorted order" $ \(xs :: [Int]) ->
      drain (fromL xs) == sort xs

