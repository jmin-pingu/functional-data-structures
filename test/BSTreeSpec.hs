{-# LANGUAGE ScopedTypeVariables #-}

module BSTreeSpec (spec) where

import Set (Set(..))
import Set.BSTree (BSTree(..))
import Data.List (nub, sort)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (choose, forAll)

import qualified Set.BSTree as T

fromL :: Ord a => [a] -> BSTree a
fromL = foldl insert T.empty

inorder :: BSTree a -> [a]
inorder Nil = []
inorder (Node x l r) = inorder l ++ [x] ++ inorder r

size :: BSTree a -> Int
size = length . inorder

depth :: BSTree a -> Int
depth Nil = 0
depth (Node _ l r) = 1 + max (depth l) (depth r)

-- The search-tree invariant: an inorder walk is strictly increasing.
ordered :: Ord a => BSTree a -> Bool
ordered t = let xs = inorder t in and (zipWith (<) xs (drop 1 xs))

spec :: Spec
spec = do
  describe "insert' / member" $ do
    prop "finds everything that was inserted" $ \(xs :: [Int]) ->
      all (member $ fromL xs) xs

    prop "rejects everything that was not inserted" $ \(xs :: [Int]) (y :: Int) ->
      member (fromL xs) y == (y `elem` xs)

    prop "keeps the search-tree invariant" $ \(xs :: [Int]) ->
      ordered (fromL xs)

    prop "is idempotent on an element already present" $ \(x :: Int) (xs :: [Int]) ->
      let t = fromL (x : xs)
      in inorder (insert t x) == inorder t

    prop "is a set: agrees with sort . nub as a model" $ \(xs :: [Int]) ->
      inorder (fromL xs) == nub (sort xs)

    prop "holds no more elements than it was given" $ \(xs :: [Int]) ->
      size (fromL xs) == length (nub xs)

  -- Exercise 2.2 asks for a member that makes at most d + 1 comparisons. Only
  -- the answer's behaviour is pinned here, not its comparison count.
  describe "lazyMember (exercise 2.2)" $ do
    prop "agrees with member on elements that are present" $ \(xs :: [Int]) ->
      all (\x -> T.lazyMember x [] (fromL xs)) xs

    prop "agrees with member everywhere" $ \(xs :: [Int]) (y :: Int) ->
      T.lazyMember y [] (fromL xs) == member (fromL xs) y

  -- Exercise 2.5a: `complete x d` is a complete tree of depth d.
  describe "complete (exercise 2.5a)" $ do
    it "is a single node at d = 0" $
      inorder (T.complete 'x' (0 :: Int)) `shouldBe` "x"

    prop "stores the same element in every node" $
      forAll (choose (0, 6 :: Int)) $ \d ->
        all (== 'x') (inorder (T.complete 'x' d))

    prop "has 2^(d+1) - 1 nodes" $
      forAll (choose (0, 6 :: Int)) $ \d ->
        size (T.complete 'x' d) == 2 ^ (d + 1) - 1

    prop "is balanced" $
      forAll (choose (0, 6 :: Int)) $ \d ->
        depth (T.complete 'x' d) == d + 1
