{-# LANGUAGE ScopedTypeVariables #-}

module RBTreeSpec (spec) where

import Set (Set(..))
import Set.RBTree (RBTree(..))
import Data.List (nub, sort)
import Test.Hspec
import Test.Hspec.QuickCheck (prop)

fromL :: Ord a => [a] -> RBTree a
fromL = foldl insert Empty

inorder :: RBTree a -> [a]
inorder Empty = []
inorder (Node _ l x r) = inorder l ++ [x] ++ inorder r

size :: RBTree a -> Int
size = length . inorder

depth :: RBTree a -> Int
depth Empty = 0
depth (Node _ l _ r) = 1 + max (depth l) (depth r)

-- The search-tree invariant: an inorder walk is strictly increasing.
ordered :: Ord a => RBTree a -> Bool
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

