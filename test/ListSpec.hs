{-# LANGUAGE ScopedTypeVariables #-}

module ListSpec (spec) where

import Control.Exception (evaluate)
import Data.List (isPrefixOf, tails)
import List (List (..))
import Test.Hspec
import Test.Hspec.QuickCheck (prop)
import Test.QuickCheck (NonNegative (..), (==>))
import qualified List

fromL :: [a] -> List a
fromL = foldr Cons Nil

toL :: List a -> [a]
toL Nil = []
toL (Cons x xs) = x : toL xs

suffixesOf :: [a] -> [[a]]
suffixesOf = map toL . toL . List.suffixes . fromL

spec :: Spec
spec = do
  describe "empty / isEmpty" $ do
    it "reports the empty list as empty" $
      List.isEmpty List.empty `shouldBe` True

    prop "is empty exactly when the list has no elements" $ \(xs :: [Int]) ->
      List.isEmpty (fromL xs) == null xs

  describe "head / tail" $ do
    it "errors on the empty list" $ do
      evaluate (List.head List.empty :: Int) `shouldThrow` anyErrorCall
      evaluate (List.isEmpty (List.tail (Nil :: List Int)))
        `shouldThrow` anyErrorCall

    prop "peel one element at a time" $ \(x :: Int) (xs :: [Int]) ->
      let l = fromL (x : xs)
      in List.head l == x && toL (List.tail l) == xs

    prop "round-trip through the List representation" $ \(xs :: [Int]) ->
      toL (fromL xs) == xs

  describe "(++)" $ do
    prop "concatenates" $ \(xs :: [Int]) (ys :: [Int]) ->
      toL (fromL xs List.++ fromL ys) == xs ++ ys

    prop "has Nil as a left and right identity" $ \(xs :: [Int]) ->
      toL (Nil List.++ fromL xs) == xs && toL (fromL xs List.++ Nil) == xs

    prop "is associative" $ \(xs :: [Int]) (ys :: [Int]) (zs :: [Int]) ->
      toL ((fromL xs List.++ fromL ys) List.++ fromL zs)
        == toL (fromL xs List.++ (fromL ys List.++ fromL zs))

  describe "update" $ do
    it "errors when the index runs off the end" $
      evaluate (length (toL (List.update (fromL [1, 2, 3 :: Int]) (5 :: Int) 9)))
        `shouldThrow` anyErrorCall

    it "errors on the empty list, at every index" $ do
      evaluate (toL (List.update (Nil :: List Int) (0 :: Int) 9))
        `shouldThrow` anyErrorCall
      evaluate (toL (List.update (Nil :: List Int) (3 :: Int) 9))
        `shouldThrow` anyErrorCall

    prop "preserves length and replaces the element at the index" $
      \(xs :: [Int]) (y :: Int) (NonNegative i) ->
        i < length xs
          ==> toL (List.update (fromL xs) (i :: Int) y)
            == take i xs ++ [y] ++ drop (i + 1) xs

    prop "leaves every other position untouched" $
      \(xs :: [Int]) (y :: Int) (NonNegative i) ->
        i < length xs
          ==> let ys = toL (List.update (fromL xs) (i :: Int) y)
              in length ys == length xs
                   && [z | (j, z) <- zip [0 ..] ys, j /= i]
                     == [z | (j, z) <- zip [0 ..] xs, j /= (i :: Int)]

  describe "suffixes" $ do
    prop "starts with the whole list" $ \(x :: Int) (xs :: [Int]) ->
      take 1 (suffixesOf (x : xs)) == [x : xs]

    prop "each suffix is the tail of the one before it" $ \(xs :: [Int]) ->
      let ss = suffixesOf xs
      in and (zipWith (\a b -> drop 1 a == b) ss (drop 1 ss))

    prop "yields genuine suffixes, in order" $ \(xs :: [Int]) ->
      suffixesOf xs `isPrefixOf` tails xs

    prop "shares structure: every suffix is a tail' chain of the original" $
      \(xs :: [Int]) ->
        all (`elem` tails xs) (suffixesOf xs)
