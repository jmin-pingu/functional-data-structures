module Heap (Heap(..)) where

class Heap h where
  isEmpty   :: h x -> Bool
  insert    :: (Ord x) => h x -> x -> h x 
  merge     :: (Ord x) => h x -> h x -> h x
  findMin   :: (Ord x) => h x -> x
  deleteMin :: (Ord x) => h x -> h x
  -- toList    :: (Ord x) => h x -> [x]
  -- fromList  :: (Ord x) => [x] -> h x

