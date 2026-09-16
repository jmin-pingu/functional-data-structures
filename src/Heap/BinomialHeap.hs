{-# LANGUAGE FlexibleInstances #-}

module Heap.BinomialHeap (
  BinomialHeap,
  BinomialTree(..),
  link,
  root,
  rank,
  insTree,
  isEmpty,
  insert,
  merge,
  findMin,
  findMinDirect,
  deleteMin,
  removeMinTree,
) where

data BinomialTree x = Node Int x [BinomialTree x] deriving (Show)
type BinomialHeap x = [BinomialTree x]

isEmpty :: BinomialHeap x -> Bool
isEmpty [] = True
isEmpty _ = False

insert :: (Ord x) => BinomialHeap x -> x ->  BinomialHeap x
insert ts x = insTree (Node 0 x []) ts

merge :: (Ord x) => BinomialHeap x -> BinomialHeap x -> BinomialHeap x
merge [] t = t
merge t [] = t
merge h0@(t0:ts0) h1@(t1:ts1) = 
  if rank t0 < rank t1 then t0 : merge ts0 h1
  else if rank t0 > rank t1 then t1 : merge h0 ts1
  else insTree (link t0 t1) (merge ts0 ts1)

findMin :: (Ord x) => BinomialHeap x -> x
findMin ts = let (t, _) = removeMinTree ts in root t

deleteMin :: (Ord x) => BinomialHeap x -> BinomialHeap x
deleteMin ts = let ((Node _ _ t), t') = removeMinTree ts in merge (reverse t) t'

link :: (Ord x) => BinomialTree x -> BinomialTree x -> BinomialTree x
link h0@(Node r a l0) h1@(Node _ b l1) = if a < b then Node (r+1) a (h1 : l0)
  else Node (r+1) b (h0 : l1)

root :: BinomialTree x -> x
root (Node _ x _) = x

rank :: BinomialTree x -> Int
rank (Node r _ _) = r

insTree :: (Ord x) => BinomialTree x -> BinomialHeap x -> BinomialHeap x
insTree h [] = [h]
insTree h t'@(t:ts) = if (rank h < rank t) then h:t' else insTree (link h t) ts

removeMinTree :: (Ord x) => BinomialHeap x -> (BinomialTree x, BinomialHeap x)
removeMinTree [] = error "Not enough elements in binomial heap"
removeMinTree (t:[]) = (t, [])
removeMinTree (t:ts) = let (t',ts') = removeMinTree ts
  in if root t < root t' then (t, ts) else (t', t : ts')

-- TODO: implement Monad for this...

-- Exercise 3.5: Define findMin directly rather than via a call to removeMinTree
findMinDirect :: (Ord x) => BinomialHeap x -> x
findMinDirect [] = error "Not enough element in binomial heap"
findMinDirect [t] = root t
findMinDirect (t:ts) = min (root t) (findMinDirect ts)

-- Exercise 3.7: One clear advantage of leftist heaps over binomial heaps is that findMin takes
-- only O(1) time, rather than O(log n) time. The following functor skeleton
-- improves the running time of findMin to O(1) by storing the minimum element separately from
-- the rest of the heap.
--
-- TODO: translate pseudocode to Haskell
-- functor ExplicitMin (H: Heap): Heap =
-- struct
--   structure Elem = H.Elem
--   datatype Heap = E | NE of Elem.T x H.Heap
--   ...
-- end
-- 
-- Note that this functor is not specific to binomial heaps but takes implementation of heaps
-- as a parameter. Complete this functor so that findMin takes O(1) time and insert, merge, and 
-- deleteMin take O(log n) time.


