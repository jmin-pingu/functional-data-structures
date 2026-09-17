module Heap.BinomialHeapV2 (
  BinomialHeapV2,
  BinomialTreeV2(..),
  link,
  root,
  rank,
  insTree,
  isEmpty,
  insert,
  merge,
  findMin,
  deleteMin,
  tree
) where
-- Exercise 3.6: most of the rank annotations in this representation of binomial heaps are
-- redundant because we know that the children of a node of rank r have ranks r-1, ..., 0. Thus,
-- we can remove the rank annotations from each node and instead pair each tree at the top-level with
-- its rank, i.e.
data BinomialTreeV2 x = Node x [BinomialTreeV2 x] deriving (Show)
type BinomialHeapV2 x = [(Int, BinomialTreeV2 x)]

isEmpty :: BinomialHeapV2 x -> Bool
isEmpty [] = True
isEmpty _ = False

insert :: (Ord x) => BinomialHeapV2 x -> x -> BinomialHeapV2 x
insert ts x = insTree (0, Node x []) ts

-- Only top-level trees carry a rank; a child's rank is implied by its position.
rank :: (Int, BinomialTreeV2 x) -> Int
rank (r, _) = r

insTree :: (Ord x) => (Int, BinomialTreeV2 x) -> BinomialHeapV2 x -> BinomialHeapV2 x
insTree t [] = [t]
insTree t@(r, tt) h@((r', tt'):ts) = if r < r' then t : h
  else insTree (r+1, link tt tt') ts

merge :: (Ord x) => BinomialHeapV2 x -> BinomialHeapV2 x -> BinomialHeapV2 x
merge [] t = t
merge t [] = t
merge h0@(t0:ts0) h1@(t1:ts1) = let 
    (r0,tt0) = t0
    (r1,tt1) = t1
  in
    if r0 < r1 then t0 : merge ts0 h1
    else if r0 > r1 then t1 : merge h0 ts1
    else insTree (r0+1, link tt0 tt1) (merge ts0 ts1)

-- The minimum lives at the root of one of the top-level trees, but not
-- necessarily the first one.
findMin :: (Ord x) => BinomialHeapV2 x -> x
findMin [] = error "BinomialHeap is empty"
findMin t = let (m, _) = removeMinTree t in root m

removeMinTree :: (Ord x) => BinomialHeapV2 x -> (BinomialTreeV2 x, BinomialHeapV2 x)
removeMinTree [] = error "BinomialHeap is empty"
removeMinTree [(_, t)] = (t, [])
removeMinTree ((r, t):ts) = let (t', ts') = removeMinTree ts in if root t <= root t' then (t, ts) else (t', (r, t) : ts')

deleteMin :: (Ord x) => BinomialHeapV2 x -> BinomialHeapV2 x
deleteMin [] = error "BinomialHeap is empty"
deleteMin t = let 
  (m, ts) = removeMinTree t 
  tt = tree m
  r = length tt
  in 
  merge (reverse (zip [r-1,r-2..0] tt)) ts


link :: (Ord x) => BinomialTreeV2 x -> BinomialTreeV2 x -> BinomialTreeV2 x
link h0@(Node a l0) h1@(Node b l1) = if a < b then Node a (h1 : l0)
  else Node b (h0 : l1)

root :: BinomialTreeV2 x -> x
root (Node a _) = a

tree :: BinomialTreeV2 x -> [BinomialTreeV2 x]
tree (Node _ t) = t

