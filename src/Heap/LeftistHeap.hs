module Heap.LeftistHeap (
  LeftistHeap(..),
  empty,
  insert',
  rank,
  fromList,
) where
import qualified Heap as H


-- Not best practice to put constraints on data declarations
-- https://stackoverflow.com/questions/40825878/type-constraints-in-data-declaration-haskell
data LeftistHeap x = Empty | Elem Int x (LeftistHeap x) (LeftistHeap x) deriving (Show)

instance H.Heap LeftistHeap where 
  isEmpty Empty = True
  isEmpty _ = False
  
  insert h x = H.merge h (Elem 1 x Empty Empty)
  
  -- Key function: the invariant is the leftist property (right spine of any node is the shortest path to an empty node)
  -- Notice we only reshuffle the right heap. The rank of a node is the length of its right spine.
  merge h Empty = h
  merge Empty h = h
  merge h1@(Elem _ x a1 b1) h2@(Elem _ y a2 b2) = if x < y 
    then makeTree x a1 (H.merge b1 h2) 
    else makeTree y a2 (H.merge b2 h1)
  
  findMin Empty           = error "heap is empty" 
  findMin (Elem _ x _ _)  = x
  
  deleteMin Empty           = error "heap is empty" 
  deleteMin (Elem _ _ a b)  = H.merge a b
  
rank :: LeftistHeap x -> Int
rank Empty = 0
rank (Elem r _ _ _) = r

empty :: LeftistHeap x
empty = Empty
  

-- The key is `makeTree` forcing the largest rank element to the left!
-- The recursion works because we assume that the arguments themselves satisfy the
-- leftist property.
makeTree :: x -> LeftistHeap x -> LeftistHeap x -> LeftistHeap x
makeTree x a b = if rank a >= rank b then Elem (rank b + 1) x a b else Elem (rank a + 1) x b a

-- Exercise 3.1: Prove that the right spine of a leftist heap of size n contains at most floor(log(n + 1)) elements.
--
-- Induct on the right spine length.
-- r = 0 is the empty heap, which has 0 = 2^0 − 1 elements.
-- For r >= 1, the root's right child has right spine length r − 1, so by induction it has at least 2^(r−1) − 1 elements.
-- By the leftist property, the left child has rank at least r − 1, so it too has at least 2^(r−1) − 1 elements.
-- Total is at least 1 + 2(2^(r−1) − 1) = 2^r − 1.
--
-- Now if a heap of size n has right spine length r, then n >= 2^r − 1, so r <=log(n + 1). Since r is an integer,
-- r <= floor(log(n + 1)).

-- Exercise 3.2: define insert directly rather than with a call to merge
insert' :: (Ord x) => LeftistHeap x -> x -> LeftistHeap x
insert' Empty x = (Elem 1 x Empty Empty)
insert' (Elem _ y a b) x = if x < y then makeTree x a (insert' b y) else makeTree y a (insert' b x)

-- Exercise 3.3: implement a function fromList of type List x -> Heap x that produces a leftist heap from an unordered list by first 
-- converting each element into a singleton heap and then merging the heaps until only one heap remains. Merge the heaps
-- in ceil(log(n)) passes, where each pass merges adjacent pairs of heaps. Show that fromList takes only O(n) time.

fromList :: (Ord x) => [x] -> LeftistHeap x
fromList l = mergeHeapsList $ map (\x -> Elem 1 x Empty Empty) l

mergeHeapsList :: (Ord x) => [LeftistHeap x] -> LeftistHeap x
mergeHeapsList [] = Empty
mergeHeapsList [x] = x 
mergeHeapsList l = mergeHeapsList $ mergeHeapRun l

mergeHeapRun :: (Ord x) => [LeftistHeap x] -> [LeftistHeap x]
mergeHeapRun (x:y:rest) = H.merge x y : mergeHeapRun rest
mergeHeapRun x = x

-- The runtime proof is as follows:
-- Assume r = \log(n) \iff 2^r = n. The strategy is to compute "bottom-up".
-- The key to the proof is that the runtime of merge is a function of tree depth since we 
-- merge heaps of size 2^d if d is the depth bottom-up. 
-- For the depth, we want the product of the runtime time of merge and the number
-- of heaps that are merged at that depth. We've already established that `merge` takes O(log(n)).
-- Thus the formula is $\sum_{d=0}^r \log(2^d) 2^{r-d} = \sum_{d=0}^r d2^{r-d} = 
-- \sum_{d=0}^r d2^{-d}2^{\log(n)} = n \sum_{d=0}d2^{-d}$ which is O(n) since the series converges to 
-- a constant as $d \rightarrow \infty$.

