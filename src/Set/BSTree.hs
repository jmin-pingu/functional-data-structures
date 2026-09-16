module Set.BSTree
(
  BSTree(..),
  empty,
  lazyMember,
  complete
) where
import qualified Set as S

data BSTree a = Nil | Node a (BSTree a) (BSTree a) deriving (Show)

instance S.Set (BSTree) where 
  isEmpty Nil = True
  isEmpty _ = False

  insert Nil e = Node e Nil Nil
  insert (Node t l r) e
    | e < t = Node t (S.insert l e) r
    | e > t = Node t l (S.insert r e)
    | otherwise = Node t l r
  
  member Nil _ = False
  member (Node t l r) e
    | e < t = S.member l e
    | e > t = S.member r e
    | otherwise = True

  -- TODO: drop
  -- drop Nil _ = error "not enough elements"
  -- drop (Node t l r) e
  --   | t == e = if l let
  --   | e < t = Node t (S.drop l e) r
  --   | e > t = Node t l (S.drop r e)
  --   | otherwise = True

  -- TODO: union + intersection

empty :: BSTree a
empty = Nil

-- Exercise 2.2: in the worst case, member performs approximately 2d comparisons,
-- where d is the depth of the tree. Rewrite member to take no more than d + 1 
-- comparisons by keeping track of a candidate element that *might* be equal 
-- to the query element.
lazyMember :: (Ord a) => a -> [a] -> BSTree a -> Bool
lazyMember x crumbs Nil = x `elem` crumbs
lazyMember x crumbs (Node item l r)
  | x < item = lazyMember x ([item] ++ crumbs) l
  | otherwise = lazyMember x ([item] ++ crumbs) r

-- Exercise 2.3: inserting an existing element into a binary search tree copies
-- the entire search path even though the copied nodes are indistinguishable from
-- the originals. Rewrite insert using exceptions to avoid this copying. Establish
-- one handler per insertion rather than one handler per iteration.
-- A: https://stackoverflow.com/questions/23808790/is-there-a-way-to-avoid-copying-the-whole-search-path-of-a-binary-tree-on-insert

-- Exercise 2.5: sharing can be useful within a single object, not just between
-- objects. For example, if two subtrees of a given node are identical, then they
-- can be represented by the same tree.

-- (a) Using this idea, write a function complete of type Elem x Int -> Tree where
-- `complete x d` creates a complete binary tree of depth `d` with `x` stored in
-- every node.
complete :: (Eq n, Num n) => a -> n -> BSTree a
complete x d
  | d == 0    = Node x Nil Nil
  | otherwise = Node x (complete x $ d-1) (complete x $ d-1)

-- (b) Extend this function to create balanced trees of arbitrary size.
-- If d == 2^n otherwise, then we can decompose it into 2m + 1 for some m
-- TODO: 
