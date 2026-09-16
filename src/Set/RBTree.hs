module Set.RBTree (
  RBTree(..),
  Color(..)
) where

data Color = R | B
data RBTree x = Empty | Node Color (RBTree x) x (RBTree x)

-- NOTE: all empty nodes are considered to be black
--
-- The two invariants are...
-- 1) No red node has a red child.
-- 2) Every bath from the root to an empty node contains the same
-- number of black nodes.


