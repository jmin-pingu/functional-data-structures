module Set.RBTree (
  RBTree(..),
  Color(..)
) where
import qualified Set as S

data Color = R | B
data RBTree x = Empty | Node Color (RBTree x) x (RBTree x)

-- Exercise 3.8: prove that the maximum depth of a node in a red-black tree of size n is at most
-- 2*floor(log(n+1))
-- TODO: 

instance S.Set (RBTree) where
  -- insert        :: (Ord x) => h x -> x -> h x

  isEmpty Empty = True
  isEmpty _ = False

  member Empty _ = False
  member (Node _ l b r) a = 
    if a < b then S.member l a 
    else if a > b then S.member r a 
    else True

  insert s x = let
    blacken (Node _ l y r) = Node B l y r
    blacken Empty = error "unreachable"

    ins Empty = Node R Empty x Empty
    ins t@(Node c l z r) =
      if x < z then balance c (ins l) z r
      else if x > z then balance c l z (ins r)
      else t
    in
    blacken $ ins s

balance :: Color -> RBTree x -> x -> RBTree x -> RBTree x
balance B (Node R (Node R a x b) y c) z d = Node R (Node B a x b) y (Node B c z d)
balance B (Node R a x (Node R b y c)) z d = Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R (Node R b y c) z d) = Node R (Node B a x b) y (Node B c z d)
balance B a x (Node R b y (Node R c z d)) = Node R (Node B a x b) y (Node B c z d)
balance c l x r = Node c l x r

