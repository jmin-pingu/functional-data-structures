module List
( 
  List(..),
  empty,
  isEmpty,
  List.head,
  List.tail,
  update,
  suffixes,
  (List.++)
) where

data List a = Nil | Cons a (List a) deriving (Show)

empty :: List a
empty = Nil

isEmpty :: List a -> Bool
isEmpty Nil = True
isEmpty _ = False

head :: List a -> a
head (Cons x _) = x
head Nil = error "list is empty"

tail :: List a -> List a
tail (Cons _ xs) = xs
tail Nil = error "list is empty"

(++) :: List a -> List a -> List a
Nil ++ l1 = l1
(Cons x xs) ++ l1 =  Cons x (xs List.++ l1)

update :: (Ord n, Num n) => List a -> n -> a -> List a
update Nil _ _ = error "out of index"
update (Cons _ xs) 0 a = Cons a xs
update (Cons x xs) n a = Cons x (update xs (n-1) a)

-- Exercise 2.1
suffixes :: List a -> List (List a)
suffixes Nil = Nil
suffixes l = Cons l $ suffixes (List.tail l)
-- Show O(n) time and space, with n the length of the list.
-- A: tail' returns a pointer to original list. 
-- Therefore, list of lists just returns pointers
-- to each list's tail, meaning we reuse all the space
-- of the original list and have linear computation.
