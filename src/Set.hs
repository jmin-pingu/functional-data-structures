module Set (Set(..)) where

-- NOTE: the `Set` type API should not be dependent on the constraint of elements of the set
class Set h where
  isEmpty       :: h x -> Bool
  insert        :: (Ord x) => h x -> x -> h x
  member        :: (Ord x) => h x -> x -> Bool
  -- drop          :: (Ord x) => h x -> x -> h x
  -- union         :: (Ord x) => h x -> h x -> h x
  -- intersection  :: (Ord x) => h x -> h x-> h x
  -- toList        :: (Ord x) => h x -> [x]
  -- fromList      :: (Ord x) => [x] -> h x

