-- file: ch10/TypeConstraints.hs
-- Constraints on type definitions are bad
data (Ord a) => OrdStack a = Bottom
                           | Item a (OrdStack a)
                             deriving (Show)

isIncreasing :: (Ord a) => OrdStack a -> Bool
isIncreasing (Item a rest@(Item b _))
    | a < b     = isIncreasing rest
    | otherwise = False
isIncreasing _  = True

-- However, because we wrote the type constraint on the type definition, that constraint ends up infecting places where it isn't needed: we need to add the Ord constraint to push, which does not care about the ordering of elements on the stack.
push :: (Ord a) => a -> OrdStack a -> OrdStack a
push a s = Item a s