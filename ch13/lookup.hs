-- file: ch13/lookup.hs

myLookup:: Eq a => a -> [(a, b)] -> Maybe b
myLookup _ [] = Nothing
myLookup key ((thiskey, thisval):rest)
    | thiskey == key = Just thisval
    | otherwise      = myLookup key rest
