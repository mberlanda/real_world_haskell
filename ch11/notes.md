# 11 Testing and quality assurance

### QuickCheck: type-based testing

Testing for properties:

```
Prelude> :l QC-basics.hs
[1 of 1] Compiling Main             ( QC-basics.hs, interpreted )
Ok, modules loaded: Main.
*Main> prop_idempotent []
True
*Main> prop_idempotent [1,1,1,1]
True
*Main> prop_idempotent [1..100]
*Main> generate $ vectorOf 10 arbitrary :: IO [Bool]
[True,False,True,False,False,True,False,True,False,True]
*Main> :type quickCheck
quickCheck :: Testable prop => prop -> IO ()
*Main> quickCheck (prop_idempotent)
+++ OK, passed 100 tests.
*Main> quickCheck (prop_idempotent :: [Integer] -> Bool)
+++ OK, passed 100 tests.
*Main> quickCheck (prop_minimum :: [Integer] -> Bool)
*** Failed! Exception: 'Prelude.head: empty list' (after 1 test):
[]
*Main> :t prop_minimum'
prop_minimum' :: Ord a => [a] -> Property
*Main> quickCheck (prop_minimum' :: [Integer] -> Property)
+++ OK, passed 100 tests.
*Main> quickCheck (prop_append :: [Integer] -> [Integer] -> Property)
+++ OK, passed 100 tests.
```
> Note that we had to change the type of the property from being a simple `Bool` result to the more general `Property` type (the property itself is now a function that filters non-empty lists, before testing them, rather than a simple boolean constant)

`\\` represents the difference operator for lists

Testing against a model: you can use a built-in function or another function to verify the property. It can be used for inefficient but correct prototypes
