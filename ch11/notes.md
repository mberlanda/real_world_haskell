# 11 Testing and quality assurance

### QuickCheck: type-based testing

Testing for properties:

```
Prelude> :l QC-basics.hs
[1 of 1] Compiling Main             ( QC-basics.hs, interpreted )
Ok, modules loaded: Main.
-- QuickCheck: Type-Based Testing
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

-- Testing for Properties
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

### Testing case study: specifying a pretty printer

Generating test Data: [Prettify2.hs](/Prettify2.hs)

```
$ cabal update
Downloading the latest package list from hackage.haskell.org
$ cabal install random-strings
Resolving dependencies...
Downloading random-strings-0.1.1.0...
Configuring random-strings-0.1.1.0...
Building random-strings-0.1.1.0...
Installed random-strings-0.1.1.0
# The expected result is:
ghci> generate 10 (System.Random.mkStdGen 2) arbitrary :: [Doc]
[Line,Empty,Union Empty Line,Union (Char 'R') (Concat (Union Line (Concat
(Text "i@BmSu") (Char ')'))) (Union (Concat (Concat (Concat (Text "kqV!iN")
Line) Line) Line) Line)),Char 'M',Text "YdwVLrQOQh"]

```


Working version after

```
Prelude> :l Arbitrary.hs
Main> generate $ vectorOf 10 arbitrary :: IO [Ternary]
[Unknown,Yes,Yes,Unknown,Unknown,No,Unknown,No,Unknown,Yes]

```
