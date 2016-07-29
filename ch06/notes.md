# 06 Using Typeclasses

####Tables:
- [Selected Numeric Types](tables/selected_numeric_types.md)
- [Selected Numeric Functions and Constants](tables/selected_numeric_functions_and_constants.md)
- [Typeclass Instances for Numeric Types](tables/typeclass_instances_for_numeric_types.md)
- [Conversion Between Numeric Types](tables/conversion_between_numeric_types.md)

The need for typeclasses:
```
Prelude> :cd ch06
Prelude> :l naiveeq.hs 
[1 of 1] Compiling Main             ( naiveeq.hs, interpreted )
Ok, modules loaded: Main.
*Main> colorEq Red Red
True
*Main> colorEq Blue Red
False
```

What are typeclasses?

> The keywoard to define a typeclass in Haskell is class. Unfortunately, this may be confusing for those of you coming from an object-oriented background, as we are not really defining the same thing.

```hs
class  Eq a  where
    (==), (/=) :: a -> a -> Bool

       -- Minimal complete definition:
       --     (==) or (/=)
    x /= y     =  not (x == y)
    x == y     =  not (x /= y)
```

Declaring typeclass instances:
```
Prelude> :cd ch06
Prelude> :l naiveeq.hs 
[1 of 1] Compiling Main             ( naiveeq.hs, interpreted )
Ok, modules loaded: Main.
*Main> isEqual3 Red Blue
False
*Main> isNotEqual3 Red Blue
True
```

Important Built-In Typeclasses:
- show
- putStrLn
- read 
```
Prelude> (read "12.4")::Double
12.4
Prelude> :t it
it :: Double
```
Automatic Derivation:
> For many simple data types, the Haskell compiler can automatically derive instances of Read, Show, Bounded, Enum, Eq, and Ord for us.

Typeclasses at work:
```hs
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-} 
```
> Refer to the wikiHaskell documentation about[pragmas](https://wiki.haskell.org/Language_Pragmas).

JSON typeclasses without overlapping instances:
```
# ex. 1
Prelude> :m +Control.Arrow
Prelude Control.Arrow> :t second
second :: Arrow a => a b c -> a (d, b) (d, c)
Prelude Control.Arrow> first (*2) (1, 1)
(2,1)
Prelude Control.Arrow> second (*2) (1, 1)
(1,2)

 # ex. 2
 Prelude Control.Arrow> :t (,)
(,) :: a -> b -> (a, b)
Prelude Control.Arrow> :t (,,)
(,,) :: a -> b -> c -> (a, b, c)
Prelude Control.Arrow> (,) 1 2
(1,2)
```