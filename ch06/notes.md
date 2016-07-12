# 06 Using Typeclasses

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