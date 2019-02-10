# 13 Data Structures

The two most common ways to handle data that is unordered but is indexed by a key, are:


### Association Lists


An *Association List* is normal list containing (key, value) tuples. E.g. `[(Integer, String)]`

```hs
Prelude> let al = [(1, "one"), (2, "two")]
Prelude> lookup 1 al
Just "one"
Prelude> lookup 5 al
Nothing
Prelude> :t lookup
lookup :: Eq a => a -> [(a, b)] -> Maybe b
```

```
$ runhaskell ch13/passwd-al.hs /etc/passwd 0
root
```

### Maps

*`Map`* type provided by `Data.Map` works like hashtables in other languages

```
Prelude> :l ch13/buildmap.hs
[1 of 1] Compiling Main             ( ch13/buildmap.hs, interpreted )
Ok, modules loaded: Main.
*Main> mapManual
fromList [(1,"one"),(2,"two"),(3,"three"),(4,"four")]
*Main> mapFromAL
fromList [(1,"one"),(2,"two"),(3,"three"),(4,"four")]
*Main> mapFold
fromList [(1,"one"),(2,"two"),(3,"three"),(4,"four")]
```

### Functions Are Data, Too

```
Prelude> :l ch13/funcrecs.hs
[1 of 1] Compiling Main             ( ch13/funcrecs.hs, interpreted )
Ok, modules loaded: Main.
*Main> :t plus5
plus5 :: FuncRec
*Main> name plus5
"plus5"
*Main> :t colorCalc plus5
colorCalc plus5 :: Int -> (CustomColor, Int)
*Main> (colorCalc plus5) 7
(CustomColor {red = 255, green = 0, blue = 255},12)
*Main> :t colorCalc always0
colorCalc always0 :: Int -> (CustomColor, Int)
*Main> (colorCalc always0) 7
(CustomColor {red = 255, green = 0, blue = 255},0)
*Main> colorCalc always0 $ 5
(CustomColor {red = 255, green = 0, blue = 255},0)
```
```
*Main> :t plus5
plus5 :: FuncRec
*Main> name plus5
"plus5"
*Main> calc plus5 $ 7
12
*Main> let plus5a = plus5 {name = "PLUS5A"}
*Main> calc plus5a $ 7
12
*Main> name plus5a
"PLUS5A"
use :? for help.
*Main> :t calc plus5a
calc plus5a :: Int -> Int
```

### Extended Example: /etc/passwd

### Extended Example: Numeric Types

Back to ch06, it was shown in some [tables](../ch06/tables) how numeric typeclasses come to Haskell.

```hs
# Prelude> :t (+)
(+) :: Num a => a -> a -> a
```

```
*Main> :l ch13/numsimple.hs
*Main> Number 5
Number 5
*Main> :t Number 5
Number 5 :: Num a => SymbolicManip a
*Main> :t Number (5::Int)
Number (5::Int) :: SymbolicManip Int
*Main> Number 5 * Number 10
Arith Mul (Number 5) (Number 10)
*Main> (5 * 10)::SymbolicManip Int
Arith Mul (Number 5) (Number 10)
*Main> (5 * 10 + 2)::SymbolicManip Int
Arith Plus (Arith Mul (Number 5) (Number 10)) (Number 2)
```
### Taking advantage of functions as data

```hs
*Main> :l ch13/DList.hs
[1 of 1] Compiling DList            ( ch13/DList.hs, interpreted )
Ok, one module loaded.
*DList> let x = from
fromEnum      fromInteger   fromIntegral  fromList      fromRational
*DList> let x = fromList [1, 2]
*DList> let y = fromList [
*DList> toList $ append' x y
[1,2]
```
