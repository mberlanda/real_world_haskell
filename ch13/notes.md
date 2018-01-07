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
