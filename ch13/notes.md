# 13 Data Structures

### Association Lists

The two most common ways to handle data that is unordered but is indexed by a key, are:

*association lists*

A normal list containing (key, value) tuples. E.g. `[(Integer, String)]`

```hs
Prelude> let al = [(1, "one"), (2, "two")]
Prelude> lookup 1 al
Just "one"
Prelude> lookup 5 al
Nothing
Prelude> :t lookup
lookup :: Eq a => a -> [(a, b)] -> Maybe b
```


*`Map`* type provided by `Data.Map`
