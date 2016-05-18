# 03 Defining Types, Streamlining Functions

Defining a New Data Type:
```hs
-- file: ch03/BookStore.hs
data BookInfo = Book Int String [String]
                deriving (Show)
```
- _BookInfo_: type constructor
- _Book_: value constructor
- _Int, String, [String]_: components
Usually the value constructor and the type constructor have the same name.