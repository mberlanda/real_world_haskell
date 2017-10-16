# 12 Barcode Recognition


### Barcode Digits (EAN-13)

Section      | Length | Description
-------------|--------|------------
Number system | 2 |The first two digits. This can either indicate the nationality of the manufacturer or describe one of a few other categories, such as ISBN (book identifier) numbers.
Manufacturer ID | 5 | The next five digits. These are assigned by a country’s numbering authority.
Product ID | 5 | The next five digits. These are assigned by the manufacturer. (Smaller manufacturers may have a longer manufacturer ID and shorter product ID, but they still add up to 10 digits.)
Check digit | 1 | The last digit. This allows a scanner to validate the digit string it scans.

### Introducing Arrays

This implementation uses ch10's modules `PNM` and `Parse`.

> The barcode encoding process can largely be table-driven, in which we use small tables of bit patterns to decide how to encode each digit. Haskell’s bread-and-butter-data types, lists, and tuples are not well-suited to use for tables whose elements may be accessed randomly. A list has to be traversed linearly to reach the kth element. A tuple doesn’t have this problem, but Haskell’s type system makes it difficult to write a function that takes a tuple and an element offset and returns the element at that offset within the tuple.

```
*Main> :t leftOddList
leftOddList :: [[Char]]
*Main> leftOddList
["0001101","0011001","0010011","0111101","0100011","0110001","0101111","0111011","0110111","0001011"]
*Main> :t listArray
listArray :: Ix i => (i, i) -> [e] -> Array i e
*Main> listArray (0,2) leftOddList
array (0,2) [(0,"0001101"),(1,"0011001"),(2,"0010011")]

*Main> let a = listArray (0,14) ['a'..]
*Main> a ! 4
'e'

ghci> let a = listArray (-9,5) ['a'..]
ghci> a ! (-2)
'h'

ghci> let a = listArray ('a', 'h') [97..]
ghci> a ! 'e'
101

-- Arrays and Laziness

*Main> let a = listArray (0,5) "bar"
*Main> a ! 2
'r'
*Main> a ! 5
*** Exception: (Array.!): undefined array element

```

