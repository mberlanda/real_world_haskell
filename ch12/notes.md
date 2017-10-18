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

### Encoding an EAN-13 Barcode

> It’s useful to have an encoder for reference. This will allow us to, for example, ensure that our code is correct by checking that the output of `decode . encode` is the same as its input

### Constraints on Our Decoder

In this book we don't process images since they tend to be out of focus, noisy, low in contrast, and of poor resolution.

### Divide and Conquer

- Convert color data into a form we can easily work with.
- Sample a single scan line from the image and extract a set of guesses as to what the encoded digits in this line could be.
- From the guesses, create a list of valid decodings.

### Turning a Color Image into Something Tractable

- Parsing a Color Image
- Grayscale Conversion: we need to convert the color data into monochrome
- Grayscale to Binary and Type Safety: convert the grayscale image into a two-valued image, where each pixel is either on or off

### Finding Matching Digits

```
-- Run Length Encoding:
*Main> group [1,1,2,3,3,3,3]
[[1,1],[2],[3,3,3,3]]
*Main> let bits = [0,0,1,1,0,0,1,1,0,0,0,0,0,0,1,1,1,1,0,0,0,0]
*Main> runLength bits
[(2,0),(2,1),(2,0),(2,1),(6,0),(4,1),(4,0)]
*Main> runLengths bits
[2,2,2,2,6,4,4]
-- Scaling Run Lengths, and Finding Approximate Matches
*Main> let group = scaleToOne [2,6,4,4]
*Main> group
[1 % 8,3 % 8,1 % 4,1 % 4]
*Main> distance group (head leftEvenSRL)
13 % 28
*Main> distance group (head leftOddSRL)
17 % 28
*Main> leftOddSRL
[[3 % 7,2 % 7,1 % 7,1 % 7],[2 % 7,2 % 7,2 % 7,1 % 7],[2 % 7,1 % 7,2 % 7,2 % 7],[1 % 7,4 % 7,1 % 7,1 % 7],[1 % 7,1 % 7,3 % 7,2 % 7],[1 % 7,2 % 7,3 % 7,1 % 7],[1 % 7,1 % 7,1 % 7,4 % 7],[1 % 7,3 % 7,1 % 7,2 % 7],[1 % 7,2 % 7,1 % 7,3 % 7],[3 % 7,1 % 7,1 % 7,2 % 7]]
*Main> bestScores leftOddSRL [1, 2, 3]
[(1 % 7,5),(2 % 7,4),(8 % 21,1)]
-- List Comprehensions
*Main> [ (a,b) | a <- [1,2], b <- "abc" ]
[(1,'a'),(1,'b'),(1,'c'),(2,'a'),(2,'b'),(2,'c')]
*Main> [ (a,b) | a <- [1..6], b <- [5..7], even (a + b ^ 2) ] # eval an expression
[(1,5),(1,7),(2,6),(3,5),(3,7),(4,6),(5,5),(5,7),(6,6)]
*Main> let vowel = (`elem` "aeiou")
*Main> [ x | a <- "etaoin", b <- "shrdlu", let x = [a,b], all vowel x ]
["eu","au","ou","iu"]
*Main> [ a | (3,a) <- [(1,'y'),(3,'a'),(5,'p')] ]
"a"
*Main> [ a | (3,a) <- [(1,'y'),(3,'a'),(3,'p')] ]
"ap"

```

### Remembering a Match's Parity

- For each match in the left group, we have to remember whether we found it in the even parity table or the odd table

```
*Main> let p1 = Even 2
*Main> let p2 = Odd 3
*Main> fromParity p1
2
*Main> fromParity p2
3
*Main> parityMap (*2) p1
Even 4
*Main> parityMap (*3) p2
Odd 9
*Main> fmap (*3) p2
Odd 9
```
- sort parity-encoded values based on the values they contain
```
-- type signature needed
-- compareWithoutParity :: Ord a => Parity a -> Parity a -> Ordering
*Main> let p1 = Even 2
*Main> let p2 = Odd 3
*Main> compareWithoutParity p1 p2
LT
*Main> compareWithoutParity p2 p1
GT
*Main> compareWithoutParity (Even 'a') (Odd 'b')
LT

*Main> bestLeft [0,0,0,0,1,1,1,0,1,1,1,1,0]
[Odd (1 % 1,0),Odd (1 % 1,0),Odd (1 % 1,0),Even (1 % 1,0),Even (1 % 1,0),Even (1 % 1,0)]
*Main> bestRight [0,0,0,0,1,1,1,0,1,1,1,1,0]
[None (1 % 1,0),None (1 % 1,0),None (1 % 1,0)]

-- Another kind of laziness, of the keyboarding variety
*Main> show $ Even 1
"Even 1"
*Main> show $ AltEven 1
"AltEven {fromAltParity = 1}"
*Main> length . show $ Even 1
6
*Main> length . show $ AltEven 1
27

-- Chunking a List
*Main> chunksOf 3 [1,2,3,4,5,6]
[[1,2,3],[4,5,6]]

-- Generating a List of Candidate Digits
*Main> let input = zip (runLengths $ encodeEAN13 "978013211467") (cycle [Zero, One])
*Main> :t input
input :: [(Run, Bit)]
*Main> candidateDigits input
[[Odd 0,Odd 1,Odd 2,Even 0,Even 0,Even 0],[Even 0,Even 1,Even 2,Odd 0,Odd 0,Odd 0],[Even 0,Even 1,Even 2,Odd 0,Odd 0,Odd 0],[Odd 0,Odd 1,Odd 2,Even 0,Even 0,Even 0],[Even 0,Even 1,Even 2,Odd 0,Odd 0,Odd 1],[Odd 0,Odd 0,Even 0,Even 0,Odd 1,Even 1],[None 0,None 1,None 2],[None 0,None 1,None 2],[None 0,None 1,None 2],[None 0,None 1,None 2],[None 0,None 1,None 2],[None 0,None 1,None 2]]
*Main> mapM_ print $ candidateDigits input
[Odd 0,Odd 1,Odd 2,Even 0,Even 0,Even 0]
[Even 0,Even 1,Even 2,Odd 0,Odd 0,Odd 0]
[Even 0,Even 1,Even 2,Odd 0,Odd 0,Odd 0]
[Odd 0,Odd 1,Odd 2,Even 0,Even 0,Even 0]
[Even 0,Even 1,Even 2,Odd 0,Odd 0,Odd 1]
[Odd 0,Odd 0,Even 0,Even 0,Odd 1,Even 1]
[None 0,None 1,None 2]
[None 0,None 1,None 2]
[None 0,None 1,None 2]
[None 0,None 1,None 2]
[None 0,None 1,None 2]
[None 0,None 1,None 2]

```

