# 08 Efficient file processing, regular expressions, and file name matching

#### Efficient file processing
- `String`
- `Data.ByteString`
- `Data.ByteString.Lazy`

```
Prelude> putStr =<< readFile "prices.csv"
Date,Open,High,Low,Close,Volume,Adj Close
2008-08-01,20.09,20.12,19.53,19.80,19777000,19.80
2008-06-30,21.12,21.20,20.60,20.66,17173500,20.66
2008-05-30,27.07,27.10,26.63,26.76,17754100,26.76
2008-04-30,27.17,27.78,26.76,27.41,30597400,27.41
Prelude> :t (!!)
(!!) :: [a] -> Int -> a
Prelude> (!!) "ascv" 2
'c'
Prelude> :l HighestClose.hs 
[1 of 1] Compiling Main             ( HighestClose.hs, interpreted )
Ok, modules loaded: Main.
*Main> highestCloseFrom "prices.csv"
Loading package array-0.4.0.1 ... linking ... done.
Loading package deepseq-1.3.0.1 ... linking ... done.
Loading package bytestring-0.10.0.2 ... linking ... done.
Just 2741
*Main> highestClose L.empty
Nothing
```