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

#### Regular expressions in Haskell

```
Prelude> :module +Text.Regex.Posix
Prelude Text.Regex.Posix> :t (=~) 
(=~)
  :: (RegexMaker Regex CompOption ExecOption source,
      RegexContext Regex source1 target) =>
     source1 -> source -> target

Prelude Text.Regex.Posix> "my left foot" =~ "foo" :: Bool
Loading package array-0.4.0.1 ... linking ... done.
Loading package deepseq-1.3.0.1 ... linking ... done.
Loading package bytestring-0.10.0.2 ... linking ... done.
Loading package containers-0.5.0.0 ... linking ... done.
Loading package transformers-0.3.0.0 ... linking ... done.
Loading package mtl-2.1.2 ... linking ... done.
Loading package regex-base-0.93.2 ... linking ... done.
Loading package regex-posix-0.95.2 ... linking ... done.
True
Prelude Text.Regex.Posix> "your right hand" =~ "(hand|foot)" :: Bool
True
Prelude Text.Regex.Posix> "a star called henry" =~ "planet" :: Int
0
Prelude Text.Regex.Posix> "honorificabilitudinitatibus" =~ "[aeiou]" :: Int
13
Prelude Text.Regex.Posix> "I, B. Ionsonii, uurit a lift'd batch" =~ "(uu|ii)" :: String
"ii"
Prelude Text.Regex.Posix> let pat = "(foo[a-z]*bar|quux)"
Prelude Text.Regex.Posix> "before foodiebar after" =~ pat :: (String,String,String)
("before ","foodiebar"," after")
Prelude Text.Regex.Posix> "eleemosynary" =~ pat :: (Int,Int)
(-1,0)

Prelude Text.Regex.Posix> :module +Data.ByteString.Char8
Prelude Text.Regex.Posix Data.ByteString.Char8> pack "good food" =~ ".ood" :: ByteString
"good"
Prelude Text.Regex.Posix Data.ByteString.Char8> pack "good food" =~ ".ood" :: Int
2
```

#### Translating a glob pattern into a regular expression
```
Prelude> :l ch08/GlobRegex.hs 
[1 of 1] Compiling GlobRegex        ( ch08/GlobRegex.hs, interpreted )
Ok, modules loaded: GlobRegex.
*GlobRegex> globToRegex "f??.c"
Loading package array-0.4.0.1 ... linking ... done.
Loading package deepseq-1.3.0.1 ... linking ... done.
Loading package bytestring-0.10.0.2 ... linking ... done.
Loading package containers-0.5.0.0 ... linking ... done.
Loading package transformers-0.3.0.0 ... linking ... done.
Loading package mtl-2.1.2 ... linking ... done.
Loading package regex-base-0.93.2 ... linking ... done.
Loading package regex-posix-0.95.2 ... linking ... done.
"^f??\\.c$"
*GlobRegex> "test.c" =~ globToRegex "t[ea]s*" :: Bool
True
*GlobRegex> "test.c" =~ globToRegex "t[!ea]s*" :: Bool
False
*GlobRegex> matchesGlob "/home/mabe/Tutorial/real_world_haskell/ch08/GlobRegex.hs" "/home/mabe/*"
True
```