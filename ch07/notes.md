# 07 I/O

#### Classic I/O in Haskell
```
$ runghc basicio.hs
ghci> :type putStrLn
putStrLn :: String -> IO ()
ghci> :type getLine
getLine :: IO String
```

| Pure | Impure |
|------|--------|
|Always produces the same result when given the same parameters | May produce different results for the same parameters|
|Never has side effects | May have side effects|
|Never alters state | May alter the global state of the program, system, or world|