# 01 Getting Started

> GHC has three main components.
>   *ghc* is an optimizing compiler that generates fast native code.
>   *ghci* is an interactive interpreter and debugger.
>   *runghc* is a program for running Haskell programs as scripts, without needing to compile them first.

You can change some *ghci* default behaviors:
```bash
ghci> :set +t
ghci> 'c'
'c'
it :: Char
ghci> "foo"
"foo"
it :: [Char]
```

Run the first simple program
```bash
$ runghc WC < quux.txt
```

Exercises
```bash
ghci> :?
ghci> let x = 1
ghci> :show bindings
x :: Integer = 1
```