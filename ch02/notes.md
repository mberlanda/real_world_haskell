# 02 Types and Functions

> There are three interesting aspects to types in Haskell:
>   *strong*: values cannot be automatically coerced from one type to another
>   *static*: the compiler knows the type of every value and expression at compile time, before any code is executed
>   *inferred*: the compiler can automatically deduce the types of almost all expressions in a program

```bash
$ ghci

# Haskell Source Files, and Writing Simple Functions
Prelude> :l add.hs 
[1 of 1] Compiling Main             ( add.hs, interpreted )
Ok, modules loaded: Main.
*Main> add 3 7 
10

# Just what is a variable, anyway?
Prelude> :l Assign.hs 
[1 of 1] Compiling Main             ( Assign.hs, interpreted )

Assign.hs:3:1:
    Multiple declarations of `x'
    Declared at: Assign.hs:2:1
                 Assign.hs:3:1
Failed, modules loaded: none.

```

Compare myDrop function in Haskell and Python
```hs
myDropX n xs = if n <= 0 || null xs then xs else myDropX (n - 1) (tail xs)
```
```py
def myDrop(n, elts):
    while n > 0 and elts:
        n = n - 1
        elts = elts[1:]
    return elts
```