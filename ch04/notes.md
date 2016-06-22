# 04 Functional Programming

Functions used in this chapter:
- System.Environment (getArgs)
- break, lines, unlines
- Data.List : isPrefixOf, isInfixOf, isSuffixOf
- lenght, null, head, tail, last, init
- ++, concat, reverse, and, or, all f(), any f()
- sublists: take int, drop int, splitAt int, takeWhile f(), dropWhile f(), span f()
- words, unwords

A simple command line framework:
```bash
mabe@ubuntu:~/Tutorial/real_world_haskell/ch04 (master)*$ ghc --make InteractWith.hs
[1 of 1] Compiling Main             ( InteractWith.hs, InteractWith.o )
Linking InteractWith ...
mabe@ubuntu:~/Tutorial/real_world_haskell/ch04 (master)*$ ls
InteractWith  InteractWith.hi  InteractWith.hs  InteractWith.o  notes.md
mabe@ubuntu:~/Tutorial/real_world_haskell/ch04 (master)*$ ./InteractWith notes.md example.txt
mabe@ubuntu:~/Tutorial/real_world_haskell/ch04 (master)*$ ./InteractWith notes.md
error: exactly two arguments needed
```

Warming up: portably splitting lines of text

```bash
mabe@ubuntu:~/Tutorial/real_world_haskell/ch04 (master)*$ ghci
Prelude> :l SplitLines.hs 
[1 of 1] Compiling Main             ( SplitLines.hs, interpreted )
Ok, modules loaded: Main.
*Main> let ab ="unodue\ntrequattro\rcinquesei\r\nsetteotto"
*Main> splitLines ab
["unodue","trequattro","cinquesei","setteotto"]
```

Partial and total functions:
```bash
ghci> :type all
all :: (a -> Bool) -> [a] -> Bool
ghci> all odd [1,3,5]
True
ghci> all odd [3,1,4,1,5,9,2,6,5]
False
ghci> all odd []
True
ghci> :type any
any :: (a -> Bool) -> [a] -> Bool
ghci> any even [3,1,4,1,5,9,2,6,5]
True
ghci> any even []
False
```
How to think about loops:
```bash
*IntParse> asInt "123"
123
*IntParse> asInt ""
0
*IntParse> asInt "a"
10
```
```C
void square(double *out, const double *in, size_t length)
{
    for (size_t i = 0; i < length; i++) {
  out[i] = in[i] * in[i];
    }
}
```
```hs
square::[Double] -> [Double]
square [] = []
square (x:xs) = x*x : square xs 
```