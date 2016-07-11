# 06 Using Typeclasses

The need for typeclasses:
```
Prelude> :cd ch06
Prelude> :l naiveeq.hs 
[1 of 1] Compiling Main             ( naiveeq.hs, interpreted )
Ok, modules loaded: Main.
*Main> colorEq Red Red
True
*Main> colorEq Blue Red
False
```