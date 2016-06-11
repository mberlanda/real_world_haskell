# 04 Functional Programming

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

