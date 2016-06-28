# 05 Writing a library: working with JSON data

Compiling Haskell source:
```bash
ghc -c SimpleJSON.hs
```
> The -c option tells ghc to only generate object code. If we were to omit the -c option, the compiler would attempt to generate a complete executable. That would fail, because we haven't written a main function, which GHC calls to start the execution of a standalone program