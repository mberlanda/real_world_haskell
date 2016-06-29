# 05 Writing a library: working with JSON data

Compiling Haskell source:
```bash
ghc -c SimpleJSON.hs
```
> The -c option tells ghc to only generate object code. If we were to omit the -c option, the compiler would attempt to generate a complete executable. That would fail, because we haven't written a main function, which GHC calls to start the execution of a standalone program

Generating a Haskell program, and importing modules:
```bash
ghc -o simple Main.hs SimpleJSON.o
```
> We pass ghc a new option, -o, which takes one argument: this is the name of the executable that ghc should create. Here, we've decided to name the program simple. On Windows, the program will have the suffix .exe, but on Unix variants there will not be a suffix.

Fleshing out the pretty printing library:
```bash
Prelude> :l Prettify.hs 
[1 of 1] Compiling Main             ( Prettify.hs, interpreted )
Ok, modules loaded: Main.
*Main> text "foo" <> text "bar"
Concat (Text "foo") (Text "bar")
*Main> empty  <> text "bar"
Text "bar"
*Main> text "foo"  </> text "bar"
Concat (Concat (Text "foo") (Union (Char ' ') Line)) (Text "bar")
```