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
Compact rendering:
```bash
Prelude SimpleJSON> :l PrettyJSON.hs 
[2 of 3] Compiling Prettify         ( Prettify.hs, interpreted )
[3 of 3] Compiling PrettyJSON       ( PrettyJSON.hs, interpreted )
Ok, modules loaded: PrettyJSON, SimpleJSON, Prettify.
*PrettyJSON> let value = renderJValue (JObject [("f", JNumber 1), ("q", JBool True)])
*PrettyJSON> :t value
value :: Doc
*PrettyJSON> putStrLn (compact value)
{"f": 1.0,
"q": true
}
```
Exercises:
```bash
*PrettyJSON> let value = renderJValue (JObject [("foo", (JObject [("baz", JNumber 123)])), ("bar", JNumber 456)])
*PrettyJSON> putStrLn (pretty 10 (Prettify.nest 4 value))

{
    "foo": 
    {
        "baz": 123.0
        
    },
    "bar": 456.0
    
}
*PrettyJSON> 
```

Creating a package:
- write a package description into a .cabal file
- create a Setup.hs file
- configuration: e.g.
```bash
$ runghc Setup configure
# Cabal will install our package in the system-wide package database
$ runghc Setup configure --prefix=$HOME --user
# Cabal will install our in the home directory and in the personal package database

~/real_world_haskell/ch05 (master) $ runghc Setup configure --prefix=$HOME --user
Configuring mypretty-0.1...
```
- build the package
```bash
$ runghc Setup build
~/real_world_haskell/ch05 (master) $ runghc Setup build
Building mypretty-0.1...
Preprocessing library mypretty-0.1...
[1 of 3] Compiling SimpleJSON       ( SimpleJSON.hs, dist/build/SimpleJSON.o )
[2 of 3] Compiling Prettify         ( Prettify.hs, dist/build/Prettify.o )
[3 of 3] Compiling PrettyJSON       ( PrettyJSON.hs, dist/build/PrettyJSON.o )
In-place registering mypretty-0.1...
```
- install the package
```bash
$ runghc Setup install
~/real_world_haskell/ch05 (master) $ runghc Setup install
Installing library in /home/mabe/lib/mypretty-0.1/ghc-7.6.3
Registering mypretty-0.1..
```