# 09 I/O Case Study: A Library for Searching the Filesystem
```
GHCi, version 7.6.3: http://www.haskell.org/ghc/  :? for help
Loading package ghc-prim ... linking ... done.
Loading package integer-gmp ... linking ... done.
Loading package base ... linking ... done.
Prelude> :m +System.FilePath
Prelude System.FilePath> takeExtension "foo.va"
Loading package filepath-1.3.0.1 ... linking ... done.
".va"
Prelude System.FilePath> :cd ch09
Prelude System.FilePath> :l SimpleFinder.hs 
[1 of 2] Compiling RecursiveContents ( RecursiveContents.hs, interpreted )
[2 of 2] Compiling Main             ( SimpleFinder.hs, interpreted )
Ok, modules loaded: RecursiveContents, Main.
*Main System.FilePath> simpleFind (\p -> takeExtension p == ".hs") "./"
Loading package array-0.4.0.1 ... linking ... done.
Loading package deepseq-1.3.0.1 ... linking ... done.
Loading package old-locale-1.0.0.5 ... linking ... done.
Loading package time-1.4.0.1 ... linking ... done.
Loading package bytestring-0.10.0.2 ... linking ... done.
Loading package unix-2.6.0.1 ... linking ... done.
Loading package directory-1.2.0.1 ... linking ... done.
["./SimpleFinder.hs","./RecursiveContents.hs"]
```

Three problems of SimpleFinder implementation:
- predicate is not very expressive
- no control over how it traverses the filesystem (e.g. versioning systems)
- it would be preferred a lazy stream of results

> Some functions used by the book (in particular handle) have a deprecated implementation, so it seems more reasonable to move on and eventually come back to this example after the book is finished.