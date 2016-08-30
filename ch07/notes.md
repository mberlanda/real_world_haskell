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

#### Working With Files and Handles
```
$ runghc toupper-imp.hs
ghci> :module System.IO
ghci> :type openFile
openFile :: FilePath -> IOMode -> IO Handle
```

Possible IOMode Values:

|IOMode|Can read?|Can write?|Starting position|Notes|
|------|---------|----------|-----------------|-----|
|ReadMode|Yes|No|Beginning of file|File must exist already|
|WriteMode|No|Yes|Beginning of file|File is truncated (completely emptied) if it already existed|
|ReadWriteMode|Yes|Yes|Beginning of file|File is created if it didn't exist; otherwise, existing data is left intact|
|AppendMode|No|Yes|End of file|File is created if it didn't exist; otherwise, existing data is left intact|

Some common functions:
- _hClose_ : close the file 
- _hTell_ : takes a Handle and returns an IO Integer with your position
- _hSeek_ : takes three parameters (a Handle, a SeekMode, and a position)
- _hIsSeekable_ : not every Handle is seekable 

SeekMode:
- _AbsoluteSeek_ : position is a precise location in the file
- _RelativeSeek_ : seek from the current position
- _SeekFromEnd_  : seek to the specified number of bytes before the end of the file

```hs
hSeek handle SeekFromEnd 0
```

Standard Input(_stdin_), Output(_stdout_), and Error(_stderr_)
```hs
getLine = hGetLine stdin
putStrLn = hPutStrLn stdout
print = hPrint stdout
```
```
$ echo Mauro | runghc callingpure.hs 
Greetings once again. What is your name?
Pleased to meet you, Mauro.
Your name countains 5 characters.
```
Deleting and Renaming Files(_System.Directory_):
- _removeFile_
- _renameFile_

Temporary Files
- _openTempFile_ and _openBinaryTempFile_ take two parameters: the directory in which to create the file, and a file name base for naming the file
- _System.Directory.getTemporaryDirectory_ reveals the best location for tmp files (e.g. '.' is the current directory)
- The return type of openTempFile is IO (FilePath, Handle). The first part of the tuple is the name of the file created, and the second is a Handle opened in ReadWriteMode over that file
- once used the tmp file, _hClose_ and _removeFile_

#### Extended Example: Functional I/O and Temporary Files
```
mabe@ubuntu:~/Tutorial/real_world_haskell/ch07 (master)*$ runhaskell tempfile.hsWelcome to tempfile.hs
I have a temporary file at /tmp/mytemp9807.txt
My initial position is 0
Writing one line containing 22 bytes: [1,2,3,4,5,6,7,8,9,10]
After writing, my new position is 23
The file content is: 
[1,2,3,4,5,6,7,8,9,10]

Which could be expressed as this Haskell literal:
"[1,2,3,4,5,6,7,8,9,10]\n"
```

#### Lazy I/O
- _hGetContents_
- _readFile_ and _writeFile_
> `putStr` (and all the similar output functions) write out data as it becomes available. They also have no need for keeping around data already written, so as long as nothing else in the program needs it, the memory can be freed immediately. In a sense, you can think of the String between `readFile` and `writeFile` as a pipe linking the two. Data goes in one end, is transformed some way, and flows back out the other.
- _interact_ `$ runghc toupper-lazy4.hs < "input.txt" > "output.txt"` (e.g. this can be used to filter rows)