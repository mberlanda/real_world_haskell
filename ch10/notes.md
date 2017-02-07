# 10 Code Case Study: Parsing a Binary Data Format

### Getting Rid of Boilerplate Code
```hs
(>>?) :: Maybe a -> (a -> Maybe b) -> Maybe b
Nothing >>? _ = Nothing
Just v >>? f = f v

skipSpace :: (a, L.ByteString) -> Maybe (a, L.ByteString)
skipSpace (a, s) = Just (a, L8.dropWhile isSpace s)
```

> The key to understanding this function is to think about the chaining. On the left side of each `(>>?)` is a `Maybe` value; on the right is a function that returns a `Maybe` value. Each left-and-right-side expression is thus of type `Maybe` , suitable for passing to the following `(>>?)` expression.
> The other change that we’ve made to improve readability is add a `skipSpace` function. With these changes, we’ve halved the number of lines of code compared to our original parsing function.

### Implicit State

```
# The identity parser
Prelude> :l ch10/Parse.hs 
[1 of 1] Compiling Parse            ( ch10/Parse.hs, interpreted )
Ok, modules loaded: Parse.
*Parse> :type parse
parse :: Parse a -> L.ByteString -> Either String a
*Parse> :type parse (identity 1) undefined
parse (identity 1) undefined :: Num a => Either String a
*Parse> parse (identity 1) undefined
Right 1
*Parse> parse (identity "foo") undefined
Right "foo"
# Record syntax, updates, and pattern matching
Prelude> :cd ch10/
Prelude> :l Parse.hs 
[1 of 2] Compiling PNM              ( PNM.hs, interpreted )
[2 of 2] Compiling Parse            ( Parse.hs, interpreted )
Ok, modules loaded: Parse, PNM.
*Parse> let before = ParseState (L8.pack "foo") 0
*Parse> before
ParseState {string = "foo", offset = 0}
*Parse> let after = modifyOffset before 3
*Parse> after
ParseState {string = "foo", offset = 3}

*PNM> L8.uncons (L8.pack "foo")
Just ('f',"oo")
*PNM> L8.uncons L8.empty
Nothing
```
