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