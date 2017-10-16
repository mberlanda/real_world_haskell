-- file: ch12/exercises.hs

{-
Write a function that takes two arguments: a four-element tuple and an integer.
With an integer argument of zero, it should return the leftmost element of the
tuple. With an argument of one, it should return the next element. And so on.
What restrictions do you have to put on the types of the arguments in order to
write a function that typechecks correctly?

func :: [a] -> Int -> a
func xs y = reverse $ xs ! y

-}

ex1 :: (a, a, a, a) -> Integer -> a
ex1 (a, _, _, _) 0 = a
ex1 (_, a, _, _) 1 = a
ex1 (_, _, a, _) 2 = a
ex1 (_, _, _, a) 3 = a
ex1 _ _ = error "index out of bounds"

{-
Write a similar function that takes a six-tuple as its first argument.
-}
ex2 :: (a, a, a, a, a, a) -> Integer -> a
ex2 (a0, a1, a2, a3, a4, a5) n
  | n < 4     = ex1 (a0, a1, a2, a3) n
  | n == 4    = a4
  | n == 5    = a5
  | otherwise = error "index out of bounds"

{-
Try refactoring the two functions to share any common code you can identify.
How much shared code are you able to find?
-}

