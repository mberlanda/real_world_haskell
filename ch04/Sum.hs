-- file: ch04/Sum.hs
module Sum
where
  
  mySum :: Num a => [a] -> a
  mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _      = acc

  foldSum :: Num a => [a] -> a
  foldSum xs = foldl step 0 xs
    where step acc x = acc + x

  niceSum :: Num a => [a] -> a
  niceSum xs = foldl (+) 0 xs