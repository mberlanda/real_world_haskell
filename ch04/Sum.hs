-- file: ch04/Sum.hs
module Sum
where
  
  mySum :: Num a => [a] -> a
  mySum xs = helper 0 xs
    where helper acc (x:xs) = helper (acc + x) xs
          helper acc _      = acc