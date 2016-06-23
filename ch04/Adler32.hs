-- file: ch04/Adler32.hs

module Adler32
where

  import Data.Char (ord)
  import Data.Bits (shiftL, (.&.), (.|.))

  base = 65521

  adler32 :: [Char] -> Int
  adler32 xs = helper 1 0 xs
    where helper a b (x:xs) = let a' = (a + (ord x .&. 0xff)) `mod` base
                                  b' = (a' + b) `mod` base
                              in helper a' b' xs
          helper a b _      = (b `shiftL` 16) .|. a

  adler32_try2 :: [Char] -> Int
  adler32_try2 xs = helper (1,0) xs
    where helper (a,b) (x:xs) =
              let a' = (a + (ord x .&. 0xff) ) `mod` base
                  b' = (a' + b ) `mod` base
              in helper (a', b') xs
          helper (a, b) _     = (shiftL b 16) .|. a

  adler32_foldl :: [Char] -> Int
  adler32_foldl xs = let (a, b) = foldl step (1,0) xs
                     in (shiftL b 16) .|. a
      where step (a,b) x = let a' = a + (ord x .&. 0xff)
                           in (a' `mod` base, (a' + b) `mod` base)

  my_adler32_foldl :: [Char] -> Int
  my_adler32_foldl xs = let [a, b] = foldl step [1,0] xs
                     in (shiftL b 16) .|. a
      where step [a,b] x = let a' = a + (ord x .&. 0xff)
                           in map (`mod` base) [a', (a' + b)]