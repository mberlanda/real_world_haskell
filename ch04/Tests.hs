-- file: ch04/Tests.hs

module Tests where

import Testing
import Fold
import Sum
import Adler32

-- testFunction
testFunction :: Eq a => (a, a) -> Bool
testFunction (f, g) = f == g

-- test niceSum code writing the recursion step by step
niceSumTest :: [Test]
niceSumTest = [Test "niceSum test" testFunction
                [( niceSum [1..3], myFoldl (+) (0+1) (2:3:[]) ),
                 ( niceSum [1..3], myFoldl (+) (0+1+2) (3:[]) ),
                 ( niceSum [1..3], myFoldl (+) (0+1+2+3) [] ),
                 ( niceSum [1..3], (((0+1)+2)+3) )
                ]
              ]

-- test all the different versions of Adler32
adler32Test :: [Test]
adler32Test = [Test "adler32_try2 test" testFunction
                [ (adler32 "test", adler32_try2 "test")],
               Test "adler32_foldl test" testFunction
                [ (adler32 "test", adler32_foldl "test")],
               Test "my_adler32_foldl test" testFunction
                [ (adler32 "test", my_adler32_foldl "test")]
              ]