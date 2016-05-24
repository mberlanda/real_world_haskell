--file ch03/Braces.hs
-- the Offside Rule is not Mandatory
bar = let a = 1
          b = 2
          c = 3
      in a + b + c

foo = let { a = 1; b = 2;
        c = 3 }
      in a + b + c