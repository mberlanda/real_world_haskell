-- file ch03/Nullable.hs
-- Parameterized Types
data Maybe a = Just a
             | Nothing
             deriving (Show)

someBool = Main.Just True
someString = Main.Just "something"