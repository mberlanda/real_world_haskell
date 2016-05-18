-- file: ch03/ALgebraicVector.hs
-- x and y coordinates or lengths.
data Cartesian2D = Cartesian2D Double Double
                   deriving (Eq, Show)
data Polar2D = Polar2D Double Double
               deriving (Eq, Show)