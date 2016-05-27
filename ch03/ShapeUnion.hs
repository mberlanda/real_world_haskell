-- file: ch03/ShapeUnion.hs
-- The discriminated union
type Vector = (Double, Double)
data Shape = Circle Vector Double
           | Poly [Vector]


data Point2D = Point2D (Float, Float)
  deriving(Show)
data Direction = Left | Right | Straight
  deriving(Show)

pitHyp :: Floating a => a -> a -> a
pitHyp cat1 cat2 = sqrt ( (cat1 ^ 2) + (cat2 ^ 2) ) 

pitCat :: Floating a => a -> a -> a
pitCat hyp cat = sqrt   ( (hyp ^ 2) - (cat ^ 2) )

segm :: Point2D -> Point2D -> Float
segm (Point2D (x1, y1)) (Point2D (x2, y2)) = pitHyp (x1-x2) (y1-y2)

rad2deg :: Floating a => a -> a
rad2deg x = (x * 180) / pi

prodLst :: Floating a => [a] -> a
prodLst [] = 1
prodLst (x:xs) = x * (prodLst xs)

-- cos alfa -> (b^2 + c^2 - a^2) / 2bc 
carnot :: Floating a => a -> a -> a -> a
carnot a b c  = rad2deg (acos  rad)
                where d   = (b ^2) + (c ^2) - (a ^2)
                      adj = 2 * b * c
                      rad = d / adj

angle :: Point2D -> Point2D -> Point2D -> Float
angle a b c = let ab = segm a b
                  bc = segm b c
                  ca = segm c a
              in carnot ca ab bc

getDirection :: Point2D -> Point2D -> Point2D -> Direction
getDirection a b c
    | (slope a b) == (slope b c) = Direction.Straight
    | (slope a b) > (slope b c)  = Direction.Left
    | (slope a b) < (slope b c)  = Direction.Right
  where slope (Point2D (x1, y1)) (Point2D (x2, y2)) = (y2 - y1) / (x2 - x1)

p1 = Point2D (0, 5)
p2 = Point2D (0, 0)
p3 = Point2D (5, 0)