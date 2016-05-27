-- file: ch03/Direction.hs
module GrahamScan where

  import Data.List

-- write a function that calculates the turn made bt three two-dimensional points
  data Point2D = Point2D {
                  x :: Float,
                  y :: Float
                  }
    deriving(Show, Eq, Ord)

  data Direction = CounterClockWise | ClockWise | Collinear
    deriving(Show, Eq)


  -- cross product of two vectors as per Graham Scan algorithm (https://en.wikipedia.org/wiki/Graham_scan)
  ccw :: Point2D -> Point2D -> Point2D -> Float
  ccw p1 p2 p3 = (x p2 - x p1 ) * (y p3 - y p1) - (y p2 - y p1) * (x p3 - x p1)

  getDir :: Point2D -> Point2D -> Point2D -> Direction
  getDir a b c 
    | ccw a b c < 0 = GrahamScan.ClockWise
    | ccw a b c > 0 = GrahamScan.CounterClockWise
    | otherwise     = GrahamScan.Collinear   

  lstDir :: [Point2D] -> [Direction]
  lstDir [] = []
  lstDir (_ : []) = []
  lstDir (_ : _ : []) = []
  lstDir (a:b:c:xs) = (getDir a b c) : lstDir (b:c:xs)

  p1 = Point2D { x= 0, y=5 }
  p2 = Point2D { x= 0, y=0 }
  p3 = Point2D { x= 5, y=0 }
  p4 = Point2D { x= 5, y=5 }
  p5 = Point2D { x= 10, y=5 }

  -- convex hull of a set X of points in the Euclidean plane or Euclidean space is the smallest convex set that contains X.
  -- https://en.wikipedia.org/wiki/Convex_hull
  --  convexHull :: [Point2D] -> [Direction]
  onYX :: Point2D -> Point2D -> Ordering
  onYX p1 p2
    | x p1 > x p2                 = GT
    | x p1 == x p2 && y p1 > y p2 = GT
    | x p1 == x p2 && y p1 < y p2 = LT
    | x p1 < x p2                 = LT

  sortByAngle :: [Point2D] -> [Point2D]
  sortByAngle pts = (sorted pts) ++ sentinel
    where sorted pts = sortBy onYX pts
          sentinel = [head (sorted pts)]

  grahamScan :: [Point2D] -> [Point2D]
  grahamScan [] = [] 
  grahamScan xs = scan $ sorted
    where scan (p1:p2:p3:pts) =
            if (getDir p1 p2 p3) == GrahamScan.CounterClockWise 
            then p1:scan(p3:pts)
            else p1:scan(p2:p3:pts)
          scan [p1, p2] = [p1]
          scan _ = []
          sorted = sortByAngle xs

  -- Sample 1
  s1 = [ (Point2D 1 1), (Point2D 2 2), (Point2D 3 3), (Point2D 1 3)]
