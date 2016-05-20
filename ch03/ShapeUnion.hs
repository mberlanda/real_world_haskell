-- file: ch03/ShapeUnion.hs
-- The discriminated union
type Vector = (Double, Double)
data Shape = Circle Vector Double
           | Poly [Vector]