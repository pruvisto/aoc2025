import Data.List
import Data.List.Split
import Data.Either
import Data.Ord

type Point = (Int, Int)

bimap :: (a -> b) -> (a,a) -> (b,b)
bimap f (x, y) = (f x, f y)

pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x : xs) = [(x,y) | y <- xs] ++ pairs xs


parsePoint :: String -> Point
parsePoint s = (x, y)
  where [x, y] = map read $ splitOn "," s

area :: Point -> Point -> Int
area (x1, y1) (x2, y2) = (abs (x2 - x1) + 1) * (abs (y2 - y1) + 1)

main1 =
  do ps <- fmap (map parsePoint . lines) getContents
     print . maximum $ [area p q | (p, q) <- pairs ps]


{-
  We represent our polygon as a collection of horizontal and vertical edges.
  Horizontal edges are represented as `(left, right, y-coord)` and vertical
  ones as `(x-coord, bottom, top)`.
-}
data HEdge = HEdge (Int, Int, Int)
data VEdge = VEdge (Int, Int, Int)
data Polygon = Polygon [HEdge] [VEdge]

{-
  Construct a polygon from a list of successive vertices. Assumes that the polgon is not self-intersecting.
-}
mkPolygon :: [Point] -> Polygon
mkPolygon ps = uncurry Polygon . partitionEithers $ zipWith mkEdge ps (tail ps ++ [head ps])
  where mkEdge (x1, y1) (x2, y2) =
          if y1 == y2 then Left (HEdge (min x1 x2, max x1 x2, y1)) else Right (VEdge (x1, min y1 y2, max y1 y2))

{-
  Determine if two polygons intersect, in the sense that any of their edges intersect.
-}
intersects :: Polygon -> Polygon -> Bool
intersects (Polygon hs1 vs1) (Polygon hs2 vs2) = or $ [aux h v | h <- hs1, v <- vs2] ++ [aux h v | h <- hs2, v <- vs1]
  where aux (HEdge (l, r, y)) (VEdge (x, b, t)) = l <= x && x <= r && b <= y && y <= t

{-
  Constructs a rectangle as a polygon given two opposite corners. To make our lives a bit easier w.r.t. "improper"
  intersections (e.g. edges that partially lie on top of each other) we simply shrink the rectangle by some 
  sufficiently small ε > 0.
  
  This still gives the correct result: every rectangle that is fully within our polygon is still fully inside after
  shrinking by any amount, and every rectangle that protrudes from the polygon does so by at least a 1x1 box, so
  after shrinking the rectangle by ε it still protrudes by at least a (1-2ε)x(1-2ε) box. We pick ε = 1/4.
  
  To avoid rational arithmetic, we implement this by multiplying all the coordinates by 4 first.
-}
rectangle :: Point -> Point -> Polygon
rectangle (x1, y1) (x2, y2) = Polygon [HEdge (l,r,y) | y <- [b,t]] [VEdge (x,b,t) | x <- [l,r]]
  where (l, r) = (4 * min x1 x2 + 1, 4 * max x1 x2 - 1)
        (b, t) = (4 * min y1 y2 + 1, 4 * max y1 y2 - 1)

main2 =
  do ps <- fmap (map parsePoint . lines) getContents
     let polygon = mkPolygon (map (bimap (*4)) ps)
     let admissible p q = not (intersects (rectangle p q) polygon)
     print . snd . maximumBy (comparing snd) $ [((p, q), area p q) | (p, q) <- pairs ps, admissible p q]

main = main2

