import Control.Arrow
import Control.Monad
import Data.List
import Data.List.Split
import qualified Data.Set as S
import Data.Equivalence.Monad

type Pos = (Int, Int, Int)

sqDist :: Pos -> Pos -> Int
sqDist (x1, y1, z1) (x2, y2, z2) = sum . map (^2) $ zipWith subtract [x1,y1,z1] [x2,y2,z2]

parsePos :: String -> Pos
parsePos s = (x, y, z)
  where [x, y, z] = map read $ splitOn "," s

{-
  Given a list `[x1, x2, ...]` compute the list `[(x_i, x_j) | i < j]`.
-}
pairs :: [a] -> [(a,a)]
pairs [] = []
pairs (x : xs) = [(x,y) | y <- xs] ++ pairs xs

main1 = 
  do ps <- fmap (map parsePos . lines) getContents
     let es = map fst . sortOn snd . map (id &&& uncurry sqDist) $ pairs ps
     let cls = runEquivM S.singleton S.union $ 
                 forM_ (take 1000 es) (uncurry equate) >> classes >>= mapM desc
     print . product . take 3 . sortBy (flip compare) . map S.size $ cls

{-
  Given a graph with `n` nodes and a list of weighted undirected edges, compute
  returns the list of edges in a minimum-weight spanning forest of the graph.
  If the graph is connected: aborts early once all edges have been found.
-}
kruskal :: (Ord a, Ord b) => Int -> [((a,a), b)] -> [(a,a)]
kruskal nNodes es = runEquivM' $ go nNodes (map fst (sortOn snd es))
  where go n es | n <= 1 || null es = return []
        go n (e@(u,v) : es) =
          do b <- equivalent u v
             if b then go n es else equate u v >> fmap (e:) (go (n-1) es)

main2 = 
  do ps <- fmap (map parsePos . lines) getContents
     let es = map (id &&& uncurry sqDist) (pairs ps)
     let ((x1, _, _), (x2, _, _)) = last (kruskal (length ps) es)
     print $ x1 * x2

main = main2

