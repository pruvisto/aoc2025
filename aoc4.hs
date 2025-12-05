import Control.Arrow
import Data.List
import qualified Data.Set as S
import Data.Set (Set)

type Pos = (Int, Int)
type Field = Set (Int, Int)

mkField :: [[Char]] -> Field
mkField css = S.fromList [(x, y) | (y, cs) <- zip [0..] css, (x, c) <- zip [0..] cs, c == '@']

neighborVecs :: [Pos]
neighborVecs = [(dx, dy) | dx <- [-1, 0, 1], dy <- [-1, 0, 1], dx /= 0 || dy /= 0]

neighbors :: Pos -> [Pos]
neighbors (x, y) = [(x + dx, y + dy) | (dx, dy) <- neighborVecs]

countNeighbors :: Field -> Pos -> Int
countNeighbors a p = length [p' | p' <- neighbors p, S.member p' a]

frees :: Field -> [Pos]
frees a = [p | p <- S.toList a, countNeighbors a p < 4]

freeAll :: Field -> (Field, Int)
freeAll a = if null ps then (a, 0) else second (+ length ps) (freeAll a')
  where ps = frees a
        a' = S.difference a (S.fromList ps)


main1 = getContents >>= (print . length . frees . mkField . lines)
main2 = getContents >>= (print . snd . freeAll . mkField . lines)

main = main1

