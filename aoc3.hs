import Data.Char
import Data.List
import Data.Array

{-
  Dynamic programming approach. `g i j` or equivalently `a ! (i, j)` is the biggest number achievable
  by picking exactly `j` of the first `i` numbers. The boundary conditions are obviously `g i j = -∞` if
  `j > i` (not enough digits to choose from) or `g i j = 0` if `j == 0` (0 digits to choose).
  
  The recurrence is also easy: the optimal way to choose `j` of the first `i` numbers is either to use the
  `i`-th digit as the last one and then choose `j-1` of the first `i-1` digits, or to ignore the `i`-th 
  digit and choose `j` of the first `i-1` digits.
-}
foo :: Int -> String -> Integer
foo k s = a ! (n, k)
  where n     = length s
        s'    = listArray (0,n) (map (fromIntegral . digitToInt) s)
        a     = array ((0,0), (n,k)) [((i,j), g i j) | i <- [0..n], j <- [0..k]]
        g i j = if j > i || j == 0 then 0 else max (a ! (i-1, j)) (10 * a ! (i-1, j-1) + s' ! (i-1))

mainAux k = getContents >>= (print . sum . map (foo k) . lines)

main1 = mainAux 2
main2 = mainAux 12

main = main2

