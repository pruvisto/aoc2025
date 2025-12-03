import Data.List

parse :: String -> Integer
parse ('R' : s) = read s
parse ('L' : s) = -read s

indicator :: Bool -> Integer -> Integer
indicator b x = if b then x else 0

f :: Integer -> Integer -> (Integer, Integer)
f acc n = (r, q')
  where (q, r) = (acc + n) `divMod` 100
        q' = abs q + indicator (n < 0) (indicator (acc /= 0) 1 - indicator (r /= 0) 1)

main1 = getContents >>= (print . length . filter (== 0) . scanl (\x y -> (x + y) `mod` 100) 50 . map parse . lines)
main2 = getContents >>= (print . sum . snd . mapAccumL f 50 . map parse . lines)

main = main1
