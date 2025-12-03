import Control.Arrow
import Data.List
import Data.List.Split
import Data.Numbers.Primes

{-
primes :: [Int]
primes = 2 : filter (\n-> head (factor n) == n) [3,5..]

primeFactors :: Int -> [Int]
primeFactors = go (head primes) (tail primes)
  where divideOut d n = if r == 0 then divideOut d q else n
          where (q, r) = divMod n d
        go d ds n
          | n <= 1    = []
          | d ^ 2 > n = [n]
          | n `mod` d == 0 = d : go (head ds) (tail ds) (divideOut d n)
          | otherwise = go (head ds) (tail ds) n
-}

primeFactors' :: Integral a => a -> [a]
primeFactors' = map head . group . primeFactors

check1 :: String -> Bool
check1 s = even n && s1 == s2
  where n = length s
        (s1, s2) = splitAt (n `div` 2) s

{-
  Let `n` be the length of `s`. The set of integers `d` such that `rotate d s == s` forms an ideal in `ℤ/nℤ`.
  We are interested in strings `s` for which this ideal is *not* the zero ideal. The obvious way to check this
  is to test all numbers `d = 0, 1, ..., n-1`. This, however, clearly takes time `Θ(n²)`.
  
  A more efficient method is to realise that it is sufficient to check whether our ideal contains any of the
  minimal ideals. The minimal ideals of `ℤ/nℤ` are the ones of the form `(n/p)` for a prime factor `p` of `n`.
  In other words, we can restrict our checks to `d = n / p`. 
  
  This brings the running time down to `Θ(n ω(n))` where `ω(n)` is the number of distinct prime factors of `n`.
  Since `ω(n) ~ log n / log log n`, we have quasi-linear running time.
  
  The time required to factor `n` even by straightforward trial division is `O(n^(0.5+ε))` and therefore
  negligible.
-}
check2 :: String -> Bool
check2 s = any (\p -> and (zipWith (==) s (drop (n `div` p) s))) (primeFactors n)
  where n = length s

strToInteger :: String -> Integer
strToInteger = read

parse :: String -> [String]
parse = map show . uncurry enumFromTo . (strToInteger *** (strToInteger . drop 1)) . span (/= '-')

mainAux check = getLine >>= (print . sum . map strToInteger . concatMap (filter check . parse) . splitOn ",")

main = mainAux check2

