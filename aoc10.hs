import Control.Monad
import Control.Monad.State

import Data.Bits
import Data.Char
import Data.List
import qualified Data.Map.Strict as M
import Text.ParserCombinators.ReadP

type BitVec = Int

toBitVec :: [Bool] -> BitVec
toBitVec = foldl' (\acc b -> 2 * acc + (if b then 1 else 0)) 0 . reverse

toBitVec' :: [Int] -> BitVec
toBitVec' is = foldl' (.|.) 0 [2 ^ i | i <- is]

data Machine = Machine BitVec [BitVec] [Int]
  deriving (Show, Eq, Ord)

parse :: String -> Machine
parse = fst . head . readP_to_S p
  where ignoreSpaces p = do {skipSpaces; x <- p; skipSpaces; return x}
        int     = fmap read (munch1 isDigit)
        lights  = ignoreSpaces $ between (char '[') (char ']') (fmap (toBitVec . map (== '#')) (munch (`elem` ".#")))
        wiring  = many . ignoreSpaces $ between (char '(') (char ')') (fmap toBitVec' (sepBy int (char ',')))
        joltage = ignoreSpaces $ between (char '{') (char '}') (sepBy int (char ','))
        p = do {m <- liftM3 Machine lights wiring joltage; eof; return m}

pick :: Int -> [a] -> [[a]]
pick 0 _  = return []
pick _ [] = []
pick n (x : xs) = [x : ys | ys <- pick (n-1) xs] ++ pick n xs

{-
  Finding a solution can easily be done in polynomial time by viewing the problem as a
  system of equations in GF(2) (i.e. integers modulo 2), e.g. using Gaussian elimination.
  However, I don't know of any way to find minimal solutions in polynomial time.
  
  Luckily, brute-forcing works just fine. We return the first `i = 0, 1, ..., n` for which
  there is a subset of buttons of size `i` that, when toggled, gives the right pattern.
-}
solve1 :: Machine -> Int
solve1 (Machine l wss _) =
  head [i | i <- [0..length wss], wss' <- pick i wss, foldl' xor 0 wss' == l]

data Infinite a = Finite a | Infinity
  deriving (Show, Eq, Ord)

suc :: Num a => Infinite a -> Infinite a
suc (Finite n) = Finite (n+1)
suc Infinity   = Infinity  

{-
  Part 2 is essentially a subset of integer linear programming and very probably NP-complete.
  The following approach solves it using a simple dynamic programming approach. It works well
  for the example inputs on the problem description but is far too slow to solve the actual task.
  
  The obvious approach is therefore to use an external ILP solver. I could not get any of the
  Haskell packages for ILP to run, so I switched to Python for this task.
-}
solve2 :: Machine -> Int
solve2 (Machine _ btns jolts) = res  
  where Finite res = fst $ runState (g jolts) (M.singleton (replicate (length jolts) 0) (Finite 0))
        apply btn jolts = snd $ mapAccumL (\x y -> (shiftR x 1, y - x .&. 1)) btn jolts
        g jolts = if any (< 0) jolts then return Infinity else
          do res <- gets (M.lookup jolts)
             case res of
               Just y -> return y
               Nothing -> 
                 do y <- fmap (suc . minimum) . sequence $ [g (apply btn jolts) | btn <- btns]
                    modify (M.insert jolts y)
                    return y

main1 =
  do ms <- fmap (map parse . lines) getContents
     print . sum . map solve1 $ ms

main2 =
  do ms <- fmap (map parse . lines) getContents
     let go m = do {let x = solve2 m in print x >> return x}
     xs <- mapM go ms
     print $ sum xs

main = main2

