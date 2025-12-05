import Control.Arrow
import Data.Char

type Interval = (Integer, Integer)

inInterval :: Integer -> Interval -> Bool
inInterval x (l, u) = l <= x && x <= u

parseInterval :: String -> Interval
parseInterval = (read *** (read . drop 1)) . span (/= '-')

mergeInterval :: Interval -> [Interval] -> [Interval]
mergeInterval ivl [] = [ivl]
mergeInterval (l, u) ((l', u') : ivls)
  | l  > u' + 1 = (l', u') : mergeInterval (l, u) ivls
  | l' > u +  1 = (l,  u)  : (l', u') : ivls
  | otherwise   = mergeInterval (min l l', max u u') ivls

main1 =
  do (ivls, ids) <- fmap ((map parseInterval *** (map read . drop 1)) . span (not . all isSpace) . lines) getContents
     print . length . filter (\x -> any (x `inInterval`) ivls) $ ids
     
main2 =
  do ivls <- fmap (map parseInterval . takeWhile (not . all isSpace) . lines) getContents
     print . sum . map (\(l,u) -> u - l + 1) . foldr mergeInterval [] $ ivls

main = main1
