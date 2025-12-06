import Control.Arrow
import Data.Char
import Data.List
import Data.List.Split

trim :: String -> String
trim = reverse . dropWhile isSpace . reverse . dropWhile isSpace

main1 =
  do xss <- fmap (map (map trim . words) . lines) getContents
     let (yss, [os]) = first (map (map read)) $ span ((`notElem` ["+", "*"]) . head) xss
     let zss = zipWith (\o ys -> (if o == "+" then sum else product) ys) os (transpose yss) :: [Integer]
     print $ sum zss
     
main2 =
  do ls <- fmap lines getContents
     let (ls', [os]) = splitAt (length ls - 1) ls
     let xss = map (map read) . splitWhen null . map trim $ transpose ls' :: [[Integer]]
     let zss = zipWith (\o ys -> (if o == "+" then sum else product) ys) (words os) xss
     print $ sum zss

main = main2
