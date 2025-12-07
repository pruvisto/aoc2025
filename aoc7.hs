import qualified Data.Set as S
import Data.Set (Set)
import qualified Data.Map as M
import Data.Map (Map)
import Data.List

main1 =
  do l : ls <- fmap lines getContents
     let Just startPos = elemIndex 'S' l
     let step (beams, acc) = foldl f (beams, acc) . filter (`S.member` beams) . elemIndices '^'
           where f (beams, acc) i = (S.insert (i-1) . S.insert (i+1) . S.delete i $ beams, acc + 1)
     print . snd $ foldl step (S.singleton startPos, 0) ls
     
main2 =
  do l : ls <- fmap lines getContents
     let Just startPos = elemIndex 'S' l
     let step acc = foldl f acc . elemIndices '^'
           where f acc i = M.insert i (sum [M.findWithDefault 1 j acc | j <- [i-1,i+1]]) acc
     let acc = foldl step M.empty (reverse ls)
     print $ M.findWithDefault 1 startPos acc

main = main2
