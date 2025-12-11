{-# LANGUAGE BangPatterns #-}
import Control.Arrow
import qualified Data.Map as M
import qualified Data.Set as S
import Data.Map (Map)
import Data.Set (Set)

type Node = String
type PredMap = Map Node (Set Node)
type OutdegMap = Map Node Int
type Graph = (PredMap, OutdegMap)

{-
  We represent a graph as a map from any node to the set of its predecessors. For efficiency, we also
  cache the outdegree of every node so that we can easily detect whether a node is a sink.
  
  Everything we do assumes the graph has no cycles. If there are cycles, the whole problem becomes potentially
  ill-defined, since there will be an infinite number of paths between some nodes.
-}
prep :: [(Node, [Node])] -> (PredMap, OutdegMap)
prep g = (preds, outdegs)
  where preds   = M.fromListWith S.union [(v, S.singleton u) | (u, vs) <- g, v <- vs]
        nodes   = S.unions [S.fromList (u : vs) | (u, vs) <- g]
        outdegs = M.fromListWith (+) $ [(u, length vs) | (u, vs) <- g] ++ [(u, 0) | u <- S.toList nodes]

parse :: String -> Graph
parse = prep . map (second (words . drop 1) . span (/= ':')) . lines

{-
  Removes a node from the graph and returns the new graph and a list of all nodes that are now sinks.
-}
removeNode :: Node -> Graph -> (Graph, [Node])
removeNode x (preds, outdegs) = ((preds', outdegs'), sinks)
  where preds'   = M.delete x preds
        updateOutdeg v (m, !sinks) = (M.insert v d m, (if d == 0 then [v] else []) ++ sinks)
          where d = M.findWithDefault 0 v m - 1
        (outdegs', sinks) = S.fold updateOutdeg (outdegs, []) (M.findWithDefault S.empty x preds)

{-
  We want to determine the number of paths from each node to `t`. We initially know that there is exactl
  one path from `t` to itself. Moreover, if the graph has any sinks other than possibly `t`, we know that there 
  are 0 paths from it to `t`.
  
  We then traverse the dual in inverse topological order, i.e. we pick a sink `v`, iterate through all immediate
  predecessors `u` of `v` and add the number of paths from `v` to `t` to the accumulated number of paths from `u` to `t`.
  Then we delete `v` and continue.
  
  As for the running time: we consider every node exactly once, and when we do, we consider each of its
  ingoing edges. In particular, every each is considered exactly once throughout the algorithm.
  The amount of work done for each of those edges consists mainly of some set and map operations, which take
  logarithmic time.

  We therefore probably end up with a running time of something like `O(m log n)`, where `n` is the number of nodes
  and `m` the number of edges. With better data structures (e.g. hash maps) one could probably get rid of that 
  logarithmic factor.
-}
countPathsTo :: Node -> Graph -> Map Node Integer
countPathsTo t g@(_, outdegs) = go (M.keys (M.filter (== 0) outdegs)) (M.singleton t 1) g
  where go [] paths _ = paths
        go (v : vs) paths g@(preds, outdegs) =
          let n = M.findWithDefault 0 v paths
              paths' = S.fold (\u -> M.insertWith (+) u n) paths (M.findWithDefault S.empty v preds)
              (g', sinks) = removeNode v g
          in  go (sinks ++ vs) paths' g'

{-
  Counts the number of paths from one node to another node. Could be optimised slightly be stopping as soon as
  we've reached our source node rather than computing the number of paths from all nodes to `t`, but the performance
  gain in the given test cases is minimal.
-}
countPathsFromTo :: Node -> Node -> Graph -> Integer
countPathsFromTo s t g = M.findWithDefault 0 s $ countPathsTo t g

main1 = getContents >>= (print . countPathsFromTo "you" "out" . parse)

{-
  For the second part of the problem, we note that any path from `svr` to `out` that contains both `dac` and `fft` is
  either composed of a path from `svr` to `dac`, a path from `dac` to `fft`, and a path from `fft` to `out` or the
  same but with `dac` and `fft` switched. We can therefore simply compute the desired number as below.
  
  Note that in fact that at most one of these two possibilities is possible, depending on whether `fft` is reachable
  from `dac` or the other way round (both are not possible due to acyclicity).
-}
main2 =
  do g <- fmap parse getContents
     let mDac = countPathsTo "dac" g
     let mFft = countPathsTo "fft" g
     let mOut = countPathsTo "out" g
     let count = M.findWithDefault 0
     print $ count "svr" mDac * count "dac" mFft * count "fft" mOut +
             count "svr" mFft * count "fft" mDac * count "dac" mOut

main = main2

