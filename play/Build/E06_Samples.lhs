GraphPlay Example 6. Sample Graphs
------
This program uses expressive powers of applicative list and list monad to create some 
sample graphs.
I will use these graphs in future examples.

\begin{code}
module Build.E06_Samples (
   allThisHardWork
   , bipartiteGraph
   , cycleGraph
   , grid
   , completeGraph
) where

import Control.Applicative
import PolyGraph.Common (UOPair, OPair, PairLike(..))
import PolyGraph.ReadOnly.Graph (EdgeSemantics)
import PolyGraph.Buildable ((+@))
import PolyGraph.Buildable.Graph ((@+~~@), toSimpleGraph)
import PolyGraph.ReadOnly.DiGraph (DiEdgeSemantics)
--import PolyGraph.Buildable.DiGraph ((@+~>@))
import PolyGraph.Buildable (BuildableEdgeSemantics, BuildableGraphDataSet, emptyGraph)

import qualified Instances.SimpleGraph as Simple
import qualified Instances.ListGraphs as ListGraphs
\end{code}

Bipartite (undirected) Graph is a graph obtained by taking 2 disjoint sets of vertices X1 and X2
and connecting every element from X1 with every element from X2.
The following logic does not validate that lists of vertices are disjoint.
If they intersect, the created graph will have loops and multiedges.

Bipartite graphs provide geometric intuition about 'standard' applicative list behavior. 
Connecting every element from partitionX set with every element form partitionY set is exactly what 
(@+~~@) <$> partitionX <*> partitionY does.

\begin{code}
bipartiteGraph :: forall g v e t . (BuildableEdgeSemantics e v,
                                   EdgeSemantics e v,
                                   BuildableGraphDataSet g v e t)
                                     =>  ([v],[v]) -> g
bipartiteGraph bipartition = 
                    let (partitionX, partitionY) = bipartition
                    in foldr ($) emptyGraph $ 
                        (@+~~@) <$> partitionX <*> partitionY
\end{code}

Cycle (undirected) Graph uses list as ZipList applicative. Again, I think, this could
provide nice intuition of how ZipList applicative works.
This code uses indexing function (Int->v) with values in arbitrary vertex type.
\begin{code}
cycleGraph :: forall g v e t . (BuildableEdgeSemantics e v,
                                   EdgeSemantics e v,
                                   BuildableGraphDataSet g v e t)
                                     =>  (Int->v) -> Int -> g
cycleGraph vInx size = 
         let vertexList :: [Int] -> ZipList v
             vertexList indxs = ZipList $ map vInx indxs
          in (foldr ($) emptyGraph) . getZipList $ 
            (@+~~@) <$> vertexList ((size-1):[0..(size-2)]) <*> vertexList [0..(size-1)]    
\end{code}

I have defined grid in example 4.  Here is an applicative equivalent of the same logic.
I will do undirected version to make it a bit different.
I find this code more intuitive as I can picture how lists of vertices are connecting with each other
to create a line fragments and then connect back to the smaller graph. All using the 'zip' list
\begin{code}
grid :: forall g v e t. (
                          BuildableEdgeSemantics e v,
                          EdgeSemantics e v,
                          BuildableGraphDataSet g v e t)
                                   =>  Int -> (Int -> Int -> v) -> g
grid 0 _ = emptyGraph
grid 1 f = emptyGraph +@ (f 0 0) :: (BuildableGraphDataSet g v e t ) => g
grid n f =
          let rowFragment :: Int -> (Int, Int) -> ZipList v
              rowFragment y (xFrom, xTo) = ZipList $ map ((flip f) y) [xFrom .. xTo]
              colFragment :: Int -> (Int, Int) -> ZipList v
              colFragment x (yFrom, yTo) = ZipList $ map (f x) [yFrom .. yTo]
              applyToGraph :: ZipList (g -> g) -> g -> g
              applyToGraph list g = foldr ($) g $ getZipList list

              addHLine y  = applyToGraph $
                             (@+~~@) <$> rowFragment y (0, (n-2)) <*> rowFragment y     (1, (n-1))
              addVBars y  = applyToGraph $
                             (@+~~@) <$> rowFragment y (0, (n-2)) <*> rowFragment (y+1) (0, (n-2))
              addVLine x  = applyToGraph $
                             (@+~~@) <$> colFragment x (0, (n-2)) <*> colFragment x     (1, (n-1))
              addHBars x  = applyToGraph $ 
                             (@+~~@) <$> colFragment x (0, (n-2)) <*> colFragment (x+1) (0, (n-2))

          in  addHLine (n-1) . addVBars (n-2) . addVLine (n-1) . addHBars (n-2) $ grid (n-1) f
\end{code}

Complete (undirected) Graph is one where each pair of two vertices is connected using exactly one edge.
Applicative is not the best choice here because I have no easy way to prevent muliedges.
Monadic list comprehension works great (and I just love the set-theoretic look of it).
Again, indexing function is used to keep the vertex type general. 
\begin{code}
completeGraph :: forall g v e t . (Eq e,
                                  BuildableEdgeSemantics e v,
                                  EdgeSemantics e v,
                                  BuildableGraphDataSet g v e t)
                                    =>  (Int->v) -> Int -> g
completeGraph vInx size =
        let vertexPairs = [(vInx i1, vInx i2) | i1 <- [0..(size-1)], i2 <- [0..(size-1)], i1 < i2 ]  
        in  foldr (uncurry (@+~~@)) emptyGraph vertexPairs
\end{code}

And, finally, we can see how these polymorphic data structures look can like:
\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
  putStrLn "BipartiteGraph:"
  putStrLn $ show (bipartiteGraph ([0..3], [10..11]) :: Simple.SimpleListGraph Int)
  putStrLn $ show (bipartiteGraph ([0..3], [10..11]) :: ListGraphs.GEdges Int)
  putStrLn "Cycle:"
  putStrLn $ show (cycleGraph id 4 :: ListGraphs.GEdges Int)
  putStrLn "GridDiGraph:"
  putStrLn $ show (grid 3 (,) :: ListGraphs.GEdges (Int, Int))
  putStrLn "CompleteGraph:"
  putStrLn $ show (completeGraph id 4 :: Simple.SimpleListGraph Int)
  putStrLn $ show (completeGraph id 4 :: ListGraphs.GEdges Int)
\end{code}

Applicative and Monad are among the most polymorphic concepts out there.
I think it is only fitting to have these used as PolyGraph sample graphs.
