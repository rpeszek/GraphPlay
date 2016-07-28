GraphPlay Example 4. Polymorphism to the max
------
As a OO programmer I found the very idea of polymorphic data production fascinating.
Examples 1 and 2 were all about it. That idea brings the next puzzling question: is it possible
to both produce and consume data polymorphically, thus, creating programs that are instance independent.

The answer is obvioulsy: no that would not be logical. For example, I can polymphically calculate edge count
using eCount function defined by the GraphDataSet (PolyGraph.ReadOnly) type class, but both Vertices and Edges
(PolyGraph.Instances.ListGraphs) implement BuildableGraphDataSet and for Vertices instance eCount will always
be zero (Vertices type forgets about edges) and for Edges will not.

This program attempt to do the next best thing. Create a program that is 'almost' agnostic to instance types.
\begin{code}
module Build.E04_PolyToMax (allThisHardWork, grid) where
\end{code}

We will need our polymorphic graph building machinery:
\begin{code}
import qualified PolyGraph.ReadOnly as Base
import qualified PolyGraph.ReadOnly.DiGraph as DiG
import PolyGraph.Common (OPair(..), PairLike(..))
import PolyGraph.Buildable ((@+))
import PolyGraph.Buildable.DiGraph ((@+~>@))
import qualified PolyGraph.Buildable as B
import qualified PolyGraph.ReadOnly.DiGraph.Fold.TAMonoidFold as FastFold
\end{code}

We will use these to finally specialize on the instance types
\begin{code}
import qualified PolyGraph.Instances.ListGraphs as ListGraphs
import Data.List
import Control.Arrow ((&&&))
\end{code}

We need something to signal the type.
\begin{code}
on :: forall g v e t.  B.BuildableGraphDataSet g v e t => g
on = B.emptyGraph
\end{code}

\begin{code}
grid :: forall g v e t. (
                          B.BuildableEdgeSemantics e v,
                          DiG.DiEdgeSemantics e v,
                          B.BuildableGraphDataSet g v e t)
                                   =>  Int -> (Int -> Int -> v) -> g
grid 0 _ = B.emptyGraph
grid 1 f = B.emptyGraph @+ (f 0 0) :: (B.BuildableGraphDataSet g v e t ) => g
grid n f =
          let addHLine :: Int -> g -> g
              addHLine j g = foldr(\i -> (f i j) @+~>@ (f (i+1) j)) g [0..(n-2)]
              addVBars j g = foldr(\i -> (f i j) @+~>@ (f i (j+1))) g [0..(n-1)]
              addVLine i g = foldr(\j -> (f i j) @+~>@ (f i (j+1))) g [0..(n-2)]
              addHBars i g = foldr(\j -> (f i j) @+~>@ (f (i+1) j)) g [0..(n-1)]
          in  addHLine (n-1) . addVBars (n-2) . addVLine (n-1) . addHBars (n-2) $ grid (n-1) f
\end{code}

_Motivation_: TODO

To keep things simple and focused, moving forward, we specialize vertex type to (Int, Int) and edge type to OPair.
What I want is a fuction which looks like this:

 countGridVertices:: Int -> Int

accepting size of the graph and computing the number of verices, not carying about which of the instances of
BuildableGraphDataSet is being used. This is not logical and Haskell compiler will not allow it. So I have 2 choices:

  - accept bugus input parameter (similar to Java List.toArray) to tell compiler which type is used
  - include graph type in the result

Since the later the first approach is more familiar to OO coders I will run with it:
\begin{code}
countGraphVertices  :: forall g v . (B.BuildableGraphDataSet g (Int,Int) (OPair (Int,Int)) [])
                                    => g -> Int -> Int
countGraphVertices _ n = Base.vCount (toPair . DiG.resolveDiEdge) (grid n (,) :: g)

\end{code}

\begin{code}
newtype Sum a = Sum { getSum :: a } deriving Show
instance Num a => Monoid (Sum a) where
    mempty = Sum 0
    Sum x `mappend` Sum y = Sum (x + y)

treeNodeCountFoldLogic :: FastFold.MonoidFoldAccLogic v (OPair v) (Sum Int)
treeNodeCountFoldLogic = FastFold.defaultMonoidFoldAccLogic {
                      FastFold.applyVertex  = const (Sum 1)
                 }

countTreeNodes:: forall g v . (DiG.DiAdjacencyIndex g (Int,Int) (OPair (Int,Int)) [],
                              B.BuildableGraphDataSet g (Int,Int) (OPair (Int,Int)) [])
                                 => g -> Int -> Int
countTreeNodes _ n = getSum $ FastFold.dfsFold (grid n (,) :: g) treeNodeCountFoldLogic (0,0)
\end{code}

\begin{code}
runProgram :: forall g v . (DiG.DiAdjacencyIndex g (Int,Int) (OPair (Int,Int)) [],
                               B.BuildableGraphDataSet g (Int,Int) (OPair (Int,Int)) [])
                                     =>  Int -> g -> [(Int, Int)]
runProgram n g = map (countGraphVertices g &&& countTreeNodes g) [1..n]
\end{code}

\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
    putStrLn "Compare grid graph v-size with corresponding binary tree node-size:"
    putStrLn $ show (runProgram 10 (on:: ListGraphs.Edges (Int,Int) (OPair (Int,Int))))
    putStrLn "Same comparizon using Vertices instance which drops edges:"
    putStrLn $ show (runProgram 10 (on:: ListGraphs.Vertices (Int,Int) (OPair (Int,Int))))
\end{code}
