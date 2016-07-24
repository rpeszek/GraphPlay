GraphPlay Example 3
Previous examples played with creation of polymorphic data structures. These structures were not
locket on any particular instance type until the time of consumption.
In this module

\begin{code}
module Build.E03_PolyReloaded (allThisHardWork) where
\end{code}

In this example I will start with specific type and reload it (or re-play it)
into a polymorphic data structure.

We will need these modules:

\begin{code}
import PolyGraph.Common (OPair(..))
import PolyGraph.Buildable ((~+), (@+))
import qualified PolyGraph.Buildable as B
import PolyGraph.Buildable.PolyMorth as Reload
import qualified PolyGraph.Instances.ListGraphs as ListGraphs
import qualified PolyGraph.Instances.SimpleGraph as Simple
import qualified PolyGraph.Instances.DiGraph.DiEdgesByVertexMap as IndexGraph
import Data.Hashable
\end{code}

Our example diamond graph starts as Simple.SimpleListDiGraph with Int vertices. This data structure
stores data as two lists:
 -  list of edges represented by ordered pairs: [OPair(Int,Int)]
 -  list of isolated vertices: [Int]

\begin{code}
edgeList :: [(Int, Int)]
edgeList = [(0,1), (0,2), (0,3), (1,3), (2,3)]

diamond0123Simple :: Simple.SimpleListDiGraph Int
diamond0123Simple = Simple.SimpleGraph (map (OPair) edgeList) []
\end{code}

Graphs in this library consist of 2 things:
  - a dataset aspect allowing to fold over edges and isolated vertices.
  - adjacency aspect (called an incidence function in Graph Theory)
    which resolves edges to pairs of adjacent vertices.

In example 2 we have started with emptyGraph and used (@~>@) function repeatedly to
create edges and build a polymorphic diamond.
We could have alternatively used function that adds one vertex at a time (constraints not show):
  (@+) :: g -> v -> g
or one edge at the time
  (~+) :: g -> v -> g
If we can create a polymorphic graph that way, we should also be able to just fold over the edges
and vertices of a graph and 'replay' it using (@+) and (~+), thus, creating a new polymorphic copy.

For diamond0123Simple which does not have any isolatedVertices the code to do so would look like

\begin{code}
diamond0123c :: forall g t . (B.BuildableGraphDataSet g Int (OPair Int) t) =>  g
diamond0123c = foldr (flip(~+)) B.emptyGraph (Simple.getEdges diamond0123Simple)
\end{code}

That works but is not that interesting.  A more interesting idea is to do some sort
of transformation as we 'reload/replay' the graph.  Graph-theoretic term for such
transformations is 'morphism'.  Future examples will use morthisms more.
Here we want do do something simple, we just shift the whole graph by (+3)

\begin{code}
diamond3456 :: forall g t . (B.BuildableGraphDataSet g Int (OPair Int) t) =>  g
diamond3456 = fmorth (+3) diamond0123Simple
\end{code}

So now we can create a chain of diamonds by doing OO style append to the end approach.
Buildable graphs come for free with 'union' function which we use here:

\begin{code}
diamondChain :: forall g t . (B.BuildableGraphDataSet g Int (OPair Int) t) =>  Int -> g
diamondChain n =  foldr (\i g -> g `B.union` (Reload.fmorth (+i) diamond0123Simple)) B.emptyGraph (map (*3) [0..n])
\end{code}

or more interestingly we can do a FP style prepend:

\begin{code}
diamondInfiniteChain :: forall g t . (B.BuildableGraphDataSet g Int (OPair Int) t) =>  g
diamondInfiniteChain =
               let polyDiamond0123 = Reload.fmorth(id) diamond0123Simple
               in foldr (\i g -> polyDiamond0123 `B.union` (Reload.fmorth (+3) g)) B.emptyGraph [0..]
\end{code}

And I drove myself into a corner with infinite graph.
Polymorphism to the rescue!  I just print first few vertices of that monster.

\begin{code}
lotsaVertices = diamondInfiniteChain :: ListGraphs.Vertices Int (OPair Int)
fewVertices = takeWhile(<30) (ListGraphs.getVertices lotsaVertices)
\end{code}

Our final consumption code in this example is again type specific.

\begin{code}
showDiamond3456AsHashMap  = show (diamond3456    :: IndexGraph.DiEdgesByVertexMap Int (OPair Int) [])
showDiamondChainAsHashMap = show (diamondChain 4 :: IndexGraph.DiEdgesByVertexMap Int (OPair Int) [])
showFewVertices = show (fewVertices)
\end{code}

To see the result evaluate this in ghci.

\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
    putStrLn "showDiamond'0123_1:"
    putStrLn showDiamondChainAsHashMap
    putStrLn "fewVertices from infinite graph:"
    putStrLn showFewVertices
\end{code}
