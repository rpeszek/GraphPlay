GraphPlay Example 3. Polymorphic graph reload
------
In this example I will start by creating data of a specific type and reload it (or re-play it)
into a polymorphic data structure.

\begin{code}
module Build.E03_PolyReloaded (allThisHardWork) where
\end{code}

We will need these modules:

\begin{code}
import PolyGraph.Common (OPair(..))
import PolyGraph.Buildable ((+~), (+@))
import qualified PolyGraph.Buildable as B
import PolyGraph.Buildable.PolyMorth as Reload
import qualified Instances.ListGraphs as ListGraphs
import qualified Instances.SimpleGraph as Simple
import qualified Instances.DiGraph.DiEdgesByVertexMap as IndexGraph
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

This library treats graphs as conceputually consisting of 2 things:
  - A dataset aspect allowing to simply discover all edges and vertices.
    This discovery is independed of the graph adjacency structure.
    Dataset aspect is defined by GraphDataSet type class (PolyGraph.ReadOnly module)
  - Adjacency aspect (called an incidence function in Graph Theory)
    which resolves edges to pairs of adjacent vertices.
    These are different for undirected and directed graphs and are defined by EdgeSemantics type class
    (PolyGraph.ReadOnly.Graph module) and DiEdgeSemantics (PolyGraph.ReadOnly.DiGraph module)

In example 2 we have started with emptyGraph and used (@~>@) function repeatedly to
create edges and build a polymorphic diamond.
We could have alternatively used the (+@) function that adds one vertex at a time (constraints not show):

  (+@) :: g -> v -> g

and (+~) that adds one edge at the time

  (+~) :: g -> v -> g

If we can create a polymorphic graph that way, we should also be able to just fold over the edges
and vertices of a graph dataset and 'replay' the graph using (+@) and (+~), thus, creating a new polymorphic copy.

For diamond0123Simple, which does not have any isolatedVertices, the code to do so would look like so:

\begin{code}
diamond0123c :: forall g t . (B.BuildableGraphDataSet g Int (OPair Int) t) =>  g
diamond0123c = foldr (flip(+~)) B.emptyGraph (Simple.getEdges diamond0123Simple)
\end{code}

That works but is not that interesting.  A more interesting idea is to do some sort
of transformation as we 'reload/replay' the graph.  Graph-theoretic term for such
transformations is 'morphism'.  Future examples will use graph morthisms more.
Here we want do do something simple, we just shift the whole graph by (+3)

\begin{code}
diamond3456 :: forall g t . (B.BuildableGraphDataSet g Int (OPair Int) t) =>  g
diamond3456 = fmorth (+3) diamond0123Simple
\end{code}

So now we can create a chain of diamonds by doing OO style 'append to the end' approach.
Buildable graphs come for free with 'union' function which we use here:

\begin{code}
diamondChain :: forall g t . (B.BuildableGraphDataSet g Int (OPair Int) t) =>  Int -> g
diamondChain n =  foldr (\i g -> g `B.union` (Reload.fmorth (+i) diamond0123Simple)) B.emptyGraph (map (*3) [0..n])
\end{code}

or more interestingly we can do an FP style prepend:

\begin{code}
diamondInfiniteChain :: forall g t . (B.BuildableGraphDataSet g Int (OPair Int) t) =>  g
diamondInfiniteChain =
               let polyDiamond0123 = Reload.fmorth(id) diamond0123Simple
               in foldr (\i g -> polyDiamond0123 `B.union` (Reload.fmorth (+3) g)) B.emptyGraph [0..]
\end{code}

And I drove myself into an infinity corner.
Polymorphism to the rescue!  I just print first few vertices of that monster:

\begin{code}
lotsaVertices = diamondInfiniteChain :: ListGraphs.Vertices Int (OPair Int)
fewVertices = takeWhile(<30) (ListGraphs.getVertices lotsaVertices)
\end{code}

Our final consumption code in this example is again type specific.

\begin{code}
showDiamond3456AsAdjMap  = show (diamond3456    :: IndexGraph.DiEdgesByVertexMap Int (OPair Int) [])
showDiamondChainAsAdjMap = show (diamondChain 4 :: IndexGraph.DiEdgesByVertexMap Int (OPair Int) [])
showFewVertices = show (fewVertices)
\end{code}

To see the result evaluate this in ghci:

\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
    putStrLn "chain of 4 diamonds as adjecency map:"
    putStrLn showDiamondChainAsAdjMap
    putStrLn "fewVertices from infinite graph:"
    putStrLn showFewVertices
\end{code}
