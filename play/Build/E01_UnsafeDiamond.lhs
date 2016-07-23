\begin{code}
module Build.E01_UnsafeDiamond where
\end{code}

This example shows polymorphic production of graph data structures
with consumption specialized to specific instance data types.

Polymorphic Directed Graphs are defined in

\begin{code}
 import qualified PolyGraph.Common as Common
 import PolyGraph.ReadOnly.DiGraph
 import PolyGraph.Buildable
 import PolyGraph.Buildable.DiGraph
\end{code}

and have various implementations defined in

\begin{code}
 import qualified PolyGraph.Instances.DiGraph.DiEdgesByVertexMap as HM
 import qualified PolyGraph.Instances.SimpleGraph as SG
 import qualified PolyGraph.Instances.AdjacencyMatrix as AM
 import qualified PolyGraph.Instances.ListGraphs as LG
 import qualified SampleInstances.FirstLastWord as FL
\end{code}

Diamond graphs have 4 vertices and 5 edges and look like so (directed to the right):

      1
   /     \
0   ----   3
   \     /
      2

'diamond' is a function that creates a Diamond directed graph with vertices v0 v1 v2 v3.
and 'diamond0123' is that function applied to 4 string arguments
The symbol ^+~>^ represents a function that adds directed edge to the graph with > signifying direction
and ^ signifies deserialization of vertex arguments.

Here is our polymorphic data production code:

\begin{code}
 diamond :: forall g v e t . (FromString v, BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t)
                           => String -> String -> String -> String -> g
 diamond v0 v1 v2 v3 =    ( v0 ^+~>^ v1 ) .
                          ( v0 ^+~>^ v3 ) .
                          ( v0 ^+~>^ v2 ) .
                          ( v1 ^+~>^ v3 ) .
                          ( v2 ^+~>^ v3 ) $ emptyGraph

 diamond0123 :: forall g v e t . (FromString v, BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t) => g
 diamond0123 = diamond "0" "1" "2" "3"
\end{code}

Please note that diamond0123 is defined 'forall' types g v e t as long as they satisfy listed constraints
  - 'FromString v' means that vertex type has deserialization mechanism
  - 'BuildableEdgeSemantics e v' means that edge type supports ability to create edges when vertices are known
  - 'DiEdgeSemantics e v' means that type e provides a way of finding adjacent vertices
  - 'BuildableGraphDataSet g v e t' means that g is a graph that implements a way of adding vertices and edges

And here is how our polymorphic graph data structure can be consumed:

\begin{code}
 showDiamond0123_1 = show (diamond0123 :: SG.SimpleSetDiGraph String)
 showDiamond0123_2 = show (diamond0123 :: SG.SimpleSetDiGraph Int)
 showDiamond0123_3 = show (diamond0123 :: FL.FLWordText)
 showDiamond0123_4 = show (diamond0123 :: HM.DiEdgesByVertexMap String (Common.OPair String) [])
 showDiamond0123_5 = AM.prettyAdjacencyMatrix (diamond0123 :: AM.DiAdjacencyMatrix Int)
 showDiamond0123_6 = show (diamond0123 :: LG.Edges Int (Common.OPair Int))
 showDiamond0123_7 = show (diamond0123 :: LG.Vertices Int (Common.OPair Int))
\end{code}

- showDiamond0123_1 - 2 specialized to a simple directed graph, one that uses 'Set' in its implementation.
  Simple Graphs are graphs that do not have multiple edges (techie graph theory term).

- showDiamond0123_3 uses "FirstLastWord" graph instance where the graph is a text, edges
  are sentences in that text and vertices are first and last words in the sentence.
  Believe it or not! It works.

- showDiamond0123_4 is a classic, it is a map with keys being vertices and values being adjacent di-edges.

- showDiamond0123_5 specializes diamond0123 to its AdjacencyMatrix.  AdjacencyMatrix is just another
  instance of our polymorphic Graph.  Ain't that cool?

- showDiamond0123_6 here our graph becomes just a set of edges.  Edges is a forgetful
  instance which does not remember added vertices if they are isolated (not adjacent to an edge)

- showDiamond0123_7 finally our graph becomes and interesting forgetful instance called Vertices.
  Vertices forget about all edges being added and remember only their vertices.

And finally please note that this is a very literate program!
Yes it is a valid program that runs and executes.

Why did I call it unsafe?  Compiler cannot check the deserialization so
you can do things like this and get a runtime error:

\begin{code}
 diamondABCD :: forall g v e t . (FromString v, BuildableEdgeSemantics e v, DiEdgeSemantics e v, BuildableGraphDataSet g v e t) => g
 diamondABCD = diamond "a" "b" "c" "d"

 diamondWrong = diamondABCD :: SG.SimpleSetDiGraph Int
\end{code}
