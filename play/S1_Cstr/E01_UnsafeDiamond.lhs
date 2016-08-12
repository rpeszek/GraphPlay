1.01 Constraint Polymorphism.  Polymorphic graph data structures.
------
If you are like me and have many years of OO habits, this example will look strange.

\begin{code}
module S1_Cstr.E01_UnsafeDiamond (allThisHardWork) where
\end{code}

This program shows polymorphic production of graph data structures
with consumption specialized to specific instance data types.
_It should become clear what that means in just a moment_.

Polymorphic directed graphs are defined in

\begin{code}
 import qualified PolyGraph.Common as Common
 import PolyGraph.ReadOnly.DiGraph (DiEdgeSemantics)
 import PolyGraph.Buildable (PrettyRead, BuildableEdgeSemantics, BuildableGraphDataSet, emptyGraph)
 import PolyGraph.Buildable.DiGraph ((^+~>^))
\end{code}

have various implementations defined in

\begin{code}
 import qualified Instances.SimpleGraph as Simple
 import qualified Instances.DiGraph.DiEdgesByVertexMap as AdjacencyIndex
 import qualified Instances.AdjacencyMatrix as AMatrix
 import qualified Instances.ListGraphs as ListGraphs
 import qualified SampleInstances.FirstLastWord as TextSentencesGraph
\end{code}

and this example will also need to handle errors:

\begin{code}
 import qualified Control.Exception as Exc

 catch :: IO a -> (Exc.SomeException -> IO a) -> IO a
 catch = Exc.catch
\end{code}

About Graph Theory:
Diamond graph can be a di-graph or regular graph and has 4 vertices and 5 edges and looks like this
(as di-graph, this diagram is directed left to right)

```
      1
   /     \
0   ----   3
   \     /
      2
```

About our program:
The following 'diamond' function creates a (polymorphic) diamond directed graph with vertices v0 v1 v2 v3.
and 'diamond0123' is that function applied to 4 String arguments

About the syntax:
  -  ^+~>^ represents a function that adds a directed edge
  -  ~ signifies edge
  -  > signifies direction
  -  ^ signifies deserialization of vertex arguments.

So, here is our polymorphic type production code:

\begin{code}
 diamond :: forall g v e t . (PrettyRead v,
                              BuildableEdgeSemantics e v,
                              DiEdgeSemantics e v,
                              BuildableGraphDataSet g v e t)
                                => String -> String -> String -> String -> g
 diamond v0 v1 v2 v3 =    ( v0 ^+~>^ v1 ) .
                          ( v0 ^+~>^ v2 ) .
                          ( v0 ^+~>^ v3 ) .
                          ( v1 ^+~>^ v3 ) .
                          ( v2 ^+~>^ v3 ) $ emptyGraph

 diamond0123 :: forall g v e t . (PrettyRead v,
                                  BuildableEdgeSemantics e v,
                                  DiEdgeSemantics e v,
                                  BuildableGraphDataSet g v e t)
                                    => g
 diamond0123 = diamond "0" "1" "2" "3"
\end{code}

Understanding the code:
Note that 'diamond0123' variable is defined 'forall' types g v e t as long as they satisfy listed constraints
  - 'PrettyRead v' defined in Buildable module, is alternative to 'Read' that acts as Identity on Strings.
     Simply put: vertex type has some deserialization mechanism.
  - 'BuildableEdgeSemantics e v' means that the edge type e supports ability to create edges when vertices are known
  - 'DiEdgeSemantics e v' means that the type e provides a way of finding adjacent vertices.
     In graph-theoretical terms it means that there is an incidence function e -> OrderedPair(v,v)
  - 'BuildableGraphDataSet g v e t' means that g is a graph that implements a way of adding vertices and edges

And here is how our polymorphic graph data structure can be consumed:

\begin{code}
 showDiamond0123_1 = show (diamond0123 :: Simple.SimpleSetDiGraph String)
 showDiamond0123_2 = show (diamond0123 :: Simple.SimpleListDiGraph Int)
 showDiamond0123_3 = show (diamond0123 :: TextSentencesGraph.FLWordText)
 showDiamond0123_4 = show (diamond0123 :: AdjacencyIndex.DiEdgesByVertexMap String (Common.OPair String) [])
 showDiamond0123_5 = AMatrix.prettyAdjacencyMatrix (diamond0123 :: AMatrix.DiAdjacencyMatrix Int)
 showDiamond0123_6 = show (diamond0123 :: ListGraphs.Edges Int (Common.OPair Int))
 showDiamond0123_7 = show (diamond0123 :: ListGraphs.Vertices Int (Common.OPair Int))
\end{code}

Understanding the code:  In all 7 lines we are still constructing (think selecting) types not data. 
- showDiamond0123_1 (-2) specializes diamond0123 to SimpleSetDiGraph or SimpleListDiGraph type,
  In Graph Theory, Simple Graphs are graphs that do not have multiple edges or loops.
  _Sidenote_: Simple<Whatever>DiGraph deviates from that a bit and allows for a single loop.
  It is also called 'Simple' because of its straightforward implementation based on
  HashSet provided by unordered-containers library and the standard library List ([]).

- showDiamond0123_3 uses "FirstLastWord" graph instance where the graph is a text, edges
  are sentences in that text and adjacent vertices are first and last words in the sentence.
  It is a weird example, but it works.

- showDiamond0123_4 is one of the classic ways to represent graphs, it is the adjacency index map
  that has vertex as key and adjacent di-edges list as value.

- showDiamond0123_5 specializes diamond0123 to its AdjacencyMatrix.  AdjacencyMatrix is just another
  instance of our polymorphic Graph.  Ain't that cool?

- showDiamond0123_6 graph becomes just a set of edges.  Edges is a forgetful
  instance type which does not remember all vertices (forgets isolated vertices)

- showDiamond0123_7 Vertices type forgets about the edges being added and remembers only vertices.

This example trades type safety for convenience.  Type checking cannot verify deserialization, so
you can do things like this and get a runtime error:

\begin{code}
 diamondABCD :: forall g v e t . (PrettyRead v,
                                  BuildableEdgeSemantics e v,
                                  DiEdgeSemantics e v,
                                  BuildableGraphDataSet g v e t)
                                   => g
 diamondABCD = diamond "a" "b" "c" "d"

 showDiamondWrong = show (diamondABCD :: Simple.SimpleSetDiGraph Int)
\end{code}

And finally, please note that this is a literate program! It is supposed to use LaTeX markup.
I am just using flat text/markdown for simplicity. But I do admit: I am tempted to write one program that
would use mathematical symbols when referring to graphs.

Yes, it is a valid program that runs and executes. Try it by importing or loading this module in ghci
and evaluate all this hard work:

\begin{code}
 allThisHardWork :: IO()
 allThisHardWork = do
   putStrLn "showDiamondWrong:"
   catch (putStrLn showDiamondWrong) (\e -> putStrLn $ "error: " ++ (show e))
   putStrLn "showDiamond0123_1:"
   putStrLn showDiamond0123_1
   putStrLn "showDiamond0123_2:"
   putStrLn showDiamond0123_2
   putStrLn "showDiamond0123_3:"
   putStrLn showDiamond0123_3
   putStrLn "showDiamond0123_4:"
   putStrLn showDiamond0123_4
   putStrLn "showDiamond0123_5:"
   putStrLn showDiamond0123_5
   putStrLn "showDiamond0123_6:"
   putStrLn showDiamond0123_6
   putStrLn "showDiamond0123_7:"
   putStrLn showDiamond0123_7
\end{code}

Next example will create diamond without deserializing vertices.
