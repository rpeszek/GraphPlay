GraphPlay Example 2. Polymorphic graph creation
------
This example is very similar to Example 1 only without automatic deserialization

\begin{code}
module Build.E02_SafeDiamond (allThisHardWork) where
\end{code}

\begin{code}
import PolyGraph.ReadOnly.DiGraph (DiEdgeSemantics)
import PolyGraph.Buildable (BuildableEdgeSemantics, BuildableGraphDataSet, emptyGraph)
import PolyGraph.Buildable.DiGraph ((@+~>@))
\end{code}

We will just use 2 instance types

\begin{code}
import qualified SampleInstances.FirstLastWord   as TextSentencesGraph
import qualified Instances.SimpleGraph as Simple
\end{code}

This time we use @+~>@ function which adds directed edge and has signature
(not listing type constraints):

  (@+~>@) :: v -> v -> g -> g

which is much stronger than:

  (^+~>^) :: String -> String -> g -> g

_Symbol @ signifies vertex argument_.
Here is the new version of our type production function:

\begin{code}
safeDiamond :: forall g v e t . (BuildableEdgeSemantics e v,
                                 DiEdgeSemantics e v,
                                 BuildableGraphDataSet g v e t)
                                   => v -> v -> v -> v -> g
safeDiamond v0 v1 v2 v3 =  ( v0 @+~>@ v1 ) .
                           ( v0 @+~>@ v2 ) .
                           ( v0 @+~>@ v3 ) .
                           ( v1 @+~>@ v3 ) .
                           ( v2 @+~>@ v3 ) $ emptyGraph
\end{code}

To achieve safer code, the construction of the v type is left to the consumption code.

Still, I still want some flexibility. The proliferation of tools that are likely throw exceptions and have type:

    String -> v

is large and I think using indexing function will be safer:

    Int -> v

New consumption code looks much less prone to error (show is safer than read!):

\begin{code}
diamond0123' :: forall g v e t . (BuildableEdgeSemantics e v,
                                  DiEdgeSemantics e v,
                                  BuildableGraphDataSet g v e t)
                                    => (Int -> v) -> g
diamond0123' f = safeDiamond (f 0) (f 1) (f 2) (f 3)

showDiamond'0123_1 =
                let mygraph:: Simple.SimpleSetDiGraph String
                    mygraph = diamond0123' show
                in show (mygraph)
showDiamond'0123_2 =
                let mygraph :: TextSentencesGraph.FLWordText
                    mygraph = diamond0123' (TextSentencesGraph.FLWord . show)
                in show (mygraph)
\end{code}

Once again, please note that at the very end we are still not constructing (think selecting) data only types.  
To run it evaluate this in ghci:

\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
    putStrLn "showDiamond'0123_1:"
    putStrLn showDiamond'0123_1
    putStrLn "showDiamond'0123_2:"
    putStrLn showDiamond'0123_2
\end{code}
