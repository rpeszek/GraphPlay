GraphPlay Example 4. Polymorphism to the max 
------
As a OO programmer I have found the idea of polymorphic data production fascinating.
Examples 1 and 2 were dedicated to that idea. Would it be possible to both produce and consume data polymorphically?
... thus, creating programs that are completely instance independent?

The answer is: no that would not make sense because results of the calculation can depend on which
instance is used. For example, I can polymphically calculate edge count using eCount function defined by
the GraphDataSet (PolyGraph.ReadOnly) type class.  Result of this calculation for Vertices
(PolyGraph.Instances.ListGraphs) will always be zero (Vertices instance forgets about edges).

This program attempts to do the next best thing. Create a program that stays
agnostic of which instance is used until the very end.
\begin{code}
module Build.E04_PolyToMax (allThisHardWork, grid) where
\end{code}

We will need our polymorphic graph building machinery:
\begin{code}
import qualified PolyGraph.ReadOnly as Base
import qualified PolyGraph.ReadOnly.DiGraph as DiG
import PolyGraph.Common (OPair(..), PairLike(..))
import PolyGraph.Buildable ((+@))
import PolyGraph.Buildable.DiGraph ((@+~>@))
import qualified PolyGraph.Buildable as B
\end{code}

with extra help from these modules:
\begin{code}
import qualified PolyGraph.ReadOnly.DiGraph.Fold.TAMonoidFold as FastFold
import Control.Arrow ((&&&))
\end{code}

And we will show off our results at the end using graph instances found in:
\begin{code}
import qualified PolyGraph.Instances.ListGraphs as ListGraphs
\end{code}

Grid is a directed graph with edges pointing up and right. We will be using a square n x n grid with n^2 vertices.
\begin{code}
grid :: forall g v e t. (
                          B.BuildableEdgeSemantics e v,
                          DiG.DiEdgeSemantics e v,
                          B.BuildableGraphDataSet g v e t)
                                   =>  Int -> (Int -> Int -> v) -> g
grid 0 _ = B.emptyGraph
grid 1 f = B.emptyGraph +@ (f 0 0) :: (B.BuildableGraphDataSet g v e t ) => g
grid n f =
          let addHLine :: Int -> g -> g
              addHLine j g = foldr(\i -> (f i j) @+~>@ (f (i+1) j)) g [0..(n-2)]
              addVBars j g = foldr(\i -> (f i j) @+~>@ (f i (j+1))) g [0..(n-1)]
              addVLine i g = foldr(\j -> (f i j) @+~>@ (f i (j+1))) g [0..(n-2)]
              addHBars i g = foldr(\j -> (f i j) @+~>@ (f (i+1) j)) g [0..(n-1)]
          in  addHLine (n-1) . addVBars (n-2) . addVLine (n-1) . addHBars (n-2) $ grid (n-1) f
\end{code}

_Motivation and Problem Statement_: Trees and Graphs are related in more than one way.
I like to think about trees as:

_Trees are directed graphs that lack the concept of vertex equality_

Program that does a recursive logic on a directed graph and ignores vertex equality can be very inefficient.
One classic example of this is the naive (and well known) recursive program example for computing Fibonacci numbers.

_Grid graph_: If grid forgot about vertex equality, it would become a (pruned) binary tree.
In this example we will compute a comparizon between the v-size of the grid against the node size of that tree.

To keep things simple and focused, we specialize vertex type to (Int, Int) and the edge type to OPair.
First, I will try to compute the v-size of the grid, ignoring the fact that we can predict the result of n^2.
What I want is a fuction which looks like this:

 countGraphVertices:: Int -> Int
 countGraphVertices n = undefined

effectively removing the 'g' from the type signature with all associated constraints.
This is not logical and Haskell compiler will not allow me to use my 'grid' function in the implementation of
'countGraphVertices'. So I have 2 choices:

  - accept a bugus input parameter (similar to Java List.toArray) to tell compiler about 'g' and its contraints
  - include graph type in the result for the same purpose

Since the first approach is more familiar to OO coders I will run with it:

\begin{code}
countGraphVertices  :: forall g v . (B.BuildableGraphDataSet g (Int,Int) (OPair (Int,Int)) [])
                                    => g -> Int -> Int
countGraphVertices _ n = Base.vCount (toPair . DiG.resolveDiEdge) (grid n (,) :: g)
\end{code}

_Understanding the code_: in the code above, it maybe confusing why do I need to resolve edges when calculating vertex count.  It is because
vCount is implemented on the GraphDataSet level GraphDataSet 'knows' only about edges and isolatedVertices, and it does not
assume anything about the edge type.

_Code below_: I find is super cool that things like Monoid type class is part of the the language base package.
Monoids are for 'appending' things. Interestingly the obvious choice of monoid: 'the number' is not implemented
as one because it is unclear if (+) or (*) should be used. Thus, I need to create my own type and make it Monoid.

I am using a tree-like fold (PolyGraph.ReadOnly.DiGraph.Fold.TAMonoidFold) for directed graphs which allows me to collect any information from each folded vertex,
each folded edge and the monoid 'appending' is performed on each vertex across fold results from all di-adjacent edges.

\begin{code}
newtype Plus a = Plus { getSum :: a } deriving Show
instance Num a => Monoid (Plus a) where
    mempty = Plus 0
    Plus x `mappend` Plus y = Plus (x + y)

treeNodeCounter :: FastFold.MonoidFoldAccLogic v (OPair v) (Plus Int)
treeNodeCounter = FastFold.defaultMonoidFoldAccLogic {
                      FastFold.applyVertex  = const (Plus 1)
                 }

countTreeNodes:: forall g v . (DiG.DiAdjacencyIndex g (Int,Int) (OPair (Int,Int)) [],
                              B.BuildableGraphDataSet g (Int,Int) (OPair (Int,Int)) [])
                                 => g -> Int -> Int
countTreeNodes _ n = getSum $ FastFold.dfsFold (grid n (,) :: g) treeNodeCounter (0,0)
\end{code}

That allows us to wrap up our example in a recognizable Haskell pattern:

\begin{code}
runProgram :: forall g v . (DiG.DiAdjacencyIndex g (Int,Int) (OPair (Int,Int)) [],
                               B.BuildableGraphDataSet g (Int,Int) (OPair (Int,Int)) [])
                                     =>  Int -> g -> [(Int, Int)]
runProgram n g = map (countGraphVertices g &&& countTreeNodes g) [1..n]
\end{code}

_Understanding the code_: If you do not know what &&& try to first guess what it does by looking
at the type signature and the rest of the code.
Here I could replace the funtion used by map with

   \i -> (countGraphVertices g i, countTreeNodes g i)

and &&& makes the code more point-free.

We need something to signal the type.

\begin{code}
on :: forall g v e t.  B.BuildableGraphDataSet g v e t => g
on = B.emptyGraph
\end{code}

And now we can run the comparizon.  Notice that the resulting numbers are different depending
on the choice of instance type.  This is why types are needed at some point in the program.
\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
    putStrLn "Compare grid graph v-size with corresponding binary tree node-size:"
    putStrLn $ show (runProgram 10 (on:: ListGraphs.Edges (Int,Int) (OPair (Int,Int))))
    putStrLn "Same comparizon using Vertices instance which drops edges:"
    putStrLn $ show (runProgram 10 (on:: ListGraphs.Vertices (Int,Int) (OPair (Int,Int))))
\end{code}
