GraphPlay Example 4. Polymorphism to the max
------
Previous examples were more about demonstrating various weird things you can do with
type contraint based polymorphism.  It is time to actually compute something.
In this example we will compute the difference between sizes of a directed grid graph and 
a corresponding tree structure. 

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

Grid is a directed graph with edges pointing up and right. 
We will be using a square n x n grid with n^2 vertices.
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

We have 2 motivations for what we are about to do:

_Performance Study Motivation_:   
Trees and directed graphs are related in more than one way.
One way to think about trees is:
   > _Trees are directed graphs that lack the concept of vertex equality_

Programs that do recursive logic on a directed graphs and ignore vertex equality can be very inefficient.
One classic example of this is the naive (and well known) recursive program example for
computing Fibonacci numbers.  
_Grid directed graph_ without vertex equality becomes a (pruned) binary tree.
In this example I will compute a comparizon between the v-size of the grid against the size of that tree.

_Playing with Polymorphism Motivation_:  
Can we do even more than polymorphic data production? Would it be possible to
both produce and consume graphs polymorphically? Create programs that are completely agnostic
to instance types?  
The answer is: no that would not make sense because results of the calculation can depend on which
instance is used.  
We will attempt the next best thing. _Create a program that stays unaware about which instance type 
is used until the very end when the type will have to be constructed_.

_Implementation_:  
To keep things simple and focused, we specialize vertex type to (Int, Int) and the edge type to OPair.
First, I will try to compute the v-size of the grid, ignoring the fact that we can predict the result of n^2.
What I want is a fuction which looks like this:

  > countGraphVertices:: Int -> Int  
    countGraphVertices gridSize = undefined

It would be not logical, and Haskell compiler will not allow me to use my 'grid' function in the implementation of
'countGraphVertices' unless I specialize the use of 'grid' to a specific instance. This is precisely what I want to avoid.
So I have 2 choices:

  - accept a bugus input parameter (similar to Java List.toArray) to tell compiler about my polymorphic 'grid' and its contraints
  - include polymorphic grid type in the result for the same purpose

Since the first approach is more familiar to OO coders I will run with it:

\begin{code}
countGraphVertices  :: forall g v . (B.BuildableGraphDataSet g (Int,Int) (OPair (Int,Int)) [])
                                    => g -> Int -> Int
countGraphVertices _ n = Base.vCount (toPair . DiG.resolveDiEdge) (grid n (,) :: g)
\end{code}

_Understanding the code_: in the code above, it maybe confusing why do I need to resolve
edges when calculating vertex count.  It is because vCount is implemented on the GraphDataSet level.
GraphDataSet 'knows' only about edges and isolatedVertices and it needs to resolve the edges into adjacent vertices to 
compute the count.

_Code below_: I find is super cool that a thing like Monoid type class is part of the the language base library.
A short intro: Monoids are for 'appending' things. Interestingly the obvious choice of monoid: 'the number' is not a monoid instance
because it is unclear if (+) or (*) should be used for 'appending'.
Thus, I need to create my own type and make it Monoid.  
I am using a tree-like fold (PolyGraph.ReadOnly.DiGraph.Fold.TAMonoidFold) for directed graphs which 
efficiently computes any information extracted from vertices,
and edges by doing monoid 'appending' on each vertex across fold results from all di-adjacent edges.
The implemenation is smart enough to calcuate each vertex only once. 

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

  > _Sidenote_: If you do not know what &&& does you may be able to just guess it by looking 
     at the type signature and the rest of the code.
     I could have replaced the the code in parentheses with   
     (\i -> (countGraphVertices g i, countTreeNodes g i))
     &&& makes the code more point-free.

Finally, we need something to signal the type.

\begin{code}
on :: forall g v e t.  B.BuildableGraphDataSet g v e t => g
on = B.emptyGraph
\end{code}

And now we can run the comparizon. We have stayed polymorphic all the way until this point.
\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
    putStrLn "Compare grid graph v-size with corresponding binary tree node-size:"
    putStrLn $ show (runProgram 10 (on:: ListGraphs.Edges (Int,Int) (OPair (Int,Int))))
    putStrLn "Same comparizon using Vertices instance which drops edges:"
    putStrLn $ show (runProgram 10 (on:: ListGraphs.Vertices (Int,Int) (OPair (Int,Int))))
\end{code}
Notice that the resulting numbers are different depending
on the choice of instance type.  This is why types are needed at some point in the program.

And finally, if you are trying to compare this to OO, please notice that I am not constructing
data anywhere in this program, I am constructing types, and I do that at the very end.
