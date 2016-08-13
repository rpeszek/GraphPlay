2.01 Free Polymorphism.  DLS for BFS traversal.
------
A fascinating question to me is: Is it possible to write a traversal program agnostic
to which algorithm is being used.  I believe the answer, in general, is no, but the 
question has more depth and I will try to examine it closer in the future. 

First problem in my quest for a universal API is that some graph traversals care about edges
and some care only about vertices. 
Computations which care about edges (like computations on a weighted graph) tend to be
more general but also slower since there are typically more edges (O(v^2)) than vertices. 

In this example we will look at BFS traversal which is vertex-centric.
\begin{code}
module S2_Free.E02_BFS (allThisHardWork) where
\end{code}

DLS and interpreter are defined in:
\begin{code}
import FreeDSL.VTraversal
import qualified FreeDSL.VTraversal.BFS as Interpreter
\end{code}

I will need these to spell in my language:
\begin{code}
import Control.Monad
import Control.Monad.Loops
\end{code}

And I will check the results using:
\begin{code}
import S1_Cstr.E05_Samples (grid)
import qualified Instances.ListGraphs as ListGraphs
import qualified Test.QuickCheck as Property
\end{code}

The traversal DSL is defined as VTraversal a v r type where 
 * a - what is being accumulated
 * v - vertex type
 * r - computation result type
Notice that this does not include graph type or edge type. I see it as a double edge 
sward in Free Monad DSL design. Type safety becomes a design factor to think about. 
Again, I hope to dwell on this more in the future.

My DSL has about 15 commands, some of them should become clear when looking at code that follows.

We will be computing distance on an unweighted (undirected) graph. 
BSF traversal needs to terminate when the 'to' node is reached or when BSF exhausts 
all vertices.  This is expressed here:

\begin{code}
termination :: Eq v => v -> VTraversal a v Bool
termination v = currentVertex >>= return . maybe True (== v)
\end{code}

Here is the distance calculation using my DSL
\begin{code}
computeDistance :: (Eq v) => v -> v -> VTraversal Int v (Maybe Int)
computeDistance root to = 
     (rootWithAnnotation root 0) 
     >> untilM_ ( nextVertex >> adjustAnnotation (+ 1))
        (termination to)
     >> getAnnotationAt to
\end{code}

To see more what is going on, here is arguably uglier but a more spelled out version:
\begin{code}
computeDistance' :: (Eq v) => v -> v -> VTraversal Int v (Maybe Int)
computeDistance' root to = do
     rootAt root
     annotateAt root 0 
     untilM_ (do
        obs <- nextObservation
        case obs of
          NoMore        -> return ()
          Observe v1 v2 -> 
            do
              runningDist <- getAnnotationAt v1
              case runningDist of 
                Nothing -> return ()
                Just dist  -> annotateAt v2 (dist+1)
      ) (termination to)
     getAnnotationAt to
\end{code}

The semantics of my DSL gives client program ability to annotate vertices with type a (here Int).
This can be done using explicit annotateAt v, getAnnotationAt v methods or using:
annotate, getAnnotation, adjustAnnotation methods where the v-s are implicit. 

In BFS, when observing edge (v1,v2) the client should always annotate/write 'the front' (v2) and 
read the back (v1). This way (unless the graph is an isolated vertex) reading is never Nothing.
Since reading from the back and writing to the front is the rule here the implicit methods do just that.  
adjustAnnotation method is an interesting one:

```
adjustAnnotation :: (a -> a) ->  VTraversal a v (Maybe a)
```
When observing (v1, v2) it applies function to the v1 annotation and uses the result to annotate v1.

We will test our program using square grid graph of size 10 (from Example 1.05):
\begin{code}
myGraph = grid 10 (,) :: ListGraphs.GEdges (Int, Int)
\end{code}

And use interpreter to wire the BFS traversal:
\begin{code}
distanceFromRoot  to = Interpreter.runBFS (computeDistance  (0,0) to) myGraph
distanceFromRoot' to = Interpreter.runBFS (computeDistance' (0,0) to) myGraph
\end{code}

Distance on a grid has an obvious formula (Range is used to implement confinement to 
size 10 grid):
\begin{code}
mightHaveGuessed :: (Range, Range) -> Bool
mightHaveGuessed to =  
             let (Range i, Range j) = to
             in distanceFromRoot (i,j) == Just(i + j) &&
                distanceFromRoot'(i,j) == Just(i + j)

newtype Range = Range Int deriving Show

instance Property.Arbitrary Range where
  arbitrary = liftM Range (Property.choose (0, 9) :: Property.Gen Int) 
\end{code}

This provides a nice property to test my computation:

This provides a nice property to test my computation:
\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
    putStrLn "Distance works:"
    Property.quickCheck mightHaveGuessed
\end{code}
