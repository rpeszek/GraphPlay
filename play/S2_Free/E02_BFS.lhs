2.02 Free Polymorphism.  DSL for Breadth-first Search Traversal.
------
In this example I will look at a DSL that consumes BFS traversal.

  > [Wikipedia](https://en.wikipedia.org/wiki/Breadth-first_search) has nice imperative pseudo-code for graph BFS.
 
\begin{code}
module S2_Free.E02_BFS (
  allThisHardWork
  , myGraph
  , termination
  , Range(..)
  , sameAsAddingCoordinates 
) where
\end{code}

DLS for consuming traversal and interpreter for it are defined in:
\begin{code}
import FreeDSL.BFS.VTraversal
import qualified FreeDSL.BFS.Interpreter as Interpreter
\end{code}

I will need these to spell in my language:
\begin{code}
import Data.Maybe (fromJust)
import Control.Monad
import Control.Monad.Loops
\end{code}

And I will check the results using:
\begin{code}
import S1_Cstr.E05_Samples (grid)
import qualified Instances.ListGraphs as ListGraphs
import qualified Test.QuickCheck as Property
\end{code}

The traversal DSL is defined by 'VTraversal a v r' type where 
 * a - type accumulated during traversal (annotation type)
 * v - vertex type
 * r - computation result type

Notice that this does not include graph type or edge type. I see this as a double edged sword
when using Free Monad DSL design. Type safety becomes a design factor to think about. 
I hope to dwell into this more in the future and ignore this issue for now.

My DSL has about 15 commands, some of them should become clear when looking at the code that follows.

We will be computing distance on an unweighted (undirected) graph. 
BFS traversal needs to terminate when the 'to' vertex is reached or when BFS exhausts 
all vertices.  This is expressed here as a computation:

\begin{code}
termination :: Eq v => v -> VTraversal a v Bool
termination v = currentVertex >>= return . maybe True (== v)
\end{code}

And the distance calculation is:
\begin{code}
computeDistance :: (Eq v) => v -> v -> VTraversal Int v Int
computeDistance root to = do
     (rootWithAnnotation root 0) 
     untilM_ ( nextVertex >> adjustAnnotation (+ 1))
        (termination to)
     (liftM fromJust) . getAnnotationAt $ to
\end{code}

To see more what is going on, here is arguably uglier but also a more spelled out version:
\begin{code}
computeDistance' :: (Eq v) => v -> v -> VTraversal Int v Int
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
                Nothing   -> return ()
                Just dist -> annotateAt v2 (dist+1)
      ) (termination to)
     (liftM fromJust) . getAnnotationAt $ to
\end{code}

The semantics of my DSL gives client program ability to annotate vertices with type a (here Int).
This can be done using explicit annotateAt v, getAnnotationAt v methods or using:
annotate, getAnnotation, adjustAnnotation methods where the v-s are implicit. 

In BFS, when observing edge (v1,v2) the client should always annotate/write 'the front' (v2) and 
read annotated back (v1). This way (unless the graph is an isolated vertex) reading is never Nothing.
Also vertices are only annotated once.
Since reading from the back and writing to the front is the rule, the implicit methods do just that.  
adjustAnnotation method is an interesting one:

```
adjustAnnotation :: (a -> a) ->  VTraversal a v (Maybe a)
```
When observing (v1, v2) it applies function to the v1 annotation and uses the result to annotate v2.

We will test our program using square grid graph of size 10 (from Example 1.05):
\begin{code}
type Point = (Int, Int)
myGraph = grid 10 (,) :: ListGraphs.GEdges Point
\end{code}

And use interpreter to wire the BFS traversal 
(Notice interpreter is passed graph instance, our program is not):
\begin{code}
distanceFrom00  to = Interpreter.runBFS (computeDistance  (0,0) to) myGraph
distanceFrom00' to = Interpreter.runBFS (computeDistance' (0,0) to) myGraph
\end{code}

Distance on a grid has an obvious formula (Range is used to implement a confinement to 
size 10 grid):
\begin{code}
sameAsAddingCoordinates :: (Point -> Int) -> (Range, Range) -> Bool
sameAsAddingCoordinates f point =  
             let (Range i, Range j) = point
             in f (i,j) == i + j

bothSatisfyFormula :: (Range, Range) -> Bool
bothSatisfyFormula  = (&&) <$>
                sameAsAddingCoordinates distanceFrom00 <*>
                sameAsAddingCoordinates distanceFrom00'

newtype Range = Range Int deriving Show

instance Property.Arbitrary Range where
  arbitrary = liftM Range (Property.choose (0, 9) :: Property.Gen Int) 
\end{code}

This provides a nice property to test my computation:
\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
    putStrLn "Distance calculation works for both implementations:"
    Property.quickCheck bothSatisfyFormula
\end{code}

  > Presented DSL API has a very statefull appearance. however it is all pure FP.  
    Except for stdout IO effect in allThisHardWork,
    no variables have been mutated or otherwise harmed. I think that is kinda slick.

This example provides a nice baseline for a Traversal DSL. My goal moving forward is to improve 
on that baseline.
