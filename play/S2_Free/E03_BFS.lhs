2.01 Free Polymorphism.  DSL for Breadth-first Search Traversal, Part 2.
------
This example does not expand my Free progress but GraphPlay is about polymorphism and in last
example I have calculated integer distance, how polymorphic is that?  Clearly not very. 
\begin{code}
module S2_Free.E03_BFS (allThisHardWork) where
\end{code}

We will expanding on previous example
\begin{code}
import qualified S2_Free.E02_BFS as E02
\end{code}

We need the same imports
\begin{code}
import FreeDSL.BFS.VTraversal
import qualified FreeDSL.BFS.Interpreter as Interpreter
import Control.Monad.Loops
import qualified Instances.ListGraphs as ListGraphs
import qualified Test.QuickCheck as Property
import Control.Applicative
\end{code}

E02 computation started at annotating root vertex with 0 and was performed this at every vertex: 
```
adjustAnnotation (+ 1)
```
Each vertex contributed some information (integer weight of 1) and that information knew how to 
aggregate itself (+1).  
This is a clear path to generalization: To replace Integer with type 'a'
I need some annotating function annF :: v -> a and the new type a needs have monoid characteristics
(has generalized '0' and generalized (+)):
\begin{code}
generalizedDistance :: (Eq v, Monoid a) => (v -> a) -> v -> v -> VTraversal a v (Maybe a)
generalizedDistance annF root to = 
     (rootWithAnnotation root mempty) 
     >> untilM_ ( nextVertex >>= maybe (return Nothing) (appendAnnotation . annF))
        (E02.termination to)
     >> getAnnotationAt to
\end{code}

To recover what I already had, I need to tell Haskell that I want to aggregate Integers with plus:
\begin{code}
newtype Plus = Plus { getSum :: Int } deriving Show
instance  Monoid (Plus) where
    mempty = Plus 0
    Plus x `mappend` Plus y = Plus (x + y)
\end{code}

Now I can check that my generalized function produces produces the same results:
\begin{code}
distanceFrom00'' to = getSum <$> Interpreter.runBFS (generalizedDistance (const $ Plus 1) (0,0) to) E02.myGraph

sameAsBefore :: IO()
sameAsBefore = Property.quickCheck $ 
         E02.sameAsAddingCoordinates distanceFrom00''
\end{code}

And this is why I love Haskell, same code on list monoid:
\begin{code}
shortestPathFrom00 to = Interpreter.runBFS (generalizedDistance ((\v -> [v])) (0,0) to) E02.myGraph
\end{code}

So here we have it:
\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
  putStrLn "Distance from 00 to 55 is: \n"
  putStrLn $ show $ distanceFrom00'' (5,5)
  putStrLn "And shortest path is: \n" 
  putStrLn $ show $ shortestPathFrom00 (5,5)
\end{code}
