2.03 Free Polymorphism.  DSL for Breadth-first Search Traversal, Part 2.
------
GraphPlay is about polymorphism and in my last example I have calculated distance using integers, how lame!  
Clearly, I should be able to do better. 
\begin{code}
module S2_Free.E03_BFS (allThisHardWork, sameAsBefore) where
\end{code}

We will be expanding on my previous example
\begin{code}
import qualified S2_Free.E02_BFS as E02
\end{code}

I need the same imports
\begin{code}
import FreeDSL.BFS.VTraversal
import qualified FreeDSL.BFS.Interpreter as Interpreter
import Control.Monad.Loops
import qualified Instances.ListGraphs as ListGraphs
import qualified Test.QuickCheck as Property
\end{code}

_Generalization:_
Previous example distance calculation started by annotating root vertex with 0 and performed this at every vertex: 
```
adjustAnnotation (+ 1)
```
Each vertex contributed some information (integer weight of 1) and the information type (Int) itself knew how to 
aggregate (+).  
This has a clear path to generalization: I will replace Integer with a general type 'a'
I need some annotating function annF :: v -> a and my new type 'a' needs to have monoid characteristics
('0' becomes mempty and (+) becomes mappend):
\begin{code}
generalizedDistance :: (Eq v, Monoid a) => 
                             (v -> a) -> v -> v -> VTraversal a v (Maybe a)
generalizedDistance annF root to = 
     (rootWithAnnotation root mempty) 
     >> untilM_ ( nextVertex >>= maybe (return Nothing) (adjustAnnotation . mappend . annF))
        (E02.termination to)
     >> getAnnotationAt to
\end{code}

I need to tell Haskell to aggregate Integers using (+ and 0):
\begin{code}
newtype PlusInt = PlusInt { getSum :: Int } deriving Show
instance  Monoid (PlusInt) where
    mempty = PlusInt 0
    PlusInt x `mappend` PlusInt y = PlusInt (x + y)
\end{code}

Now I can check that my generalized function produces expected results:
\begin{code}
distanceFrom00'' to = getSum <$> Interpreter.runBFS 
                       (generalizedDistance (const $ PlusInt 1) (0,0) to) E02.myGraph

sameAsBefore :: IO()
sameAsBefore = Property.quickCheck $ 
         E02.sameAsAddingCoordinates distanceFrom00''
\end{code}

And here is why I love Haskell, same code only using list monoid instead of PlusInt:
\begin{code}
shortestPathFrom00 to = Interpreter.runBFS 
                         (generalizedDistance ((\v -> [v])) (0,0) to) E02.myGraph
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
