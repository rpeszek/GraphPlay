module S2_Free.E03_BFSx (allThisHardWork) where
import FreeDSL.BFS.VTraversal
import qualified FreeDSL.BFS.Interpreter as Interpreter
import Control.Monad
import Control.Monad.Loops
import S1_Cstr.E05_Samples (grid)
import qualified Instances.ListGraphs as ListGraphs
import qualified Test.QuickCheck as Property

import qualified S2_Free.E02_BFS as E02


generalizedDistance :: (Eq v, Monoid a) => (v -> a) -> v -> v -> VTraversal a v (Maybe a)
generalizedDistance annF root to = 
     (rootWithAnnotation root (annF root)) 
     >> untilM_ ( nextVertex >>= maybe (return Nothing) (appendAnnotation . annF))
        (E02.termination to)
     >> getAnnotationAt to

superGenDistance :: (Eq v, Applicative f, Monoid (f v)) =>  v -> v -> VTraversal (f v) v (Maybe (f v))
superGenDistance = generalizedDistance pure

newtype Plus a = Plus { getSum :: a } deriving Show
instance Num a => Monoid (Plus a) where
    mempty = Plus 0
    Plus x `mappend` Plus y = Plus (x + y)


dist  = Interpreter.runBFS (generalizedDistance (pure) (0,0) (3,3) :: VTraversal [(Int, Int)] (Int, Int) (Maybe [(Int, Int)])) E02.myGraph
dist'  = Interpreter.runBFS (superGenDistance (0,0) (3,3) :: VTraversal [(Int, Int)] (Int, Int) (Maybe [(Int, Int)])) E02.myGraph

allThisHardWork = undefined


shortestPathFrom00' :: (Int, Int) -> Maybe [(Int, Int)]
shortestPathFrom00' to = Interpreter.runBFS (generalizedDistance pure (0,0) to) E02.myGraph

-- works but pure is implemented as mempty
distanceFrom00''' to = Interpreter.runBFS (generalizedDistance (pure :: (Int, Int) -> Const Plus (Int, Int)) (0,0) to) E02.myGraph
