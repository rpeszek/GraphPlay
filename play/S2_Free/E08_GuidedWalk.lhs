-- WORK in progress

\begin{code}
module S2_Free.E08_GuidedWalk where
\end{code}

\begin{code}
import Control.Monad
import Control.Monad.Free (Free(..))
import PolyGraph.Common.DslSupport (interpretInM)
import PolyGraph.Common.DslSupport.Coproduct ((:+:))
\end{code}

\begin{code}
import FreeDSL.VWalk
import S2_Free.E07_ChoiceDSL
\end{code}

\begin{code}
import Control.Monad.State.Strict (runStateT)
import PolyGraph.ReadOnly.Graph (AdjacencyIndex(..))
\end{code}

\begin{code}
import qualified Instances.ListGraphs as ListGraphs
import S1_Cstr.E05_Samples (bipartiteGraph)
\end{code}

\begin{code}
type GuidedWalkDSL v  = Free ((VWalkInstructions v) :+: (ChoiceInstructions v))
\end{code}

\begin{code}
{-
interpretGuidedWalk :: forall g v e t r. (Show v, Read v, Eq v, AdjacencyIndex g v e t) =>
                               GuidedWalkDSL v r -> g  -> StateT [v] IO r
interpretGuidedWalk prog g  = interpretInM g prog
-}

runGuidedWalkFull :: forall  g v e t r  . (Show v, Read v, Eq v, AdjacencyIndex g v e t) => 
                               GuidedWalkDSL v r -> g -> v -> IO (r, [v])
runGuidedWalkFull program g v = runStateT (interpretInM g program) [v] -- ([] is empty initial state)

runGuidedWalk :: forall g v e t r . (Show v, Read v, Eq v,  AdjacencyIndex g v e t) => 
                               GuidedWalkDSL v r -> g -> v -> IO r
runGuidedWalk program g  =  liftM fst . runGuidedWalkFull program g
\end{code}

\begin{code}
{-
Note ugly type hint
-}
guidedStep ::  forall v.  GuidedWalkDSL v ()
guidedStep  = do
     neighVs <- getNeighbors                        -- WalkDSL
     next    <- choose neighVs :: GuidedWalkDSL v v -- ChoiceDSL
     walkTo next                                    -- WalkDSL
     return ()
\end{code}

\begin{code}
sampleWalk ::  Int -> GuidedWalkDSL v [v]
sampleWalk steps = do
   forM_ [1..steps] (const guidedStep)
   walk <- history                                  -- WalkDSL
   return walk
\end{code}

\begin{code}
testGraph = bipartiteGraph ([0..3], [10..11]) :: ListGraphs.GEdges Int

testWalk :: IO [Int]
testWalk = runGuidedWalk (sampleWalk 3) testGraph 0 
\end{code}
