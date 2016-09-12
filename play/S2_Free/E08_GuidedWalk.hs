-- WORK in progress

module S2_Free.E08_GuidedWalk where

import Control.Monad
import Control.Monad.Free (Free(..))
import PolyGraph.Common.DslSupport (execInM)
import PolyGraph.Common.DslSupport.Coproduct ((:+:))

import FreeDSL.VWalk
import S2_Free.E07_ChoiceDSL

import Control.Monad.State.Strict (runStateT)
import PolyGraph.ReadOnly.Graph (AdjacencyIndex(..))

import qualified Instances.ListGraphs as ListGraphs
import S1_Cstr.E05_Samples (bipartiteGraph)


type GuidedWalkDSL v  = Free ((VWalkInstructions v) :+: (ChoiceInstructions v))

{-
interpretGuidedWalk :: forall g v e t r. (Show v, Read v, Eq v, AdjacencyIndex g v e t) =>
                               GuidedWalkDSL v r -> g  -> StateT [v] IO r
interpretGuidedWalk prog g  = execInM g prog
-}

runGuidedWalkFull :: forall  g v e t r  . (Show v, Read v, Eq v, AdjacencyIndex g v e t) => 
                               GuidedWalkDSL v r -> g -> v -> IO (r, [v])
runGuidedWalkFull program g v = runStateT (execInM g program) [v] -- ([] is empty initial state)

runGuidedWalk :: forall g v e t r . (Show v, Read v, Eq v,  AdjacencyIndex g v e t) => 
                               GuidedWalkDSL v r -> g -> v -> IO r
runGuidedWalk program g  =  liftM fst . runGuidedWalkFull program g


guidedStep ::  forall v.  GuidedWalkDSL v ()
guidedStep  = do
     neighVs <- getNeighbors                        -- WalkDSL
     next    <- choose neighVs :: GuidedWalkDSL v v -- ChoiceDSL
     walkTo next                                    -- WalkDSL
     return ()

sampleWalk ::  Int -> GuidedWalkDSL v [v]
sampleWalk steps = do
   forM_ [1..steps] (const guidedStep)
   walk <- history                                  -- WalkDSL
   return walk


testGraph = bipartiteGraph ([0..3], [10..11]) :: ListGraphs.GEdges Int

testWalk :: IO [Int]
testWalk = runGuidedWalk (sampleWalk 3) testGraph 0 
