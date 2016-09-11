-- WORK in progress

module S2_Free.E07_ChoiceDSL where

import Control.Monad
import Control.Monad.IO.Class
import Control.Monad.Free
import PolyGraph.Common.DslSupport (execInIO, IoInterpreter(..), MInterpreterWithCtx (..), execInM)
import PolyGraph.Common.DslSupport.Coproduct -- ((:+:), (:<:), liftDSL)

import qualified Instances.ListGraphs as ListGraphs
import FreeDSL.VWalk

import Control.Monad.State.Strict
import PolyGraph.ReadOnly.Graph (AdjacencyIndex(..))

import S1_Cstr.E05_Samples (bipartiteGraph)

data ChoiceAbstInst a r =  Choose [a] (a -> r)
                                  deriving Functor
                  
type ChoiceDSL a r = Free (ChoiceAbstInst a) r


choose ::  forall a polyglot. (Functor polyglot, (ChoiceAbstInst a) :<: polyglot) 
                         => [a] -> Free polyglot a
choose list =  liftDSL $ liftF (Choose list id)

choose' :: [a] -> ChoiceDSL a a
choose' list = liftF (Choose list id)

interpretChoices :: (Show a, Read a, Eq a) => ChoiceDSL a r -> IO ()
interpretChoices (Free (Choose alist nF)) = do
      putStrLn $ "Make a pick " ++ show(alist)
      choiceStr <- getLine
      let choiceA = read choiceStr
      guard (elem choiceA alist)
      interpretChoices $ nF choiceA

interpretChoices (Pure r) = return ()

--testChoice :: forall a polyglot. (IoInterpreter polyglot, Functor polyglot, (ChoiceAbstInst a) :<: polyglot) 
--        => [a] -> Free polyglot ()
testChoice :: [a] -> ChoiceDSL a ()
testChoice alist = do
   choose alist
   testChoice alist

testIO = interpretChoices $ testChoice [0..10]

{-
instance (Show a, Read a, Eq a) => IoInterpreter (ChoiceAbstInst a) where
  interpretInIO (Choose alist nF) = do
        putStrLn $ "Make a pick " ++ show(alist)
        choiceStr <- getLine
        let choiceA = read choiceStr
        guard (elem choiceA alist)
        nF choiceA

testIO2  = execInIo $ testChoice [0..10]
-}


-- TODO change IO to t IO (transformer)
instance (Show a, Read a, Eq a, MonadIO m, MonadPlus m) => MInterpreterWithCtx c m (ChoiceAbstInst a) where
  interpretM _ (Choose alist nF) = do
        liftIO $ putStrLn $ "Make a pick " ++ show(alist)
        choiceStr <- liftIO $ getLine
        let choiceA = read choiceStr
        guard (elem choiceA alist)
        nF choiceA

testIO3 :: forall c. c -> IO()
testIO3 c = execInM c (testChoice [0..10]) 

--not really needed to get it working, work with explicit :+: types
{-
testChoice' :: forall a polyglot. (Functor polyglot, (ChoiceAbstInst a) :<: polyglot) 
        => [a] -> Free polyglot ()
testChoice' alist = do
   choose alist
   testChoice' alist
-}


--testIO4 :: forall c. c -> IO()
--testIO4 c = execInM c (testChoice' [0..10]) 
type GuidedWalkDSL v  = Free ((VWalkInstructions v) :+: (ChoiceAbstInst v))

{-
interpretGuidedWalk :: forall g v e t r. (Show v, Read v, Eq v, AdjacencyIndex g v e t) =>
                               GuidedWalkDSL v r -> g -> v -> StateT [v] IO r
interpretGuidedWalk prog g v = execInM (g,v) prog
-}

runGuidedWalkFull :: forall  g v e t r  . (Show v, Read v, Eq v, AdjacencyIndex g v e t) => 
                               GuidedWalkDSL v r -> g -> v -> IO (r, [v])
runGuidedWalkFull program g v = runStateT (execInM (g,v) program) [v] -- ([] is empty initial state)

runGuidedWalk :: forall g v e t r . (Show v, Read v, Eq v,  AdjacencyIndex g v e t) => 
                               GuidedWalkDSL v r -> g -> v -> IO r
runGuidedWalk program g  =  liftM fst . runGuidedWalkFull program g

walkDSL = id
choiceDSL :: ChoiceDSL v r -> GuidedWalkDSL v r
choiceDSL = liftRight

guidedStep ::  GuidedWalkDSL v ()
guidedStep  = do
     neighVs <- walkDSL $ getNeighbors
     next <- choiceDSL $ choose' neighVs
     walkDSL $ walkTo next
     return ()

sampleWalk ::  Int -> GuidedWalkDSL v [v]
sampleWalk steps = do
   forM_ [1..steps] (const guidedStep)
   walk <- history
   return walk

testGraph = bipartiteGraph ([0..3], [10..11]) :: ListGraphs.GEdges Int

testWalk :: IO [Int]
testWalk = runGuidedWalk (sampleWalk 3) testGraph 0 
