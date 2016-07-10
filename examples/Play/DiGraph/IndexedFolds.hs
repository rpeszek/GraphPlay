------
-- Examples demostrate converting D-Graph that would be slow to
-- fast CIndex/DiGraph
------
module Play.DiGraph.IndexedFolds where

import Data.Hashable
import qualified PolyGraph.DiGraph.Indexers as I
import PolyGraph.DiGraph
import PolyGraph.DiGraph.TAFold
import qualified Play.DiGraph.SampleInstances.FirstLastWord as T
import qualified Play.DiGraph.SampleData as S (playFirstLast)
import qualified Data.HashSet as HS

------
-- Our play example is set of setences (lines) that define FLWordSentences.
-- First word implies Last FLWord.  Logically Lines are edges and First-Last words are adjecent vertices
-- Here edges are converted to pre-parsed pairs and HashMap based CIndex is used for fast calculations.
------
playGraph :: I.DiGraphHelper T.FLWord T.FLWordSentence []
playGraph = I.buidDiGraph T.fLWordsInFLWordSentence (T.fLWordSentencesInFLWordText S.playFirstLast)

playCIndex :: I.CIndexHelper T.FLWord (I.DEdgeHelper T.FLWordSentence T.FLWord) []
playCIndex = I.buildHmCIndex playGraph

-- this counts edges as if graph was expanded to a tree
allFLWordSentences ::forall e v . (Hashable v, Eq v) => FoldAccLogic [] v e (HS.HashSet v)
allFLWordSentences = FoldAccLogic {
       applyEdge   = const id,
       applyVertex = HS.insert,
       aggregate   = mconcat,
       handleCycle = Right . HS.singleton
    }

playAllFLWordSentences:: String -> [String]
playAllFLWordSentences word =
           map (T.getFLWordText) . HS.toList $ dfsFold
                         playCIndex
                         (allFLWordSentences :: FoldAccLogic [] T.FLWord (I.DEdgeHelper T.FLWordSentence T.FLWord) (HS.HashSet T.FLWord))
                         (T.FLWord(word))

experiments = [playAllFLWordSentences "a", playAllFLWordSentences "d"]
