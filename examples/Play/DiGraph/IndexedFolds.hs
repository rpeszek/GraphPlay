------
-- Examples demostrate converting D-Graph that would be slow to
-- fast DiAdjacencyIndex/DiGraph
------
module Play.DiGraph.IndexedFolds where

import Data.Hashable
import PolyGraph.Common
import qualified PolyGraph.ReadOnly.DiGraph.Optimize.HashMapDiGraphConversion as I
import qualified PolyGraph.ReadOnly.DiGraph.Optimize.MaterializedEdge as ME
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.ReadOnly.DiGraph.Fold.TAFold
import qualified PolyGraph.Instances.DiGraph.DiEdgesByVertexMap as HTG
import qualified Play.DiGraph.SampleInstances.FirstLastWord as T
import qualified Play.DiGraph.SampleData as S (playFirstLast)
import qualified Data.HashSet as HS

------
-- Our play example is set of setences (lines) that define FLWordSentences.
-- First word implies Last FLWord.  Logically Lines are edges and First-Last words are adjecent vertices
-- Here edges are converted to pre-parsed pairs and HashMap based DiAdjacencyIndex is used for fast calculations.
------

playGraph :: HTG.DiEdgesByVertexMap T.FLWord (ME.EdgeHelper T.FLWordSentence T.FLWord) []
playGraph = I.buildDiEdgesByVertexMap T.fLWordsInFLWordSentence S.playFirstLast

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
                         playGraph
                         (allFLWordSentences :: FoldAccLogic [] T.FLWord (ME.EdgeHelper T.FLWordSentence T.FLWord) (HS.HashSet T.FLWord))
                         (T.FLWord(word))

experiments = [playAllFLWordSentences "a", playAllFLWordSentences "d"]
