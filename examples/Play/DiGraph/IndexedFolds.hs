------
-- Examples demostrate converting D-Graph that would be slow to
-- fast CIndex/DiGraph
------
module Play.DiGraph.IndexedFolds where

import Data.Hashable
import qualified PolyGraph.DiGraph.Indexers as I
import PolyGraph.DiGraph
import PolyGraph.DiGraph.TreeFold
import qualified Play.DiGraph.Types as T
import qualified Play.DiGraph.Samples as S (playFirstLast)
import qualified Data.HashSet as HS

------
-- Our play example is set of setences (lines) that define implications.
-- First word implies Last Word.  Logically Lines are edges and First-Last words are adjecent vertices
-- Here edges are converted to pre-parsed pairs and HashMap based CIndex is used for fast calculations.
------
playGraph :: I.DiGraphHelper T.FirstLastWord T.FirstLastLine []
playGraph = I.buidDiGraph T.firstLastWordInLine (T.firstLastWordTextLines S.playFirstLast)

playCIndex :: I.CIndexHelper T.FirstLastWord (I.DEdgeHelper T.FirstLastLine T.FirstLastWord) []
playCIndex = I.buildHmCIndex playGraph

-- this counts edges as if graph was expanded to a tree
allImplications ::forall e v . (Hashable v, Eq v) => FoldAccLogic [] v e (HS.HashSet v)
allImplications = FoldAccLogic {
       applyEdge   = const id,
       applyVertex = HS.insert,
       aggregate   = mconcat,
       handleCycle = Right . HS.singleton
    }

playAllImplications:: String -> [String]
playAllImplications word =
           map (T.getWordT) . HS.toList $ dfsFold
                         playCIndex
                         (allImplications :: FoldAccLogic [] T.FirstLastWord (I.DEdgeHelper T.FirstLastLine T.FirstLastWord) (HS.HashSet T.FirstLastWord))
                         (T.FirstLastWord(word))

experiments = [playAllImplications "a", playAllImplications "d"]
