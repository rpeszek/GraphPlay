------
-- Examples demostrate converting D-Graph that would be slow to
-- fast CIndex/DiGraph
------
module Play.DiGraph.IndexedFolds where

import Data.Hashable
import qualified PolyGraph.DiGraph.Indexers as I
import PolyGraph.DiGraph
import PolyGraph.DiGraph.TAFold
import qualified Play.DiGraph.Types as T
import qualified Play.DiGraph.Samples as S (playFirstLast)
import qualified Data.HashSet as HS

------
-- Our play example is set of setences (lines) that define implications.
-- First word implies Last Word.  Logically Lines are edges and First-Last words are adjecent vertices
-- Here edges are converted to pre-parsed pairs and HashMap based CIndex is used for fast calculations.
------
playGraph :: I.DiGraphHelper T.Statement T.Implication []
playGraph = I.buidDiGraph T.statementsInImplication (T.implicationsInTheory S.playFirstLast)

playCIndex :: I.CIndexHelper T.Statement (I.DEdgeHelper T.Implication T.Statement) []
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
           map (T.getStatementText) . HS.toList $ dfsFold
                         playCIndex
                         (allImplications :: FoldAccLogic [] T.Statement (I.DEdgeHelper T.Implication T.Statement) (HS.HashSet T.Statement))
                         (T.Statement(word))

experiments = [playAllImplications "a", playAllImplications "d"]
