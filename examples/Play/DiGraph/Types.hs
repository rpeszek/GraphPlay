module Play.DiGraph.Types (
   SimpleGraph(..)
   , SimpleListGraph
   , SimpleSetGraph
   , Implication(..)
   , Statement(..)
   , Theory(..)
   , statementsInImplication
   , statementsInTheory
) where

import PolyGraph.Graph
import PolyGraph.DiGraph
import PolyGraph.Graph.Adjustable
import PolyGraph.Helpers
import Data.List (nub, null, lines, words)
import qualified PolyGraph.DiGraph.Indexers as INX
import qualified Data.Hashable as HASH
import qualified Data.HashSet as HS

--
-- Simple implemenation of DiGraph
-- Note: getEdges is like a getter you can obtain list of pairs encapsulated
-- in SimpleGraph sg by calling 'getEdges sg'
--
data SimpleGraph v t = SimpleGraph { getEdges:: t (v,v), getDisconnectedVertices:: t v}
type SimpleListGraph v = SimpleGraph v []
type SimpleSetGraph v = SimpleGraph v HS.HashSet

-- INSTANCES SimpleGraph v [] --
instance  forall v . (Eq v) => (GraphDataSet (SimpleListGraph v) v (v,v) []) where
  vertices g =  let connectedVertices = (foldr (\vv acc ->  (first' vv) : (second' vv) : acc) []) . getEdges $ g
                in nub $ connectedVertices ++ (getDisconnectedVertices g)
  edges g  =  getEdges $ g

instance forall v t. (Eq v) => (CIndex (SimpleListGraph v) v (v,v) []) where
  cEdgesOf g ver = filter (\vv -> first' vv == ver) . getEdges $ g  --(:t) g -> v -> [e]

instance  forall v . (HASH.Hashable v, Eq v) => (DiGraph (SimpleListGraph v) v (v,v) [])

-- INSTANCES SimpleGraph v HashSet --
instance  forall v . (HASH.Hashable v, Eq v) => (GraphDataSet (SimpleSetGraph v) v (v,v) HS.HashSet) where
  vertices g = let connectedVertices = (HS.foldr (\vv acc ->  (first' vv) `HS.insert` ((second' vv) `HS.insert` acc)) HS.empty) . getEdges $ g
               in connectedVertices `HS.union` (getDisconnectedVertices g)
  edges g  =  getEdges $ g

instance forall v t. (Eq v) => (CIndex (SimpleSetGraph v) v (v,v) []) where
  cEdgesOf g ver = HS.toList . HS.filter (\vv -> first' vv == ver) . getEdges $ g  --(:t) g -> v -> [e]

instance  forall v . (HASH.Hashable v, Eq v) => (DiGraph (SimpleSetGraph v) v (v,v) HS.HashSet)

-- no lenses no fun
instance  forall v . (HASH.Hashable v, Eq v) => (BuildableGraphDataSet(SimpleSetGraph v) v (v,v) HS.HashSet) where
   empty = SimpleGraph HS.empty HS.empty
   g @+ v = let newVertices = HS.insert v (getDisconnectedVertices g)
            in g {getDisconnectedVertices = newVertices}
   g ~+ (v1,v2) =
            let newVertices = HS.delete v1 $ HS.delete v2 (getDisconnectedVertices g)
                newEdges = HS.insert (v1,v2) (getEdges g)
            in g {getEdges = newEdges}

instance forall v . (HASH.Hashable v, Eq v) => (AdjustableGraphDataSet (SimpleGraph v HS.HashSet) v (v,v) HS.HashSet) where
   g @\ f = let newVertices = HS.filter f (getDisconnectedVertices g)
                newEdges = HS.filter (\vv -> (f $ first' vv) && (f $ second' vv)) (getEdges g)
            in  SimpleGraph { getEdges = newEdges, getDisconnectedVertices = newVertices}

   g ~\ f = let newEdges = HS.filter f (getEdges g)
            in  g {getEdges = newEdges}



-- TODO use Text?
newtype Implication      = Implication { getImplicationText:: String } deriving (Show, Eq)
newtype Statement      = Statement { getStatementText:: String } deriving (Show, Eq)
newtype Theory      = Theory { getTheoryText :: String } deriving (Show, Eq)

instance HASH.Hashable(Statement) where
  hashWithSalt salt x = HASH.hashWithSalt salt (getStatementText x)

statementsInImplication :: Implication -> (Statement, Statement)
statementsInImplication line =
          let lineTxt = getImplicationText line
              wordTexts = words lineTxt
              firstWord = if (null wordTexts)
                then ""
                else (head wordTexts)
              lastWord  = if (null wordTexts)
                then ""
                else (last wordTexts)
          in (Statement{getStatementText = firstWord}, Statement{getStatementText = lastWord})

statementsInTheory :: Theory -> [Implication]
statementsInTheory text =  map(Implication) . lines . getTheoryText $ text

--instance DiEdgeSemantics Implication Statement where
--  resolveDiEdge  = statementsInImplication

--instance DiGraph Theory Implication (Statement, Statement) [] where
--  edges     = statementsInTheory
--  vertices  = statementsInImplication . statementsInTheory
