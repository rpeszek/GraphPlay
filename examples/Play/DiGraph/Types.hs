module Play.DiGraph.Types (
   SimpleGraph(..)
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
newtype SimpleGraph v t = SimpleGraph { getEdges:: t (v,v)}

-- INSTANCES SimpleGraph v [] --
instance  forall v . (Eq v) => (GraphDataSet (SimpleGraph v []) v (v,v) []) where
  vertices g =  nub . (foldr (\vv acc ->  (first' vv) : (second' vv) : acc) []) . getEdges $ g
  edges g  =  getEdges $ g

instance forall v t. (Eq v) => (CIndex (SimpleGraph v []) v (v,v) []) where
  cEdgesOf g ver = filter (\vv -> first' vv == ver) . getEdges $ g  --(:t) g -> v -> [e]

instance  forall v . (HASH.Hashable v, Eq v) => (DiGraph (SimpleGraph v []) v (v,v) [])

-- INSTANCES SimpleGraph v HashSet --
instance  forall v . (HASH.Hashable v, Eq v) => (GraphDataSet (SimpleGraph v HS.HashSet) v (v,v) HS.HashSet) where
  vertices g = (HS.foldr (\vv acc ->  (first' vv) `HS.insert` ((second' vv) `HS.insert` acc)) HS.empty) . getEdges $ g
  edges g  =  getEdges $ g

instance forall v t. (Eq v) => (CIndex (SimpleGraph v HS.HashSet) v (v,v) []) where
  cEdgesOf g ver = HS.toList . HS.filter (\vv -> first' vv == ver) . getEdges $ g  --(:t) g -> v -> [e]

instance  forall v . (HASH.Hashable v, Eq v) => (DiGraph (SimpleGraph v HS.HashSet) v (v,v) HS.HashSet)



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
