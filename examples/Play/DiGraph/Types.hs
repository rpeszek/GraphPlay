module Play.DiGraph.Types (
   SimpleGraph(..)
   , SimpleListGraph
   , SimpleSetGraph
   , FLWordSentence(..)
   , FLWord(..)
   , FLWordText(..)
   , fLWordsInFLWordSentence
   , fLWordSentencesInFLWordText
) where

import PolyGraph.Graph
import PolyGraph.DiGraph
import PolyGraph.Graph.PolyBuild
import PolyGraph.Helpers
import Data.List (nub, null, lines, words, concat)
import qualified PolyGraph.DiGraph.Indexers as INX
import qualified Data.Hashable as HASH
import qualified Data.HashSet as HS
import qualified Data.Foldable as F

--
-- Simple implemenation of DiGraph
-- Note: getEdges is like a getter you can obtain list of pairs encapsulated
-- in SimpleGraph sg by calling 'getEdges sg'
--
data SimpleGraph v t = SimpleGraph { getEdges:: t (v,v), getDisconnectedVertices:: t v}
type SimpleListGraph v = SimpleGraph v []
type SimpleSetGraph v = SimpleGraph v HS.HashSet

-- INSTANCES --
--foldMap :: Monoid m => (a -> m) -> t a -> m
instance forall v t . (Show v, Foldable t) => Show (SimpleGraph v t) where
  show g =  let looseVerticesS = F.foldMap (\v -> show(v)++",") (getDisconnectedVertices g)
                looseVerticesD = if looseVerticesS == ","
                                 then ""
                                 else "Loose Vertices: " ++ looseVerticesS ++ "\n"
                edgesS = F.foldMap (\vv -> " " ++ show(vv)++ "\n") (getEdges g)
                edgesD = if edgesS == []
                         then "No Edges"
                         else "Edges: \n" ++ edgesS
            in looseVerticesD ++ edgesD

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
----------------------------------
-- Text represents a graph where each sentence/line is an edge
-- and first and last word in this line is a vertex
----------------------------------
newtype FLWord            = FLWord       { getFLWordText:: String }        deriving (Show, Eq)
newtype FLWordSentence    = FLWordSentence { getFLWordSentenceText:: String }  deriving (Show, Eq)
newtype FLWordText        = FLWordText { getFLWordTextText :: String } deriving (Show, Eq)

toFLWordText :: String -> FLWordText
toFLWordText text = FLWordText text

--getFLWordText :: FLWord -> String
--getFLWordText = id

instance HASH.Hashable(FLWord) where
  hashWithSalt salt x = HASH.hashWithSalt salt (getFLWordText x)

instance FromString (FLWord) where
  fromString s = FLWord s

fLWordsInFLWordSentence :: FLWordSentence -> (FLWord, FLWord)
fLWordsInFLWordSentence line =
          let lineTxt = getFLWordSentenceText line
              wordTexts = words lineTxt
              firstFLWord = if (null wordTexts)
                then ""
                else (head wordTexts)
              lastFLWord  = if (null wordTexts)
                then ""
                else (last wordTexts)
          in (FLWord{getFLWordText = firstFLWord}, FLWord{getFLWordText = lastFLWord})

fLWordSentencesInFLWordText :: FLWordText -> [FLWordSentence]
fLWordSentencesInFLWordText text =  map(FLWordSentence) . lines . getFLWordTextText $ text

--
--Working with FLWordText directly will be slow but it is a digraph anyway
--

instance DiEdgeSemantics FLWordSentence FLWord where
  resolveDiEdge  = fLWordsInFLWordSentence

instance BuildableEdgeSemantics FLWordSentence FLWord where
  defaultEdge s1 s2 = FLWordSentence ((getFLWordText s1) ++ " implies " ++ (getFLWordText s2))

instance GraphDataSet FLWordText FLWord FLWordSentence [] where
  edges     = fLWordSentencesInFLWordText
  vertices  = concat . map (\(a,b) -> [a,b]) . map (fLWordsInFLWordSentence) . fLWordSentencesInFLWordText

instance DiGraph FLWordText FLWord FLWordSentence []

-- TODO this can insert duplicate vertices and edges
instance BuildableGraphDataSet FLWordText FLWord FLWordSentence [] where
  empty = FLWordText ""
  g @+ statment      = g   -- TODO currently FLWordText does not care about FLWords that do not imply anything
  g ~+ flWordSentence = let newText = (getFLWordSentenceText flWordSentence) ++ "\n" ++ (getFLWordTextText g)
                        in FLWordText newText
