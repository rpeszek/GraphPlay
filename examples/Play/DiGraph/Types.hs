module Play.DiGraph.Types (
   SimpleGraph(..)
   , FirstLastLine(..)
   , FirstLastWord(..)
   , FirstLastText(..)
   , firstLastWordInLine
   , firstLastWordTextLines
) where

import PolyGraph.DiGraph
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

instance  forall v . (Eq v) => (DiGraph (SimpleGraph v []) v (v,v) []) where
  vertices g =  nub . (foldr (\vv acc ->  (first' vv) : (second' vv) : acc) []) . getEdges $ g
  edges g  =  getEdges $ g

instance forall v t. (Eq v) => (CIndex (SimpleGraph v []) v (v,v) []) where
  cEdgesOf g ver = filter (\vv -> first' vv == ver) . getEdges $ g  --(:t) g -> v -> [e]

instance  forall v . (HASH.Hashable v, Eq v) => (DiGraph (SimpleGraph v HS.HashSet) v (v,v) HS.HashSet) where
  vertices g = (HS.foldr (\vv acc ->  (first' vv) `HS.insert` ((second' vv) `HS.insert` acc)) HS.empty) . getEdges $ g
  edges g  =  getEdges $ g

instance forall v t. (Eq v) => (CIndex (SimpleGraph v HS.HashSet) v (v,v) []) where
  cEdgesOf g ver = HS.toList . HS.filter (\vv -> first' vv == ver) . getEdges $ g  --(:t) g -> v -> [e]

-- TODO use Text?
newtype FirstLastLine      = FirstLastLine { getLineT:: String } deriving (Show, Eq)
newtype FirstLastWord      = FirstLastWord { getWordT:: String } deriving (Show, Eq)
newtype FirstLastText      = FirstLastText { getText :: String } deriving (Show, Eq)

instance HASH.Hashable(FirstLastWord) where
  hashWithSalt salt x = HASH.hashWithSalt salt (getWordT x)

firstLastWordInLine :: FirstLastLine -> (FirstLastWord, FirstLastWord)
firstLastWordInLine line =
          let lineTxt = getLineT line
              wordTexts = words lineTxt
              firstWord = if (null wordTexts)
                then ""
                else (head wordTexts)
              lastWord  = if (null wordTexts)
                then ""
                else (last wordTexts)
          in (FirstLastWord{getWordT = firstWord}, FirstLastWord{getWordT = lastWord})

firstLastWordTextLines :: FirstLastText -> [FirstLastLine]
firstLastWordTextLines text =  map(FirstLastLine) . lines . getText $ text

--instance DEdgeSemantics FirstLastLine FirstLastWord where
--  resolveVertices  = firstLastWordInLine

--instance DiGraph FirstLastText FirstLastLine (FirstLastWord, FirstLastWord) [] where
--  edges     = firstLastWordTextLines
--  vertices  = firstLastWordInLine . firstLastWordTextLines
