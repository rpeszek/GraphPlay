module Play.DGraph.Types (
   SimpleGraph(..)
   , FirstLastLine(..)
   , FirstLastWord(..)
   , FirstLastText(..)
   , firstLastWordInLine
   , firstLastWordTextLines
) where

import PolyGraph.DGraph
import PolyGraph.Helpers
import Data.List (nub, null, lines, words)
import qualified PolyGraph.DGraph.Indexers as INX
import qualified Data.Hashable as HASH


-- let's create a very simple (and slow)  of CIndex class for testing
-- Note: getEdges is like a getter you can obtain list of pairs encapsulated
-- in SimpleGraph sg by calling 'getEdges sg'
--
newtype SimpleGraph v t = SimpleGraph { getEdges:: t (v,v)}

instance  forall v . (Eq v) => (DGraph (SimpleGraph v []) v (v,v) []) where
  vertices g =  nub . (foldr (\vv acc ->  (first' vv) : (second' vv) : acc) []) . getEdges $ g
  edges g  =  getEdges $ g

instance forall v t. (Eq v) => (CIndex (SimpleGraph v []) v (v,v) []) where
  cEdgesOf g ver = filter (\vv -> first' vv == ver) . getEdges $ g  --(:t) g -> v -> [e]

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

--instance DGraph FirstLastText FirstLastLine (FirstLastWord, FirstLastWord) [] where
--  edges     = firstLastWordTextLines
--  vertices  = firstLastWordInLine . firstLastWordTextLines
