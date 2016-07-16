module Play.DiGraph.SampleInstances.FirstLastWord (
     FLWordSentence(..)
   , FLWord(..)
   , FLWordText(..)
   , fLWordsInFLWordSentence
   , fLWordSentencesInFLWordText
) where

import PolyGraph.ReadOnly.Graph
import PolyGraph.ReadOnly.DiGraph
import PolyGraph.Buildable.GDSBuild
import PolyGraph.Common.Helpers
import Data.List (nub, null, lines, words, concat)
import qualified PolyGraph.ReadOnly.DiGraph.Optimize.HashMapDiGraphConversion as INX
import qualified Data.Hashable as HASH
import qualified Data.HashSet as HS
import qualified Data.Foldable as F

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

fLWordsInFLWordSentence :: FLWordSentence -> HPair FLWord
fLWordsInFLWordSentence line =
          let lineTxt = getFLWordSentenceText line
              wordTexts = words lineTxt
              firstFLWord = if (null wordTexts)
                then ""
                else (head wordTexts)
              lastFLWord  = if (null wordTexts)
                then ""
                else (last wordTexts)
          in HPair (FLWord{getFLWordText = firstFLWord}, FLWord{getFLWordText = lastFLWord})

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
  isolatedVertices  = const []  -- in this graph there are no isolated vertices each word comes from a sentence

instance DiGraph FLWordText FLWord FLWordSentence []

instance BuildableGraphDataSet FLWordText FLWord FLWordSentence [] where
  empty = FLWordText ""
  g @+ statment       = g   -- TODO currently FLWordText does not care about FLWords that do not imply anything
  g ~+ flWordSentence = let newText = (getFLWordSentenceText flWordSentence) ++ "\n" ++ (getFLWordTextText g)
                        in FLWordText newText
  union g1 g2 = FLWordText $ (getFLWordTextText g1) ++ (getFLWordTextText g2)
