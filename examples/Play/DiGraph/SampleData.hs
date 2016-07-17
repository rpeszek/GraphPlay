module Play.DiGraph.SampleData where

import PolyGraph.Common.Helpers
import qualified PolyGraph.Instances.SimpleGraph as SG
import qualified Play.DiGraph.SampleInstances.FirstLastWord as FL
import qualified Data.HashSet as HS

-- simple test data (list of pars that will serve as edges)
testEdges = map(OPair) [
        ("a0", "a01"),
        ("a0", "a02"),
        ("a01", "a1"),
        ("a02", "a1"),
        ("a0", "a1"),
        ("a1", "a11"),
        ("a1", "a12"),
        ("a11", "a2"),
        ("a12", "a2"),
        ("a1",  "a2")
       ]

--
-- notice SimpleGraph is not specialized to String type
--
playTwoDiamondsSetGraph :: SG.SimpleSetDiGraph String
playTwoDiamondsSetGraph = SG.SimpleGraph (HS.fromList testEdges) HS.empty

playTwoDiamonds :: SG.SimpleListDiGraph String
playTwoDiamonds = SG.SimpleGraph testEdges []

playFirstLastTxt:: String
playFirstLastTxt = "a implies b\n" ++
                "a implies c\n" ++
                "b implies d\n" ++
                "c implies d\n" ++
                "d implies e\n" ++
                "a implies f\n"

playFirstLast = FL.FLWordText playFirstLastTxt
