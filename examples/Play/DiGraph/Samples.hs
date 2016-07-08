module Play.DiGraph.Samples where

import Play.DiGraph.Types
import qualified Data.HashSet as HS

-- simple test data (list of pars that will serve as edges)
testEdges = [
        ("a0", "a01"),
        ("a0", "a02"),
        ("a01", "a1"),
        ("a02", "a1"),
        ("a1", "a11"),
        ("a1", "a12"),
        ("a11", "a2"),
        ("a12", "a2")
       ]

--
-- notice SimpleGraph is not specialized to String type
--
playTwoDimondsSetGraph :: SimpleGraph String HS.HashSet
playTwoDimondsSetGraph = SimpleGraph (HS.fromList testEdges)

playTwoDimonds :: SimpleGraph String []
playTwoDimonds = SimpleGraph testEdges

playFirstLastTxt:: String
playFirstLastTxt = "a implies b\n" ++
                "a implies c\n" ++
                "b implies d\n" ++
                "c implies d\n" ++
                "d implies e\n" ++
                "a implies f\n"

playFirstLast = Theory playFirstLastTxt
