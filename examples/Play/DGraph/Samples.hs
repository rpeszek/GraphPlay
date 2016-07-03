module Play.DGraph.Samples where

import Play.DGraph.Types

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
playTwoDimonds :: SimpleGraph String []
playTwoDimonds = SimpleGraph testEdges
