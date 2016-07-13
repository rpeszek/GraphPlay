
module PolyGraph.ReadOnly.Graph where --exports everything, a terrible programmer wrote it

import Data.List (nub, length)
import PolyGraph.Common.Helpers

--
-- e are edges v are vertices, the order of (v,v) does not imply ordering of vertices
-- Graph FLWordText term would be: incidence function
--
class EdgeSemantics e v | e -> v where
  resolveEdge      ::  e -> (v,v)

class (Eq v, Foldable t)  => GraphDataSet g v e t | g -> t, g -> v, g -> e where
  isolatedVertices ::  g -> t v
  edges            ::  g -> t e

  -- | edge count
  e    ::  g -> Int
  e g  =  length . edges $ g

class (EdgeSemantics e v, GraphDataSet g v e t) => Graph g v e t


defaultVertexCount :: forall g v e t. (GraphDataSet g v e t) => (e -> (v,v)) -> g -> Int
defaultVertexCount f g =
     let isolatedVCount = length . isolatedVertices $ g
         appendVertices :: e -> [v] -> [v]
         appendVertices e list =
                              let (v1, v2) = f e
                              in  v1 : v2 : list
         nonIsolatedVCount = length . nub $ foldr appendVertices [] (edges g)
     in  isolatedVCount + nonIsolatedVCount

-- where
--  degree  :: g -> v -> Int
--  degree  = degreeSlow


-- degreeSlow :: forall g v e t . (EdgeSemantics e v, GraphDataSet g v e t) => g -> v -> Int
-- degreeSlow = undefined

-- ?? union ::
-- ?? intersection ::

--TODO make DiEdgeSemantics EdgeSemantics or have common ancestor
  --     make DiGraph a Graph
  --     byparite
