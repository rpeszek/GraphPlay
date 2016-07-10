
module PolyGraph.ReadOnly.Graph where --exports everything, a terrible programmer wrote it

--
-- e are edges v are vertices, the order of (v,v) does not imply ordering of vertices
-- Graph FLWordText term would be: incidence function
--
class EdgeSemantics e v | e -> v where
  resolveEdge      ::  e -> (v,v)

class (Eq v, Foldable t)  => GraphDataSet g v e t | g -> t, g -> v, g -> e where
  vertices ::  g -> t v
  edges    ::  g -> t e
  e        ::  g -> Int                --edge count
  e g      =  length . edges $ g
  v        ::  g -> Int                --vertex count
  v g      =  length . vertices $ g

class (EdgeSemantics e v, GraphDataSet g v e t) => Graph g v e t
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
