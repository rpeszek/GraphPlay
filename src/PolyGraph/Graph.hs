
module PolyGraph.Graph where --exports everything, a terrible programmer wrote it

class (Eq v, Foldable t)  => GraphDataSet g v e t | g -> t, g -> v, g -> e where
  vertices ::  g -> t v
  edges    ::  g -> t e
