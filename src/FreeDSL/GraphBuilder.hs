module FreeDSL.GraphBuilder (
    GraphDSL
    , addVertex
    , addEdgeWithData
    , programPrettyShow
    , runDefaultInterpreter
) where

import Control.Monad.Free (Free(..),liftF)
import Control.Monad.State (State, execState, modify)
import Data.List (nub)

data GraphDslCmds v edata next = 
                 AddVertex v next |
                 AddEdgeWithData v v edata next 
                       deriving (Functor)

type GraphDSL v edata = Free (GraphDslCmds v edata) ()

addVertex :: forall v edata .  v -> Free (GraphDslCmds v edata) ()
addVertex v = liftF (AddVertex v ())

addEdgeWithData :: forall v edata . v -> v -> edata -> Free (GraphDslCmds v edata) ()
addEdgeWithData v1 v2 edgeData = liftF (AddEdgeWithData v1 v2 edgeData ())


--- interpreters ---

programPrettyShow :: forall v edata r . (Show v, Show edata, Show r) => Free (GraphDslCmds v edata) r -> String
programPrettyShow (Free (AddVertex v1 next)) = "addVertex " ++ show(v1) ++ " \n" ++ programPrettyShow next
programPrettyShow (Free (AddEdgeWithData v1 v2 edata next)) = 
          "addEdgeWithData " ++ show(v1) ++ " " ++ show(v2) ++ " " ++ show(edata) ++") \n" ++ programPrettyShow next          
programPrettyShow (Pure _) = "" --show r ++ "\n"

addVertexHelper :: forall v edata .  v -> ([v], [(v,v,edata)]) -> ([v], [(v,v,edata)])
addVertexHelper v (vlist, elist) = (v:vlist, elist)

addEdgeHelper :: forall v edata . (v,v,edata) -> ([v], [(v,v,edata)]) -> ([v], [(v,v,edata)])
addEdgeHelper (v1, v2, edata) (vlist, elist) = (v1:v2:vlist, (v1, v2, edata):elist)

--note r in State definition does currently nothing, could be changed to ()
interpret :: forall v edata r .  Free (GraphDslCmds v edata) r -> State ([v], [(v,v,edata)]) r
interpret (Free (AddVertex v1 next)) = (modify (addVertexHelper v1)) >> interpret next
interpret (Free (AddEdgeWithData v1 v2 edata next)) = (modify (addEdgeHelper (v1,v2,edata))) >> interpret next
interpret (Pure r) = return r

runDefaultInterpreter :: forall v edata r . (Eq v) => Free (GraphDslCmds v edata) r -> ([v], [(v,v,edata)]) -> ([v], [(v,v,edata)])
runDefaultInterpreter program initdata = 
                    let (vlist, elist) = execState (interpret program) initdata
                    in (nub vlist, elist)
