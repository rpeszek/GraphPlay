module PolyForFree.DiGraphDSL (
    DiGraphDSL
    , addVertex
    , addDiEdgeWithData
    , showProgram
    , runDefaultInterpreter
) where

import Control.Monad.Free (Free(..),liftF)
import Control.Monad.State (State, execState, modify)
import Data.List (nub)

data DiGraphDslCmds v edata next = 
                 AddVertex v next |
                 AddDiEdgeWithData v v edata next 
                       deriving (Functor)

type DiGraphDSL v edata = Free (DiGraphDslCmds v edata) ()

addVertex :: forall v edata .  v -> Free (DiGraphDslCmds v edata) ()
addVertex v = liftF (AddVertex v ())

addDiEdgeWithData :: forall v edata . v -> v -> edata -> Free (DiGraphDslCmds v edata) ()
addDiEdgeWithData v1 v2 edgeData = liftF (AddDiEdgeWithData v1 v2 edgeData ())


--- interpreters ---

showProgram :: forall v edata r . (Show v, Show edata) => Free (DiGraphDslCmds v edata) r -> String
showProgram (Free (AddVertex v1 next)) = "+@ (" ++ show(v1) ++ ") \n" ++ showProgram next
showProgram (Free (AddDiEdgeWithData v1 v2 edata next)) = 
          "+~ (" ++ show(v1) ++ " --{" ++ show(edata) ++ "}-> " ++ show(v2) ++ ") \n" ++ showProgram next          
showProgram (Pure _) = "end" -- ++ show r ++ "\n"

addVertexHelper :: forall v edata .  v -> ([v], [(v,v,edata)]) -> ([v], [(v,v,edata)])
addVertexHelper v (vlist, elist) = (v:vlist, elist)

addEdgeHelper :: forall v edata . (v,v,edata) -> ([v], [(v,v,edata)]) -> ([v], [(v,v,edata)])
addEdgeHelper (v1, v2, edata) (vlist, elist) = (v1:v2:vlist, (v1, v2, edata):elist)

--note r in State definition does currently nothing, could be changed to ()
interpret :: forall v edata r .  Free (DiGraphDslCmds v edata) r -> State ([v], [(v,v,edata)]) r
interpret (Free (AddVertex v1 next)) = (modify (addVertexHelper v1)) >> interpret next
interpret (Free (AddDiEdgeWithData v1 v2 edata next)) = (modify (addEdgeHelper (v1,v2,edata))) >> interpret next
interpret (Pure r) = return r

runDefaultInterpreter :: forall v edata r . (Eq v) => Free (DiGraphDslCmds v edata) r -> ([v], [(v,v,edata)]) -> ([v], [(v,v,edata)])
runDefaultInterpreter program initdata = 
                    let (vlist, elist) = execState (interpret program) initdata
                    in (nub vlist, elist)
