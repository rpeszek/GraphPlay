
module FreeDSL.VWalk where

import Control.Monad
import Control.Monad.Free


data VWalkCmds v r = GetNeighbors ([v] -> r)      |
                     WalkTo v (v -> r)            |
                     History  ([v] -> r) deriving (Functor)

type VWalk v = Free (VWalkCmds v) 

stepWith ::  ([v] -> v) -> VWalk v v
stepWith f = do
    n <- getNeighbors
    walkTo $ f n

getNeighbors :: VWalk v [v]
getNeighbors = liftF (GetNeighbors id)

walkTo :: v -> VWalk v v
walkTo v = liftF (WalkTo v id)

history :: VWalk v [v]
history = liftF (History id)

whereAmI :: VWalk v v
whereAmI = (liftM head) history
