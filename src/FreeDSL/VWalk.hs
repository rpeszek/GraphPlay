
module FreeDSL.VWalk where

import Control.Monad
import Control.Monad.Free
--import Data.Functor.Identity
--import Control.Monad.Trans.Free

data VWalkCmds v r = GetNeighbors ([v] -> r)      |
                     WalkTo v (v -> r)            |
                     History  ([v] -> r) deriving (Functor)

type VWalk v = Free (VWalkCmds v)

-- need different pairing to do FreeT
--type VWalk v m a = FreeT (VWalkCmds v) m a
--type VWalk v a = VWalk v Identity a

--stepWith ::  Monad m => ([v] -> v) -> VWalk v m v
stepWith ::  ([v] -> v) -> VWalk v v
stepWith f = do
    n <- getNeighbors
    walkTo $ f n

getNeighbors :: VWalk v [v]
getNeighbors = liftF (GetNeighbors id)

walkTo :: v -> VWalk v v
walkTo v = liftF (WalkTo v id)

history ::  VWalk v [v]
history = liftF (History id)

whereAmI ::  VWalk v v
whereAmI = (liftM head) history
