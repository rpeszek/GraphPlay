
module FreeDSL.VWalk where

import Control.Monad
import Control.Monad.Free
--import Data.Functor.Identity
--import Control.Monad.Trans.Free

data VWalkInstructions v r = GetNeighbors ([v] -> r)      |
                     WalkTo v (v -> r)            |
                     History  ([v] -> r) deriving (Functor)

type VWalkDSL v = Free (VWalkInstructions v)

-- need different pairing to do FreeT
--type VWalkT v m a = FreeT (VWalkInstructions v) m a
--type VWalkDSL v a = VWalkDSL v Identity a

--stepWith ::  Monad m => ([v] -> v) -> VWalkDSL v m v
stepWith ::  ([v] -> v) -> VWalkDSL v v
stepWith f = do
    n <- getNeighbors
    walkTo $ f n

getNeighbors :: VWalkDSL v [v]
getNeighbors = liftF (GetNeighbors id)

walkTo :: v -> VWalkDSL v v
walkTo v = liftF (WalkTo v id)

history ::  VWalkDSL v [v]
history = liftF (History id)

whereAmI ::  VWalkDSL v v
whereAmI = (liftM head) history
