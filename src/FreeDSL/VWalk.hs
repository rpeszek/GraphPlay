
module FreeDSL.VWalk (
  VWalkInstructions(..)
  , VWalkDSL
  , stepWith
  , getNeighbors
  , walkTo
  , history
  , whereAmI
) where

import Control.Monad
import Control.Monad.Free
import PolyGraph.Common.DslSupport.Coproduct ((:<:), liftDSL)
import Control.Monad.State.Strict
import PolyGraph.Common.DslSupport (MInterpreterWithCtx (..))
import PolyGraph.ReadOnly.Graph (AdjacencyIndex(..), neighborsOf)
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
--getRating ::  forall a polyglot.(Functor polyglot, (RatingInstructions a) :<: polyglot) 
--                       => a -> Free polyglot Int

--
-- using polymorphic signatures to allow for composability a la carte
--
stepWith :: forall v polyglot.(Functor polyglot, (VWalkInstructions v) :<: polyglot) 
                      => ([v] -> v) -> Free polyglot v
stepWith f = do
    n <- getNeighbors
    walkTo $ f n

getNeighbors :: forall v polyglot.(Functor polyglot, (VWalkInstructions v) :<: polyglot) 
                      => Free polyglot [v]
getNeighbors = liftDSL $ liftF (GetNeighbors id)

walkTo :: forall v polyglot.(Functor polyglot, (VWalkInstructions v) :<: polyglot) 
                      => v -> Free polyglot v
walkTo v = liftDSL $ liftF (WalkTo v id)

history ::  forall v polyglot.(Functor polyglot, (VWalkInstructions v) :<: polyglot) 
                      => Free polyglot [v]
history = liftDSL $ liftF (History id)

whereAmI ::  forall v polyglot.(Functor polyglot, (VWalkInstructions v) :<: polyglot) 
                      => Free polyglot v
whereAmI = (liftM head) history


instance forall  g v e t m. (Eq v, AdjacencyIndex g v e t, MonadState ([v]) m) => 
               MInterpreterWithCtx g m (VWalkInstructions v) where
  interpretM g (GetNeighbors nF) = do
       (vx:_) <- get
       let choiceVs = neighborsOf g vx
       if null choiceVs 
       then fail "out of cheese error"
       else nF choiceVs

  interpretM _ (WalkTo choiceV nF) =
       do
         modify $ (:) choiceV
         nF choiceV

  interpretM _ (History nF) = 
       do
         path <- get
         nF path
