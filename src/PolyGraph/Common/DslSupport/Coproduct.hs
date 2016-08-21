-- inspired by http://dlaing.org/cofun/posts/free_and_cofree.html

module PolyGraph.Common.DslSupport.Coproduct (
    Sum (..)
    , Coproduct
    , liftLeft
    , liftRight
    , (:+:)
    , (:<:)()
) where
import Control.Monad.Free 


data Sum f g a = InL (f a) | InR (g a) deriving (Functor)
type Coproduct = Sum
type f :+: g = Sum f g

class (Functor sub, Functor sup) => sub :<: sup where
  inj :: sub a -> sup a

instance Functor f => f :<: f where
  inj = id

instance (Functor f, Functor g) => f :<: (f :+: g) where
  inj = InL

instance (Functor f, Functor g, Functor h, f :<: g) => f :<: (h :+: g) where
  inj = InR . inj

liftLeft :: (Functor g, Functor f) => Free f a -> Free (Sum f g) a
liftLeft = hoistFree InL

liftRight :: (Functor g, Functor f) => Free g a -> Free (Sum f g) a
liftRight = hoistFree InR
