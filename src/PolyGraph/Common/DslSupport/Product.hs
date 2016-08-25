-- inspired by http://dlaing.org/cofun/posts/free_and_cofree.html

module PolyGraph.Common.DslSupport.Product
(
  Product(..)
  , (:*:)
  , (*:*)
  , (:>:) ()
)
 where

import Control.Applicative

data Product f g a = Pair (f a) (g a) deriving (Functor, Show)
type f :*: g = Product f g

--instance (Functor f, Functor g) => Functor (Product f g) where
--  fmap h (Pair f g) = Pair (fmap h f) (fmap h g)

(*:*) :: (Functor f, Functor g) => (a -> f a) -> (a -> g a) -> a -> (f :*: g) a
(*:*) = liftA2 Pair

class (Functor big, Functor small) => big :>: small where
  prj :: big a -> small a

instance Functor f => f :>: f where
  prj = id

instance {-# OVERLAPPING #-} (Functor f, Functor g) => (f :*: g) :>: f where
  prj (Pair fa _) = fa

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor h, g :>: f) => (h :*: g) :>: f  where
  prj (Pair _ ga) = prj ga 
