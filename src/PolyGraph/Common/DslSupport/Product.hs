-- inspired by http://dlaing.org/cofun/posts/free_and_cofree.html

module PolyGraph.Common.DslSupport.Product
(
  Product(..)
  , (:*:)
  , (*:*)
)
 where

import Control.Applicative

data Product f g a = Pair (f a) (g a) deriving Functor
type f :*: g = Product f g

--instance (Functor f, Functor g) => Functor (Product f g) where
--  fmap h (Pair f g) = Pair (fmap h f) (fmap h g)

(*:*) :: (Functor f, Functor g) => (a -> f a) -> (a -> g a) -> a -> (f :*: g) a
(*:*) = liftA2 Pair
