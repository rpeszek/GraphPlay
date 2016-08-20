-- inspired by http://dlaing.org/cofun/posts/free_and_cofree.html
-- see credits in that post/ Ed Kmett and Dan Piponi

module PolyGraph.Common.Pairing where
import Data.Functor.Identity
import Control.Monad.Free
import Control.Comonad.Cofree

class (Functor f, Functor g) => Pairing f g where
    pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = p (snd f) (g (fst f))

instance Pairing f g => Pairing (Cofree f) (Free g) where
  pair p (a :< _ ) (Pure x)  = p a x
  pair p (_ :< fs) (Free gs) = pair (pair p) fs gs
