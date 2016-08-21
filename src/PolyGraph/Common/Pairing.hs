-- inspired by http://dlaing.org/cofun/posts/free_and_cofree.html
-- see credits in that post/ Ed Kmett and Dan Piponi

module PolyGraph.Common.Pairing where
import Data.Functor.Identity
import qualified Control.Monad.Free as Free
--import qualified Control.Monad.Trans.Free as TFree
import qualified Control.Comonad.Cofree as Cofree
--import qualified Control.Comonad.Trans.Cofree as TCofree

class (Functor f, Functor g) => Pairing f g where
    pair :: (a -> b -> r) -> f a -> g b -> r

instance Pairing Identity Identity where
  pair f (Identity a) (Identity b) = f a b

instance Pairing ((->) a) ((,) a) where
  pair p f = uncurry (p . f)

instance Pairing ((,) a) ((->) a) where
  pair p f g = p (snd f) (g (fst f))

instance Pairing f g => Pairing (Cofree.Cofree f) (Free.Free g) where
  pair p (a Cofree.:< _ ) (Free.Pure x)  = p a x
  pair p (_ Cofree.:< fs) (Free.Free gs) = pair (pair p) fs gs
