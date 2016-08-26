\begin{code}
module  S2_Free.E06_WalkAndRate where
\end{code}

\begin{code}
import Control.Monad
import Control.Monad.Free
import Control.Comonad.Cofree
\end{code}

\begin{code}
import S2_Free.E05_Ratings hiding (allThisHardWork)
import FreeDSL.VWalk
import FreeDSL.VWalk.CoWalk
import PolyGraph.ReadOnly.Graph (AdjacencyIndex)
\end{code}

\begin{code}
import PolyGraph.Common.DslSupport.Coproduct ((:+:), liftLeft, liftRight)
import PolyGraph.Common.DslSupport.Product ((:*:), (*:*))
import PolyGraph.Common.DslSupport.Pairing (Pairing (..))
\end{code}

\begin{code}
import Data.Hashable
import qualified Data.HashMap.Strict as HM
import Data.List ((\\), zip)
import S1_Cstr.E05_Samples (grid)
import qualified PolyGraph.Buildable.PolyMorth as Morth
import qualified Instances.ListGraphs as ListGraphs
\end{code}

\begin{code}
type RatedWalkInstructions v  = ((VWalkInstructions v) :+: (RatingInstructions v)) 
type RatedWalkCoInstructions v  = ((VWalkCoinstructions v) :*: (RatingCoinstructions v)) 
\end{code}

\begin{code}
type RatedWalkDSL v r = Free (RatedWalkInstructions v) r
type RatedWalkInterperter v k = Cofree (RatedWalkCoInstructions v) k
\end{code}

\begin{code}
runPaired :: RatedWalkDSL v r -> RatedWalkInterperter v k -> r
runPaired prog coprog = pair (\_ b -> b) coprog prog
\end{code}

\begin{code}
class (RatingContainer c v, VWalkContainer c v g) => RatedWalkContainer c v g

buildRatedWalkInstructions :: forall g v e t c. (RatedWalkContainer c v g, Show v, Hashable v, Eq v, AdjacencyIndex g v e t) =>
                          g ->  c -> RatedWalkCoInstructions v c
buildRatedWalkInstructions g  = (buildWalkInstructions g) *:* buildRatingInstructions
\end{code}

\begin{code}
data ComboContainer v g = ComboContainer {
   vwalkState :: (g, [v]),
   ratingState :: (HM.HashMap v Int)
}
instance RatingContainer (ComboContainer v g) v where
  extractRating = ratingState
  extendRating (ComboContainer wk rg) f = ComboContainer wk (f $ ComboContainer wk rg)
instance VWalkContainer (ComboContainer v g) v g where
  extractWalk = vwalkState
  extendWalk (ComboContainer wk rg) f = ComboContainer (f $ ComboContainer wk rg) rg
\end{code}

\begin{code}
buildRatedWalkInterpreter :: forall g v e t . (Show v, Hashable v, Eq v, AdjacencyIndex g v e t) =>
                          g -> v -> HM.HashMap v Int -> RatedWalkInterperter v (ComboContainer v g)
buildRatedWalkInterpreter g v map = coiter ((buildWalkInstructions g) *:* buildRatingInstructions) $ ComboContainer (g, [v]) map
\end{code}

\begin{code}
interpretRatedWalk :: forall g v e t r. (Show v, Hashable v, Eq v, AdjacencyIndex g v e t) =>
                          RatedWalkDSL v r -> g -> v -> [(v,Int)] -> r
interpretRatedWalk prog g v ratings = runPaired prog (buildRatedWalkInterpreter g v (HM.fromList ratings))
\end{code}

\begin{code}
walkDSL :: VWalkDSL v r -> RatedWalkDSL v r
walkDSL = liftLeft
ratingDSL :: RatingDSL v r -> RatedWalkDSL v r
ratingDSL = liftRight
\end{code}

\begin{code}
ratedWalk :: Eq v => Int -> RatedWalkDSL v [v]
ratedWalk 0  = return []
ratedWalk steps = do
     neighVs <- walkDSL $ getNeighbors
     visited <- walkDSL $ history
     maybeV  <- ratingDSL $ findBest (neighVs \\ visited)
     case maybeV of
        Nothing -> return []
        Just v  -> (walkDSL $ walkTo v) >> (liftM((:) v) . ratedWalk $ (steps-1))
\end{code}

\begin{code}
weightF :: (Int,Int) -> Int
weightF (i,j) = (-1)*(i-j)^2 
\end{code}

\begin{code}
walkTest n = 
              let graph = grid 5 (,) :: ListGraphs.GEdges (Int, Int)
                  vertices =  Morth.morthToVertices graph  
                  ratings = map weightF vertices
              in interpretRatedWalk (ratedWalk n) graph (0,0) (vertices `zip` ratings)
\end{code}

\begin{code}
allThisHardWork :: IO()
allThisHardWork = do
  putStrLn "Diagonal grid walk looks like this: \n"
  putStrLn $ show (walkTest 15)
\end{code}
