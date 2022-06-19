module Genetic.Strategies
  ( tournamentSelect,
  )
where

import Control.Monad.Random.Class (MonadRandom (getRandomR))
import Data.Vector (Vector)
import qualified Data.Vector as Vector

tournamentSelect :: MonadRandom m => Vector (a, Double) -> m a
tournamentSelect populationWithFitness = do
  (member0, fitness0) <- chooseVector populationWithFitness
  (member1, fitness1) <- chooseVector populationWithFitness
  pure $ if fitness0 > fitness1 then member0 else member1

chooseVector :: MonadRandom m => Vector a -> m a
chooseVector items = Vector.unsafeIndex items <$> getRandomR (0, Vector.length items - 1)
