module Genetic.Strategies
  ( rouletteSelect,
    tournamentSelect,
    gaussianMutate,
    noMutate,
    uniformMutate,
  )
where

import Control.Monad.Random.Class (MonadRandom (getRandomR))
import Control.Monad.Random.Lazy (Random)
import Data.Vector (Vector)
import qualified Data.Vector as Vector

{-# SPECIALIZE rouletteSelect :: Vector (a, Double) -> IO a #-}
rouletteSelect :: MonadRandom m => Vector (a, Double) -> m a
rouletteSelect populationWithFitness =
  let minFitness = Vector.minimum (fmap snd populationWithFitness)
      normalizedPopulationWithFitness =
        if minFitness <= 0
          then fmap (\(member, fitness) -> (member, fitness + abs minFitness)) populationWithFitness
          else populationWithFitness
      totalFitness = Vector.sum $ fmap snd normalizedPopulationWithFitness
   in if totalFitness == 0
        then fst <$> chooseVectorUniform normalizedPopulationWithFitness
        else do
          threshold <- getRandomR (0, totalFitness)
          pure $ chooseThreshold threshold normalizedPopulationWithFitness

{-# SPECIALIZE tournamentSelect :: Vector (a, Double) -> IO a #-}
tournamentSelect :: MonadRandom m => Vector (a, Double) -> m a
tournamentSelect populationWithFitness = do
  (member0, fitness0) <- chooseVectorUniform populationWithFitness
  (member1, fitness1) <- chooseVectorUniform populationWithFitness
  pure $ if fitness0 > fitness1 then member0 else member1

{-# SPECIALIZE gaussianMutate :: Double -> Double -> IO Double #-}
gaussianMutate :: (Floating a, Random a, MonadRandom m) => a -> a -> m a
gaussianMutate stdDev base = do
  x <- getRandomR (0, 1)
  y <- getRandomR (0, 1)
  pure $ stdDev * sqrt (-2 * log x) * cos (2 * pi * y) + base

{-# SPECIALIZE noMutate :: a -> IO a #-}
noMutate :: Monad m => a -> m a
noMutate = pure

{-# SPECIALIZE uniformMutate :: (Double, Double) -> Double -> IO Double #-}
uniformMutate :: (Num a, Random a, MonadRandom m) => (a, a) -> a -> m a
uniformMutate (deltaMin, deltaMax) base = (+ base) <$> getRandomR (deltaMin, deltaMax)

{-# INLINEABLE chooseVectorUniform #-}
chooseVectorUniform :: MonadRandom m => Vector a -> m a
chooseVectorUniform items = Vector.unsafeIndex items <$> getRandomR (0, Vector.length items - 1)

chooseThreshold :: Double -> Vector (a, Double) -> a
chooseThreshold threshold populationWithFitness =
  case Vector.length populationWithFitness of
    0 -> error "cannot choose from an empty vector"
    1 -> fst $ Vector.unsafeIndex populationWithFitness 0
    _ ->
      let (a, weight) = Vector.unsafeIndex populationWithFitness 0
       in if weight >= threshold
            then a
            else chooseThreshold (threshold - weight) (Vector.drop 1 populationWithFitness)
