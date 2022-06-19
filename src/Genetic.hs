{-# LANGUAGE TupleSections #-}

module Genetic
  ( Config (..),
    simulateSteps,
  )
where

import Data.Vector (Vector)
import qualified Data.Vector as Vector
import qualified Data.Vector.Algorithms.Merge as MVector

data Config m a = Config
  { fitness :: a -> m Double,
    select :: Vector (a, Double) -> m a,
    crossover :: a -> a -> m a,
    mutate :: a -> m a,
    numElites :: Int,
    numGenerations :: Int,
    onGeneration :: Int -> Vector (a, Double) -> m (),
    onFinalGeneration :: Int -> Vector (a, Double) -> m ()
  }

{-# SPECIALIZE simulateSteps :: Config IO a -> Vector a -> IO () #-}
simulateSteps :: Monad m => Config m a -> Vector a -> m ()
simulateSteps config = simulateSteps' config 0

{-# INLINEABLE simulateSteps' #-}
simulateSteps' :: Monad m => Config m a -> Int -> Vector a -> m ()
simulateSteps' config generationNumber population = do
  populationWithFitness <- populationWithFitnessSortedDesc config population
  onGeneration config generationNumber populationWithFitness
  if generationNumber == numGenerations config - 1
    then onFinalGeneration config generationNumber populationWithFitness
    else nextPopulation config populationWithFitness >>= simulateSteps' config (generationNumber + 1)

{-# INLINEABLE populationWithFitnessSortedDesc #-}
populationWithFitnessSortedDesc :: Monad m => Config m a -> Vector a -> m (Vector (a, Double))
populationWithFitnessSortedDesc config population =
  let compareFitnessDesc (_, fitness0) (_, fitness1) = fitness1 `compare` fitness0
   in do
        populationWithFitness <- Vector.forM population $ \member -> (member,) <$> (fitness config member)
        pure $ Vector.modify (MVector.sortBy compareFitnessDesc) populationWithFitness

{-# INLINEABLE nextPopulation #-}
nextPopulation :: Monad m => Config m a -> Vector (a, Double) -> m (Vector a)
nextPopulation config populationWithFitness =
  let populationSize = Vector.length populationWithFitness
      elites = Vector.map fst $ Vector.take (numElites config) populationWithFitness
   in do
        children <-
          Vector.replicateM (populationSize - numElites config) $ do
            parent0 <- select config populationWithFitness
            parent1 <- select config populationWithFitness
            crossover config parent0 parent1 >>= mutate config
        pure $ elites <> children
