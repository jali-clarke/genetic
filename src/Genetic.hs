{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}

module Genetic
  ( Genetic (..),
    Mutatable (..),
    GeneticOpts (..),
    newPopulation,
    rankGeneration,
    nextGeneration,
    simulate,
  )
where

import qualified Control.Monad.Random as Random
import Control.Monad.ST (runST)
import qualified Data.Foldable as Foldable
import qualified Data.Vector as Vector
import qualified Data.Vector.Algorithms.Heap as Heap
import Genetic.Positive (Positive)
import Genetic.Zipperable (Zipperable (..))

class (Mutatable a) => Genetic a where
  generateNew :: (Random.MonadRandom m) => m a

  crossover :: (Random.MonadRandom m) => a -> a -> m a
  default crossover :: (Zipperable a, Random.MonadRandom m) => a -> a -> m a
  crossover a b = do
    (ctxA, holeA) <- split a
    (ctxB, holeB) <- split b
    Random.uniform [ctxA holeB, ctxB holeA]

class Mutatable a where
  mutate :: (Random.MonadRandom m) => a -> m a
  default mutate :: (Zipperable a, Mutatable (Hole a), Random.MonadRandom m) => a -> m a
  mutate a = do
    (ctx, hole) <- split a
    hole' <- mutate hole
    pure $ ctx hole'

data GeneticOpts m a
  = GeneticOpts
  { populationSize :: Int,
    fitness :: a -> m Positive,
    numElites :: Int,
    replicateWeight :: Positive,
    crossoverWeight :: Positive,
    mutateWeight :: Positive
  }

newPopulation :: (Genetic a, Random.MonadRandom m) => GeneticOpts m a -> m (Vector.Vector a)
newPopulation opts = Vector.replicateM (populationSize opts) generateNew

rankGeneration :: (Monad m) => GeneticOpts m a -> Vector.Vector a -> m (Vector.Vector (a, Positive))
rankGeneration opts currentGen = do
  withFitnesses <- Vector.mapM (\a -> fitness opts a >>= (\f -> pure (a, f))) currentGen
  let withFitnessesSorted = runST $ do
        withFitnessesMutable <- Vector.thaw withFitnesses
        Heap.sortBy (\(_, f0) (_, f1) -> compare f1 f0) withFitnessesMutable
        Vector.unsafeFreeze withFitnessesMutable
  pure withFitnessesSorted

nextGeneration :: (Genetic a, Random.MonadRandom m) => GeneticOpts m a -> Vector.Vector (a, Positive) -> m (Vector.Vector a)
nextGeneration opts withFitnessesSorted =
  Vector.generateM
    (populationSize opts)
    ( \idx ->
        if idx < numElites opts
          then pure $ fst (withFitnessesSorted Vector.! idx)
          else produceIndividual opts withFitnessesSorted
    )

simulate :: (Genetic a, Random.MonadRandom m) => GeneticOpts m a -> Int -> Vector.Vector a -> (Int -> Vector.Vector (a, Positive) -> m ()) -> m (Vector.Vector (a, Positive))
simulate = simulate' 0

simulate' :: (Genetic a, Random.MonadRandom m) => Int -> GeneticOpts m a -> Int -> Vector.Vector a -> (Int -> Vector.Vector (a, Positive) -> m ()) -> m (Vector.Vector (a, Positive))
simulate' iterIdx opts numIters thisGen callback = do
  withFitnessesSorted <- rankGeneration opts thisGen
  callback iterIdx withFitnessesSorted
  if iterIdx < numIters
    then do
      nextGen <- nextGeneration opts withFitnessesSorted
      simulate' (iterIdx + 1) opts numIters nextGen callback
    else pure withFitnessesSorted

produceIndividual :: (Genetic a, Random.MonadRandom m) => GeneticOpts m a -> Vector.Vector (a, Positive) -> m a
produceIndividual opts as = do
  a <- weighted as
  modifier <-
    weighted
      [ (pure, replicateWeight opts),
        (\a' -> weighted as >>= crossover a', crossoverWeight opts),
        (mutate, mutateWeight opts)
      ]
  modifier a

weighted :: (Foldable t, Random.MonadRandom m) => t (a, Positive) -> m a
weighted = Random.fromList . fmap (\(w, p) -> (w, toRational p)) . Foldable.toList
