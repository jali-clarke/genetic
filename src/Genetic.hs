module Genetic (
    Config(..),

    gaussian,

    tournament,
    simulate
) where

import Control.Monad.Random
import Control.Monad.ST
import Control.Parallel.Strategies
import qualified Data.Vector as V
import qualified Data.Vector.Algorithms.Heap as V

data Config m a = Config {
    numGens :: Int,
    numMembers :: Int,
    newMember :: m a,
    fitness :: m (a -> Double),
    mutate :: a -> m a,
    cross :: a -> a -> m a,
    selectParent :: V.Vector (Double, a) -> m a,
    withOldGen :: Int -> V.Vector (Double, a) -> m ()
}

gaussian :: MonadRandom m => Double -> m Double
gaussian s = do
    x0 <- getRandomR (0, 1)
    x1 <- getRandomR (0, 1)
    pure $ s * sqrt (-2 * log x0) * cos (2 * pi * x1)

sortFitness :: V.Vector (Double, a) -> V.Vector (Double, a)
sortFitness vec = runST $ do
    mvec <- V.thaw vec
    V.sortBy (\pf0 pf1 -> fst pf0 `compare` fst pf1) mvec
    V.unsafeFreeze mvec

tournament :: MonadRandom m => V.Vector (Double, a) -> m a
tournament pop =
    let randomMember = fmap (pop V.!) $ getRandomR (0, V.length pop - 1)
    in do
        (f0, p0) <- randomMember
        (f1, p1) <- randomMember
        pure $ if f0 < f1 then p0 else p1

calcFitnesses :: (a -> Double) -> V.Vector a -> V.Vector (Double, a)
calcFitnesses fitnessFunc pop =
    let toEval = fmap (\a -> (fitnessFunc a, a)) pop
        evalFirstStrat (f, a) = fmap (flip (,) a) (rdeepseq f)
        vecStrat = fmap V.fromList . parListChunk (V.length pop `div` 16) evalFirstStrat . V.toList
    in withStrategy vecStrat toEval

step :: Monad m => Int -> Config m a -> V.Vector (Double, a) -> m (V.Vector (Double, a))
step genIdx config measuredPop =
    let childGen = do
            a0 <- selectParent config measuredPop
            a1 <- selectParent config measuredPop
            a <- cross config a0 a1
            mutate config a
    in do
        withOldGen config genIdx measuredPop
        fitnessFunc <- fitness config
        newPop <- V.replicateM (numMembers config) childGen
        let measuredNewPop = calcFitnesses fitnessFunc newPop
            allMeasured = measuredPop <> measuredNewPop
            sortedPop = sortFitness allMeasured
        pure . V.force $ V.take (numMembers config) sortedPop

simulate :: Monad m => Config m a -> m ()
simulate config =
    let stepHelper genIdx pop =
            when (genIdx <= numGens config) $ step genIdx config pop >>= stepHelper (genIdx + 1)
    in do
        startingPop <- V.replicateM (numMembers config) (newMember config)
        fitnessFunc <- fitness config
        let measuredPop = calcFitnesses fitnessFunc startingPop
            sortedPop = sortFitness measuredPop
        stepHelper 0 sortedPop
