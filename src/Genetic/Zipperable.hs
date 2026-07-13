{-# LANGUAGE TypeFamilies #-}

module Genetic.Zipperable
  ( Zipperable (..),
  )
where

import qualified Control.Monad.Random as Random
import Data.Kind (Type)

class Zipperable a where
  type Hole a :: Type
  split :: (Random.MonadRandom m) => a -> m (Hole a -> a, Hole a)
