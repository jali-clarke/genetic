{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Genetic.Positive
  ( Positive,
    absDiff,
    abs',
    square,
  )
where

import Control.Monad.Random (Random (..))

newtype Positive = Positive Double
  deriving (Eq, Ord, Show, Real, RealFrac, Random)

instance Num Positive where
  Positive a + Positive b = Positive (a + b)
  Positive a * Positive b = Positive (a * b)

  signum (Positive a) = if a == 0 then Positive 0 else Positive 1

  abs p = p

  fromInteger n = if n < 0 then error ("negative fromInteger int for Positive: " ++ show n) else Positive (fromInteger n)

  negate _ = error "cannot negate Positive"

  (-) _ _ = error "cannot subtract Positive"

instance Fractional Positive where
  fromRational n = if n < 0 then error ("negative fromRational n for Positive: " ++ show n) else Positive (fromRational n)

  recip (Positive a) = Positive (recip a)

  Positive a / Positive b = Positive (a / b)

instance Floating Positive where
  pi = Positive pi
  exp (Positive a) = Positive (exp a)
  log (Positive a) = if a < 1 then error ("cannot take log of Positive < 1: " ++ show a) else Positive (log a)

  sin _ = error "cannot take sin of Positive"
  cos _ = error "cannot take cos of Positive"
  asin _ = error "cannot take asin of Positive"
  acos _ = error "cannot take acos of Positive"
  atan _ = error "cannot take atan of Positive"
  cosh (Positive a) = Positive (cosh a)
  sinh _ = error "cannot take sinh of Positive"
  acosh _ = error "cannot take acosh of Positive"
  asinh _ = error "cannot take asinh of Positive"
  atanh _ = error "cannot take atanh of Positive"

absDiff :: Positive -> Positive -> Positive
absDiff (Positive a) (Positive b) = Positive (abs $ a - b)

abs' :: Double -> Positive
abs' = Positive . abs

square :: Positive -> Positive
square p = p * p
