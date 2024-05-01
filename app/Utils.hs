module Utils where

import System.Random

posInfinity :: Double
posInfinity = 1 / 0

negInfinity :: Double
negInfinity = -posInfinity

randomDouble :: Double -> Double -> IO Double
randomDouble hi lo = do
  fst . randomR (hi, lo) <$> newStdGen

randomDoubleUnit :: IO Double
randomDoubleUnit = randomDouble 0 1