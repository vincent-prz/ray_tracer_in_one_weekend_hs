module Utils where

import Control.Monad.State
import System.Random

posInfinity :: Double
posInfinity = 1 / 0

negInfinity :: Double
negInfinity = -posInfinity

type RandomState a = State StdGen a

randomDouble :: Double -> Double -> RandomState Double
randomDouble hi lo = do
  gen <- get
  let (result, gen') = randomR (hi, lo) gen
  put gen'
  return result

randomDoubleUnit :: RandomState Double
randomDoubleUnit = randomDouble 0 1

degreesToRadian :: Double -> Double
degreesToRadian angle = angle * (pi / 180)