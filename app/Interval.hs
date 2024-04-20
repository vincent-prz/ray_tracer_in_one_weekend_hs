module Interval where

import Utils (negInfinity, posInfinity)

data Interval = Interval Double Double

intervalSize :: Interval -> Double
intervalSize (Interval tmin tmax) = tmax - tmin

intervalContains :: Interval -> Double -> Bool
intervalContains (Interval tmin tmax) t = t >= tmin && t <= tmax

intervalSurrounds :: Interval -> Double -> Bool
intervalSurrounds (Interval tmin tmax) t = t > tmin && t < tmax

universe :: Interval
universe = Interval negInfinity posInfinity

empty :: Interval
empty = Interval posInfinity negInfinity

findFirstInInterval :: Interval -> [Double] -> Maybe Double
findFirstInInterval _ [] = Nothing
findFirstInInterval interval (d : ds) = if intervalSurrounds interval d then Just d else findFirstInInterval interval ds
