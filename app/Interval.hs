module Interval where

import Utils (negInfinity, posInfinity)

data Interval = Interval Double Double

intervalSize :: Interval -> Double
intervalSize (Interval tmin tmax) = tmax - tmin

intervalContains :: Interval -> Double -> Bool
intervalContains (Interval tmin tmax) t = t >= tmin && t <= tmax

intervalSurrounds :: Interval -> Double -> Bool
intervalSurrounds (Interval tmin tmax) t = t > tmin && t < tmax

intervalClamps :: Interval -> Double -> Double
intervalClamps (Interval tmin _) x | x < tmin = tmin
intervalClamps (Interval _ tmax) x | x > tmax = tmax
intervalClamps _ x = x

universe :: Interval
universe = Interval negInfinity posInfinity

empty :: Interval
empty = Interval posInfinity negInfinity

findFirstInInterval :: Interval -> [Double] -> Maybe Double
findFirstInInterval _ [] = Nothing
findFirstInInterval interval (d : _) | intervalSurrounds interval d = Just d
findFirstInInterval interval (_ : ds) = findFirstInInterval interval ds
