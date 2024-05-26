{-# LANGUAGE InstanceSigs #-}

module Vec3 where

import Utils (randomDouble, randomDoubleUnit)

data Vec3 = Vec3 {x :: Double, y :: Double, z :: Double} deriving (Show)

type Point = Vec3

instance Num Vec3 where
  (+) :: Vec3 -> Vec3 -> Vec3
  (Vec3 x1 y1 z1) + (Vec3 x2 y2 z2) = Vec3 (x1 + x2) (y1 + y2) (z1 + z2)
  (-) :: Vec3 -> Vec3 -> Vec3
  (Vec3 x1 y1 z1) - (Vec3 x2 y2 z2) = Vec3 (x1 - x2) (y1 - y2) (z1 - z2)
  (*) :: Vec3 -> Vec3 -> Vec3
  (Vec3 x1 y1 z1) * (Vec3 x2 y2 z2) = Vec3 (x1 * x2) (y1 * y2) (z1 * z2)
  abs :: Vec3 -> Vec3
  abs (Vec3 x y z) = Vec3 (abs x) (abs y) (abs z)
  signum :: Vec3 -> Vec3
  signum (Vec3 x y z) = Vec3 (signum x) (signum y) (signum z)
  fromInteger :: Integer -> Vec3
  fromInteger i = Vec3 (fromInteger i) (fromInteger i) (fromInteger i)

dotProduct :: Vec3 -> Vec3 -> Double
dotProduct (Vec3 x1 y1 z1) (Vec3 x2 y2 z2) = x1 * x2 + y1 * y2 + z1 * z2

mulVec3 :: Double -> Vec3 -> Vec3
mulVec3 k (Vec3 x y z) = Vec3 (k * x) (k * y) (k * z)

divVec3 :: Vec3 -> Double -> Vec3
divVec3 (Vec3 x y z) k = Vec3 (x / k) (y / k) (z / k)

lengthSquared :: Vec3 -> Double
lengthSquared v = dotProduct v v

lengthVec3 :: Vec3 -> Double
lengthVec3 = sqrt . lengthSquared

unitVec3 :: Vec3 -> Vec3
unitVec3 v = v `divVec3` lengthVec3 v

applyToVec3 :: Vec3 -> (Double -> Double) -> Vec3
applyToVec3 (Vec3 x y z) f = Vec3 (f x) (f y) (f z)

randomVec3 :: Double -> Double -> IO Vec3
randomVec3 hi lo = Vec3 <$> randomDouble hi lo <*> randomDouble hi lo <*> randomDouble hi lo

getRandomVec3InUnitSphere :: IO Vec3
getRandomVec3InUnitSphere = do
  v <- randomVec3 (-1) 1
  if lengthSquared v < 1
    then return v
    else getRandomVec3InUnitSphere

getRandomUnitVector :: IO Vec3
getRandomUnitVector = unitVec3 <$> getRandomVec3InUnitSphere

getRandomOnHemisphere :: Vec3 -> IO Vec3
getRandomOnHemisphere n = do
  v <- getRandomUnitVector
  if dotProduct n v > 0
    then return v
    else return (-v)

isVec3NearZero :: Vec3 -> Bool
isVec3NearZero (Vec3 x y z) = (abs x < threshold) && (abs y < threshold) && (abs z < threshold)
  where
    threshold = 1e-8