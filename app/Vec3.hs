{-# LANGUAGE InstanceSigs #-}

module Vec3 where

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