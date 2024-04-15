module Hittable where

import Ray
import Vec3

class Hittable a where
  hit :: a -> Ray -> (Double, Double) -> Maybe HitRecord

data HitRecord = HitRecord
  { hitRecordP :: Point,
    hitRecordNormal :: Vec3,
    hitRecordT :: Double,
    hitRecordfrontFace :: Bool
  }

mkHitRecord :: Point -> Vec3 -> Double -> Ray -> HitRecord
mkHitRecord p outwardNormal t ray =
  let frontFace = dotProduct outwardNormal (direction ray) < 0
      normal = if frontFace then outwardNormal else -outwardNormal
   in HitRecord
        { hitRecordP = p,
          hitRecordNormal = normal,
          hitRecordT = t,
          hitRecordfrontFace = frontFace
        }
