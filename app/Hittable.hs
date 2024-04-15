module Hittable where

import Ray
import Vec3

class Hittable a where
  hit :: a -> Ray -> (Double, Double) -> Maybe HitRecord

data HitRecord = HitRecord {hitRecordP :: Point, hitRecordNormal :: Vec3, hitRecordT :: Double}
