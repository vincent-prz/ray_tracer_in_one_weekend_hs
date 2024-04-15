{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Sphere where

import Hittable (HitRecord (..), Hittable (hit))
import Ray
import Vec3 (Point, dotProduct, unitVec3)

data Sphere = Sphere {center :: Point, radius :: Double}

findFirstInInterval :: [Double] -> (Double, Double) -> Maybe Double
-- assumption; t1 <= t2
findFirstInInterval [] _ = Nothing
findFirstInInterval (d : ds) (t1, t2) = if d > t1 && d < t2 then Just d else findFirstInInterval ds (t1, t2)

instance Hittable Sphere where
  hit :: Sphere -> Ray -> (Double, Double) -> Maybe HitRecord
  hit (Sphere {radius, center}) ray (tmin, tmax) =
    let d = direction ray
        qc = center - origin ray
        a = dotProduct d d
        -- let's use h such as b = - 2 * h, to simplify the equations
        h = dotProduct d qc
        c = dotProduct qc qc - radius * radius
        discrimininant = h * h - a * c
     in if discrimininant >= 0
          then do
            root <- findFirstInInterval [(h - sqrt discrimininant) / a, (h + sqrt discrimininant) / a] (tmin, tmax)
            let intersection = rayAt ray root
            let n = unitVec3 (intersection - center)
            return HitRecord {hitRecordP = intersection, hitRecordNormal = n, hitRecordT = root}
          else Nothing
