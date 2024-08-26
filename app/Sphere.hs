{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Sphere (Sphere (..)) where

import Hittable (HitRecord (..), Hittable (hit), mkHitRecord)
import Interval (Interval, findFirstInInterval)
import Material (Material)
import Ray
import Vec3 (Point, dotProduct, unitVec3)

data Sphere = forall mat. (Material mat) => Sphere {center :: Point, radius :: Double, sphereMat :: mat}

instance Hittable Sphere where
  hit :: Sphere -> Ray -> Interval -> Maybe HitRecord
  hit (Sphere {radius, center, sphereMat}) !ray !interval =
    let d = direction ray
        qc = center - origin ray
        a = dotProduct d d
        -- let's use h such as b = - 2 * h, to simplify the equations
        h = dotProduct d qc
        c = dotProduct qc qc - radius * radius
        discrimininant = h * h - a * c
     in if discrimininant >= 0
          then do
            root <- findFirstInInterval interval [(h - sqrt discrimininant) / a, (h + sqrt discrimininant) / a]
            let intersection = rayAt ray root
            let n = unitVec3 (intersection - center)
            return $ mkHitRecord intersection n root ray sphereMat
          else Nothing
