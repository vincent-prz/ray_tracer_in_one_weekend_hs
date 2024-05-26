{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Material where

import Color (Color)
import Hittable (HitRecord (..), hitRecordNormal, hitRecordP)
import Ray (Ray (Ray, direction, origin))
import Vec3 (getRandomVec3InUnitSphere, isVec3NearZero)

class Material a where
  scatter :: a -> Ray -> HitRecord -> IO (Maybe (Color, Ray))

newtype Lambertian = Lambertian Color

instance Material Lambertian where
  scatter :: Lambertian -> Ray -> HitRecord -> IO (Maybe (Color, Ray))
  scatter (Lambertian albedo) _ (HitRecord {hitRecordP, hitRecordNormal}) =
    do
      direction <- (+ hitRecordNormal) <$> getRandomVec3InUnitSphere
      -- Catch degenerate scatter direction
      let scatterDirection = if isVec3NearZero direction then hitRecordNormal else direction
      return $ Just (albedo, Ray {origin = hitRecordP, direction = scatterDirection})
