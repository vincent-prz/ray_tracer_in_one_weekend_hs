{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Material where

import Color (Color)
import Hittable (HitRecord (..), hitRecordNormal, hitRecordP)
import Ray (Ray (Ray, direction, origin))
import Vec3 (Vec3 (Vec3), dotProduct, getRandomVec3InUnitSphere, isVec3NearZero, mulVec3, reflect, refract, unitVec3)

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

data Metal = Metal Color Double

instance Material Metal where
  scatter :: Metal -> Ray -> HitRecord -> IO (Maybe (Color, Ray))
  scatter (Metal albedo fuzz) rayIn (HitRecord {hitRecordP, hitRecordNormal}) =
    do
      let normalizedFuzz = max fuzz 1
      let reflected = reflect (direction rayIn) hitRecordNormal
      randomVec <- getRandomVec3InUnitSphere
      let reflectedWithFuzz = unitVec3 reflected + mulVec3 normalizedFuzz randomVec
      let scattered = Ray hitRecordP reflectedWithFuzz
      if dotProduct (direction scattered) hitRecordNormal > 0
        then return $ Just (albedo, scattered)
        else return Nothing

newtype Dielectric = Dielectric Double

instance Material Dielectric where
  scatter :: Dielectric -> Ray -> HitRecord -> IO (Maybe (Color, Ray))
  scatter (Dielectric refractionIndex) rayIn (HitRecord {hitRecordNormal, hitRecordfrontFace, hitRecordP}) =
    let actualRefractionIndex = if hitRecordfrontFace then 1 / refractionIndex else refractionIndex
        unitDirection = unitVec3 (direction rayIn)
        cosTheta = min (dotProduct (-unitDirection) hitRecordNormal) 1.0
        sinTheta = sqrt (1 - cosTheta * cosTheta)
        cannotRefract = actualRefractionIndex * sinTheta > 1
        scatteredDir =
          if cannotRefract
            then reflect unitDirection hitRecordNormal
            else refract unitDirection hitRecordNormal actualRefractionIndex
        scattered = Ray hitRecordP scatteredDir
     in return $ Just (Vec3 1 1 1, scattered)
