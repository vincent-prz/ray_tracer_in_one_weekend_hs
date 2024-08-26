{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE InstanceSigs #-}
{-# LANGUAGE NamedFieldPuns #-}

module Material
  ( Material (..),
    AnyMaterial (..),
    Lambertian (..),
    Metal (..),
    Dielectric (..),
  )
where

import Color (Color)
import GHC.Float (powerDouble)
import Hittable (HitRecord (..), hitRecordNormal, hitRecordP)
import Ray (Ray (Ray, direction, origin))
import Utils (RandomState, randomDoubleUnit)
import Vec3 (Vec3 (Vec3), dotProduct, getRandomVec3InUnitSphere, isVec3NearZero, mulVec3, reflect, refract, unitVec3)

class Material a where
  scatter :: a -> Ray -> HitRecord -> RandomState (Maybe (Color, Ray))

-- Define an existential wrapper type to allow heterogeneous lists of Materials.
data AnyMaterial = forall a. (Material a) => AnyMaterial a

newtype Lambertian = Lambertian Color

instance Material Lambertian where
  scatter :: Lambertian -> Ray -> HitRecord -> RandomState (Maybe (Color, Ray))
  scatter (Lambertian albedo) _ (HitRecord {hitRecordP, hitRecordNormal}) =
    do
      direction <- (+ hitRecordNormal) <$> getRandomVec3InUnitSphere
      -- Catch degenerate scatter direction
      let scatterDirection = if isVec3NearZero direction then hitRecordNormal else direction
      return $ Just (albedo, Ray {origin = hitRecordP, direction = scatterDirection})

data Metal = Metal Color Double

instance Material Metal where
  scatter :: Metal -> Ray -> HitRecord -> RandomState (Maybe (Color, Ray))
  scatter (Metal albedo fuzz) rayIn (HitRecord {hitRecordP, hitRecordNormal}) =
    do
      let normalizedFuzz = min fuzz 1
      let reflected = reflect (direction rayIn) hitRecordNormal
      randomVec <- getRandomVec3InUnitSphere
      let reflectedWithFuzz = unitVec3 reflected + mulVec3 normalizedFuzz randomVec
      let scattered = Ray hitRecordP reflectedWithFuzz
      if dotProduct (direction scattered) hitRecordNormal > 0
        then return $ Just (albedo, scattered)
        else return Nothing

newtype Dielectric = Dielectric Double

instance Material Dielectric where
  scatter :: Dielectric -> Ray -> HitRecord -> RandomState (Maybe (Color, Ray))
  scatter (Dielectric refractionIndex) rayIn (HitRecord {hitRecordNormal, hitRecordfrontFace, hitRecordP}) =
    let actualRefractionIndex = if hitRecordfrontFace then 1 / refractionIndex else refractionIndex
        unitDirection = unitVec3 (direction rayIn)
        cosTheta = min (dotProduct (-unitDirection) hitRecordNormal) 1.0
        sinTheta = sqrt (1 - cosTheta * cosTheta)
        cannotRefract = actualRefractionIndex * sinTheta > 1
     in do
          randValue <- randomDoubleUnit
          let scatteredDir =
                if cannotRefract || reflectance cosTheta actualRefractionIndex > randValue
                  then reflect unitDirection hitRecordNormal
                  else refract unitDirection hitRecordNormal actualRefractionIndex
          let scattered = Ray hitRecordP scatteredDir
          return $ Just (Vec3 1 1 1, scattered)

-- Schlick approximation
reflectance :: Double -> Double -> Double
reflectance cosine refactionIndex =
  let r0 = (1 - refactionIndex) / (1 + refactionIndex)
      r1 = r0 * r0
   in r1 + (1 - r1) * powerDouble (1 - cosine) 5