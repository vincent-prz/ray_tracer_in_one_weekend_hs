{-# LANGUAGE InstanceSigs #-}

module Hittable where

import Control.Applicative (Alternative ((<|>)))
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

instance (Hittable a) => Hittable [a] where
  hit :: (Hittable a) => [a] -> Ray.Ray -> (Double, Double) -> Maybe HitRecord
  hit objects ray (tmin, tmax) = foldl f Nothing objects
    where
      f :: (Hittable a) => Maybe HitRecord -> a -> Maybe HitRecord
      f closestRecord object =
        hit
          object
          ray
          (tmin, maybe tmax hitRecordT closestRecord)
          <|> closestRecord