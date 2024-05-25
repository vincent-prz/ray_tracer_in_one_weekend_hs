{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE InstanceSigs #-}

module Hittable where

import Control.Applicative (Alternative ((<|>)))
import Interval (Interval (Interval))
-- the SOURCE pragma is in order to avoid circular dependency
import {-# SOURCE #-} Material (Material)
import Ray
import Vec3

class Hittable a where
  hit :: a -> Ray -> Interval -> Maybe HitRecord

-- Define an existential wrapper type to allow heterogeneous lists of Hittables.
data AnyHittable = forall a. (Hittable a) => AnyHittable a

data HitRecord = forall mat. (Material mat) => HitRecord
  { hitRecordP :: Point,
    hitRecordNormal :: Vec3,
    hitRecordT :: Double,
    hitRecordfrontFace :: Bool,
    hitRecordMat :: mat
  }

mkHitRecord :: (Material mat) => Point -> Vec3 -> Double -> Ray -> mat -> HitRecord
mkHitRecord p outwardNormal t ray mat =
  let frontFace = dotProduct outwardNormal (direction ray) < 0
      normal = if frontFace then outwardNormal else -outwardNormal
   in HitRecord
        { hitRecordP = p,
          hitRecordNormal = normal,
          hitRecordT = t,
          hitRecordfrontFace = frontFace,
          hitRecordMat = mat
        }

instance Hittable [AnyHittable] where
  hit :: [AnyHittable] -> Ray -> Interval -> Maybe HitRecord
  hit objects ray (Interval tmin tmax) = foldl f Nothing objects
    where
      f :: Maybe HitRecord -> AnyHittable -> Maybe HitRecord
      f closestRecord (AnyHittable object) =
        hit
          object
          ray
          (Interval tmin (maybe tmax hitRecordT closestRecord))
          <|> closestRecord