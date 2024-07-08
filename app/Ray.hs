module Ray where

import Vec3 (Point, Vec3, mulVec3)

data Ray = Ray {origin :: !Point, direction :: !Vec3} deriving (Show)

rayAt :: Ray -> Double -> Point
rayAt ray t = origin ray + t `mulVec3` direction ray