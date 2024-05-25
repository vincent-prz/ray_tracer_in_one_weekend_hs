module Main where

import Camera (Camera, mkCamera, render)
import Hittable (AnyHittable (AnyHittable))
import Material (Lambertian (Lambertian))
import Sphere (Sphere (..))
import Vec3

-- Image
aspectRatio :: Double
aspectRatio = 16 / 9

imageWidth :: Int
imageWidth = 256

material :: Lambertian
material = Lambertian (Vec3 0.5 0.5 0.5)

world :: AnyHittable
world =
  AnyHittable
    [ AnyHittable Sphere {center = Vec3 0 0 (-1), radius = 0.5, sphereMat = material},
      AnyHittable Sphere {center = Vec3 0 (-100.5) (-1), radius = 100, sphereMat = material}
    ]

camera :: Camera
camera = mkCamera (16 / 9) 256 100 50

main :: IO ()
main = render camera world