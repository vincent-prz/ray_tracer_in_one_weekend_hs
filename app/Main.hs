module Main where

import Camera (Camera, CameraArgs (..), mkCamera, render)
import Hittable (AnyHittable (AnyHittable))
import Material (Lambertian (Lambertian))
import Sphere (Sphere (..))
import Vec3

-- Image
aspectRatio :: Double
aspectRatio = 16 / 9

imageWidth :: Int
imageWidth = 256

materialLeft :: Lambertian
materialLeft = Lambertian (Vec3 0 0 1)

materialRight :: Lambertian
materialRight = Lambertian (Vec3 1 0 0)

r :: Double
r = cos (pi / 4)

world :: AnyHittable
world =
  AnyHittable
    [ AnyHittable Sphere {center = Vec3 (-r) 0 (-1), radius = r, sphereMat = materialLeft},
      AnyHittable Sphere {center = Vec3 r 0 (-1), radius = r, sphereMat = materialRight}
    ]

camera :: Camera
camera =
  mkCamera
    CameraArgs
      { cameraArgsAspectRatio = 16 / 9,
        cameraArgsImageWidth = 256,
        cameraArgsSamplesPerPixel = 100,
        cameraArgsMaxDepth = 50,
        cameraArgsVerticalAngle = 90,
        cameraArgsLookFrom = 0,
        cameraArgsLookAt = Vec3 0 0 (-1),
        cameraArgsVup = Vec3 0 1 0
      }

main :: IO ()
main = render camera world