module Main where

import Camera (Camera, CameraArgs (..), mkCamera, render)
import Hittable (AnyHittable (AnyHittable))
import Material (Dielectric (Dielectric), Lambertian (Lambertian), Metal (Metal))
import Sphere (Sphere (..))
import Vec3

-- Image
aspectRatio :: Double
aspectRatio = 16 / 9

imageWidth :: Int
imageWidth = 256

materialGround :: Lambertian
materialGround = Lambertian (Vec3 0.8 0.8 0)

materialCenter :: Lambertian
materialCenter = Lambertian (Vec3 0.1 0.2 0.5)

materialLeft :: Dielectric
materialLeft = Dielectric 1.5

materialBubble :: Dielectric
materialBubble = Dielectric (1 / 1.5)

materialRight :: Metal
materialRight = Metal (Vec3 0.8 0.6 0.2) 1.0

world :: AnyHittable
world =
  AnyHittable
    [ AnyHittable Sphere {center = Vec3 0 (-100.5) (-1), radius = 100, sphereMat = materialGround},
      AnyHittable Sphere {center = Vec3 0 0 (-1.2), radius = 0.5, sphereMat = materialCenter},
      AnyHittable Sphere {center = Vec3 (-1) 0 (-1), radius = 0.5, sphereMat = materialLeft},
      AnyHittable Sphere {center = Vec3 (-1) 0 (-1), radius = 0.4, sphereMat = materialBubble},
      AnyHittable Sphere {center = Vec3 1 0 (-1), radius = 0.5, sphereMat = materialRight}
    ]

r :: Double
r = cos (pi / 4)

camera :: Camera
camera =
  mkCamera
    CameraArgs
      { cameraArgsAspectRatio = 16 / 9,
        cameraArgsImageWidth = 256,
        cameraArgsSamplesPerPixel = 100,
        cameraArgsMaxDepth = 50,
        cameraArgsVerticalAngle = 20,
        cameraArgsLookFrom = Vec3 (-2) 2 1,
        cameraArgsLookAt = Vec3 0 0 (-1),
        cameraArgsVup = Vec3 0 1 0
      }

main :: IO ()
main = render camera world