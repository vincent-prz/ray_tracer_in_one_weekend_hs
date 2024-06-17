module Main where

import Camera (Camera, CameraArgs (..), mkCamera, render)
import Control.Monad (zipWithM)
import Data.Maybe (catMaybes)
import Hittable (AnyHittable (AnyHittable))
import Material (AnyMaterial (AnyMaterial), Dielectric (Dielectric), Lambertian (Lambertian), Metal (Metal))
import Sphere (Sphere (..))
import Utils (randomDouble, randomDoubleUnit)
import Vec3

-- Image
aspectRatio :: Double
aspectRatio = 16 / 9

imageWidth :: Int
imageWidth = 256

groundMaterial :: Lambertian
groundMaterial = Lambertian (Vec3 0.5 0.5 0.5)

material1 :: Dielectric
material1 = Dielectric 1.5

material2 :: Lambertian
material2 = Lambertian (Vec3 0.4 0.2 0.1)

material3 :: Metal
material3 = Metal (Vec3 0.7 0.6 0.5) 0

genSpheres :: IO AnyHittable
genSpheres = AnyHittable . catMaybes <$> mapM (uncurry genSphere) [(i, j) | i <- [-11 .. 10], j <- [-11 .. 10]]

genSphere :: Int -> Int -> IO (Maybe AnyHittable)
genSphere a b = do
  let aFloat = fromIntegral a
  let bFloat = fromIntegral b
  chooseMat <- randomDoubleUnit
  sphereCenter <-
    (\r1 r2 -> Vec3 (aFloat + 0.9 * r1) 0.2 (bFloat + 0.9 * r2))
      <$> randomDoubleUnit
      <*> randomDoubleUnit
  (AnyMaterial material) <- selectMaterial chooseMat
  if lengthVec3 (sphereCenter - Vec3 4 0.2 0) > 0.9
    then
      return
        (Just (AnyHittable Sphere {center = sphereCenter, radius = 0.2, sphereMat = material}))
    else return Nothing
  where
    selectMaterial :: Double -> IO AnyMaterial
    selectMaterial mVal
      | mVal < 0.8 = AnyMaterial . Lambertian <$> ((*) <$> randomVec3 0 1 <*> randomVec3 0 1)
      | mVal < 0.95 = AnyMaterial <$> (Metal <$> randomVec3 0.5 1 <*> randomDouble 0 0.5)
      | otherwise = return (AnyMaterial (Dielectric 1.5))

camera :: Camera
camera =
  mkCamera
    CameraArgs
      { cameraArgsAspectRatio = 16 / 9,
        cameraArgsImageWidth = 1200,
        cameraArgsSamplesPerPixel = 500,
        cameraArgsMaxDepth = 50,
        cameraArgsVerticalAngle = 20,
        cameraArgsLookFrom = Vec3 13 2 3,
        cameraArgsLookAt = 0,
        cameraArgsVup = Vec3 0 1 0,
        cameraArgsDefocusAngle = 0.6,
        cameraArgsFocusDist = 10
      }

main :: IO ()
main = do
  spheres <- genSpheres
  let world =
        AnyHittable
          [ AnyHittable Sphere {center = Vec3 0 (-1000) 0, radius = 1000, sphereMat = groundMaterial},
            AnyHittable Sphere {center = Vec3 0 1 0, radius = 1.0, sphereMat = material1},
            AnyHittable Sphere {center = Vec3 (-4) 1 0, radius = 1.0, sphereMat = material2},
            AnyHittable Sphere {center = Vec3 4 1 0, radius = 1.0, sphereMat = material3},
            spheres
          ]
  render camera world