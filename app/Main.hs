module Main where

import Color (Color, writeColor)
import Hittable (AnyHittable (..), HitRecord (..), hit)
import Ray (Ray (..))
import Sphere (Sphere (..))
import System.IO (hPutStrLn, stderr)
import Vec3 (Point, Vec3 (..), divVec3, mulVec3, unitVec3)

-- positive infinity
maxDouble :: Double
maxDouble = 1 / 0

-- Image
aspectRatio :: Double
aspectRatio = 16 / 9

imageWidth :: Int
imageWidth = 256

imageHeight :: Int
imageHeight =
  let val = round (fromIntegral imageWidth / aspectRatio)
   in max val 1

-- Camera
focalLength :: Double
focalLength = 1.0

viewPortHeight :: Double
viewPortHeight = 2.0

viewPortWidth :: Double
viewPortWidth = viewPortHeight * (fromIntegral imageWidth / fromIntegral imageHeight)

cameraCenter :: Point
cameraCenter = 0

viewPortU :: Vec3
viewPortU = Vec3 viewPortWidth 0 0

viewPortV :: Vec3
viewPortV = Vec3 0 (-viewPortHeight) 0

pixelDeltaU :: Vec3
pixelDeltaU = viewPortU `divVec3` fromIntegral imageWidth

pixelDeltaV :: Vec3
pixelDeltaV = viewPortV `divVec3` fromIntegral imageHeight

viewPortUpperLeft :: Point
viewPortUpperLeft = cameraCenter - Vec3 0 0 focalLength - viewPortU `divVec3` 2 - viewPortV `divVec3` 2

pixel00Loc :: Point
pixel00Loc = viewPortUpperLeft + 0.5 `mulVec3` (pixelDeltaU + pixelDeltaV)

-- Render

world :: [AnyHittable]
world =
  [ AnyHittable Sphere {center = Vec3 0 0 (-1), radius = 0.5},
    AnyHittable Sphere {center = Vec3 0 (-100.5) (-1), radius = 100}
  ]

rayColor :: Ray -> Color
rayColor ray =
  case hit world ray (0, maxDouble) of
    Just record -> 0.5 `mulVec3` (hitRecordNormal record + 1)
    Nothing ->
      let a = (1 + y (unitVec3 (direction ray))) / 2
       in (1 - a) `mulVec3` 1 + a `mulVec3` Vec3 0.5 0.7 1.0

getColor :: Int -> Int -> Color
getColor i j =
  let pixelCenter = pixel00Loc + (fromIntegral i `mulVec3` pixelDeltaU) + (fromIntegral j `mulVec3` pixelDeltaV)
      rayDirection = pixelCenter - cameraCenter
      ray = Ray {origin = cameraCenter, direction = rayDirection}
   in rayColor ray

displayLine :: Int -> IO ()
displayLine j = do
  hPutStrLn stderr ("Scanlines remaining: " ++ show (imageHeight - j))
  mapM_ writeColor [getColor i j | i <- [0 .. imageWidth - 1]]

main :: IO ()
main = do
  putStr ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255\n")
  mapM_ displayLine [0 .. imageHeight - 1]