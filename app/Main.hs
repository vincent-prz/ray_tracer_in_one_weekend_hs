module Main where

import Color (Color, writeColor)
import Ray (Ray (..))
import System.IO (hPutStrLn, stderr)
import Vec3 (Point, Vec3 (..), divVec3, dotProduct, mulVec3, unitVec3)

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
hitSphere :: Point -> Double -> Ray -> Bool
hitSphere center radius ray =
  let d = direction ray
      qc = center - origin ray
      a = dotProduct d d
      b = -(2 * dotProduct d qc)
      c = dotProduct qc qc - radius * radius
      discrimininant = b * b - 4 * a * c
   in discrimininant >= 0

rayColor :: Ray -> Color
rayColor ray =
  if hitSphere (Vec3 0 0 (-1)) 0.5 ray
    then Vec3 1 0 0
    else
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