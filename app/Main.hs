module Main where

import Color (Color, writeColor)
import Ray (Ray (..), rayAt)
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
hitSphere :: Point -> Double -> Ray -> Maybe Double
hitSphere center radius ray =
  let d = direction ray
      qc = center - origin ray
      a = dotProduct d d
      -- let's use h such as b = - 2 * h, to simplify the equations
      h = dotProduct d qc
      c = dotProduct qc qc - radius * radius
      discrimininant = h * h - a * c
   in if discrimininant >= 0 then Just ((h - sqrt discrimininant) / a) else Nothing

rayColor :: Ray -> Color
rayColor ray =
  case hitSphere (Vec3 0 0 (-1)) 0.5 ray of
    Just t ->
      let n = unitVec3 (rayAt ray t - Vec3 0 0 (-1))
       in 0.5 `mulVec3` (n + 1)
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