{-# LANGUAGE NamedFieldPuns #-}

module Camera where

import Color
import Hittable (AnyHittable (AnyHittable), HitRecord (hitRecordNormal), Hittable (hit))
import Interval
import Ray
import System.IO (hPutStrLn, stderr)
import Utils (posInfinity)
import Vec3 (Point, Vec3 (..), divVec3, mulVec3, unitVec3)

data Camera = Camera
  { cameraAspectRatio :: Double,
    cameraImageWidth :: Int,
    cameraImageheight :: Int,
    cameraCenter :: Point,
    cameraPixel00Loc :: Point,
    cameraPixelDeltaU :: Vec3,
    cameraPixelDeltaV :: Vec3
  }

mkCamera :: Double -> Int -> Camera
mkCamera aspectRatio imageWidth =
  let imageHeight = let val = round (fromIntegral imageWidth / aspectRatio) in max val 1
      focalLength :: Double
      focalLength = 1.0

      viewPortHeight :: Double
      viewPortHeight = 2.0

      viewPortWidth :: Double
      viewPortWidth = viewPortHeight * (fromIntegral imageWidth / fromIntegral imageHeight)

      center :: Point
      center = 0

      viewPortU :: Vec3
      viewPortU = Vec3 viewPortWidth 0 0

      viewPortV :: Vec3
      viewPortV = Vec3 0 (-viewPortHeight) 0

      pixelDeltaU :: Vec3
      pixelDeltaU = viewPortU `divVec3` fromIntegral imageWidth

      pixelDeltaV :: Vec3
      pixelDeltaV = viewPortV `divVec3` fromIntegral imageHeight

      viewPortUpperLeft :: Vec3
      viewPortUpperLeft = center - Vec3 0 0 focalLength - viewPortU `divVec3` 2 - viewPortV `divVec3` 2

      pixel00Loc :: Vec3
      pixel00Loc = viewPortUpperLeft + 0.5 `mulVec3` (pixelDeltaU + pixelDeltaV)
   in Camera
        { cameraAspectRatio = aspectRatio,
          cameraImageWidth = imageWidth,
          cameraImageheight = imageHeight,
          cameraCenter = center,
          cameraPixel00Loc = pixel00Loc,
          cameraPixelDeltaU = pixelDeltaU,
          cameraPixelDeltaV = pixelDeltaV
        }

render :: Camera -> AnyHittable -> IO ()
render cam@(Camera {cameraImageWidth, cameraImageheight}) world = do
  putStr ("P3\n" ++ show cameraImageWidth ++ " " ++ show cameraImageheight ++ "\n255\n")
  mapM_ (displayLine cam world) [0 .. cameraImageheight - 1]

displayLine :: Camera -> AnyHittable -> Int -> IO ()
displayLine cam@(Camera {cameraImageWidth, cameraImageheight}) world j = do
  hPutStrLn stderr ("Scanlines remaining: " ++ show (cameraImageheight - j))
  mapM_ writeColor [getColor cam world i j | i <- [0 .. cameraImageWidth - 1]]

getColor :: Camera -> AnyHittable -> Int -> Int -> Color
getColor (Camera {cameraCenter, cameraPixel00Loc, cameraPixelDeltaU, cameraPixelDeltaV}) world i j =
  let pixelCenter = cameraPixel00Loc + (fromIntegral i `mulVec3` cameraPixelDeltaU) + (fromIntegral j `mulVec3` cameraPixelDeltaV)
      rayDirection = pixelCenter - cameraCenter
      ray = Ray {origin = cameraCenter, direction = rayDirection}
   in rayColor ray world

rayColor :: Ray -> AnyHittable -> Color
rayColor ray (AnyHittable world) =
  case hit world ray (Interval 0 posInfinity) of
    Just record -> 0.5 `mulVec3` (hitRecordNormal record + 1)
    Nothing ->
      let a = (1 + y (unitVec3 (direction ray))) / 2
       in (1 - a) `mulVec3` 1 + a `mulVec3` Vec3 0.5 0.7 1.0
