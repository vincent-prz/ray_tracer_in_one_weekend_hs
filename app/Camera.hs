{-# LANGUAGE NamedFieldPuns #-}

module Camera where

import Color
import Hittable (AnyHittable (AnyHittable), HitRecord (hitRecordNormal, hitRecordP), Hittable (hit))
import Interval
import Ray
import System.IO (hPutStrLn, stderr)
import Utils (posInfinity, randomDoubleUnit)
import Vec3 (Point, Vec3 (..), divVec3, getRandomUnitVector, mulVec3, unitVec3)

data Camera = Camera
  { cameraAspectRatio :: Double,
    cameraImageWidth :: Int,
    cameraImageheight :: Int,
    cameraCenter :: Point,
    cameraPixel00Loc :: Point,
    cameraPixelDeltaU :: Vec3,
    cameraPixelDeltaV :: Vec3,
    cameraSamplesPerPixel :: Int,
    cameraPixelSamplesScale :: Double,
    cameraMaxDepth :: Int
  }

mkCamera :: Double -> Int -> Int -> Int -> Camera
mkCamera aspectRatio imageWidth samplesPerPixel maxDepth =
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
          cameraPixelDeltaV = pixelDeltaV,
          cameraSamplesPerPixel = samplesPerPixel,
          cameraPixelSamplesScale = 1 / fromIntegral samplesPerPixel,
          cameraMaxDepth = maxDepth
        }

render :: Camera -> AnyHittable -> IO ()
render cam@(Camera {cameraImageWidth, cameraImageheight}) world = do
  putStr ("P3\n" ++ show cameraImageWidth ++ " " ++ show cameraImageheight ++ "\n255\n")
  mapM_ (displayLine cam world) [0 .. cameraImageheight - 1]

displayLine :: Camera -> AnyHittable -> Int -> IO ()
displayLine cam@(Camera {cameraImageWidth, cameraImageheight}) world j = do
  hPutStrLn stderr ("Scanlines remaining: " ++ show (cameraImageheight - j))
  colors <- sequence [getColor cam world i j | i <- [0 .. cameraImageWidth - 1]]
  mapM_ writeColor colors

getColor :: Camera -> AnyHittable -> Int -> Int -> IO Color
getColor cam@(Camera {cameraSamplesPerPixel, cameraPixelSamplesScale}) world i j =
  do
    randomColors <- sequence [getRandomColor cam world i j | _ <- [0 .. cameraSamplesPerPixel - 1]]
    return (cameraPixelSamplesScale `mulVec3` sum randomColors)

getRandomColor :: Camera -> AnyHittable -> Int -> Int -> IO Color
getRandomColor cam world i j =
  do
    randomRay <- getRandomRay cam i j
    rayColor randomRay (cameraMaxDepth cam) world

getRandomRay :: Camera -> Int -> Int -> IO Ray
getRandomRay (Camera {cameraCenter, cameraPixel00Loc, cameraPixelDeltaU, cameraPixelDeltaV}) i j = do
  offset <- sampleSquare
  let pixelCenterU = (fromIntegral i + x offset) `mulVec3` cameraPixelDeltaU
  let pixelCenterV = (fromIntegral j + y offset) `mulVec3` cameraPixelDeltaV
  let pixelCenter = cameraPixel00Loc + pixelCenterU + pixelCenterV
  let rayDirection = pixelCenter - cameraCenter
  return (Ray {origin = cameraCenter, direction = rayDirection})
  where
    sampleSquare = (\x y -> Vec3 (x - 0.5) (y - 0.5) 0) <$> randomDoubleUnit <*> randomDoubleUnit

rayColor :: Ray -> Int -> AnyHittable -> IO Color
rayColor _ 0 _ = return 0
rayColor ray depth (AnyHittable world) =
  case hit world ray (Interval 0.001 posInfinity) of
    Just record -> do
      direction <- (+ hitRecordNormal record) <$> getRandomUnitVector
      mulVec3 0.5 <$> rayColor (Ray (hitRecordP record) direction) (depth - 1) (AnyHittable world)
    Nothing ->
      let a = (1 + y (unitVec3 (direction ray))) / 2
       in return $ (1 - a) `mulVec3` 1 + a `mulVec3` Vec3 0.5 0.7 1.0
