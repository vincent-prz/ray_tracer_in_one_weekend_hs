{-# LANGUAGE NamedFieldPuns #-}

module Camera where

import Color
import Control.Monad.State (evalState, replicateM)
import Hittable (AnyHittable (AnyHittable), HitRecord (HitRecord, hitRecordMat), Hittable (hit))
import Interval
import Material (Material (scatter))
import Ray
import System.Random (StdGen)
import Utils (RandomState, degreesToRadian, posInfinity, randomDoubleUnit)
import Vec3 (Point, Vec3 (..), crossVec3, divVec3, getRandomInUnitDisk, mulVec3, unitVec3)

-- TODO: add default args
data CameraArgs = CameraArgs
  { cameraArgsAspectRatio :: Double,
    cameraArgsImageWidth :: Int,
    cameraArgsSamplesPerPixel :: Int,
    cameraArgsMaxDepth :: Int,
    cameraArgsVerticalAngle :: Double,
    cameraArgsLookFrom :: Point,
    cameraArgsLookAt :: Point,
    cameraArgsVup :: Vec3,
    cameraArgsDefocusAngle :: Double,
    cameraArgsFocusDist :: Double
  }

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
    cameraMaxDepth :: Int,
    cameraU :: Vec3,
    cameraV :: Vec3,
    cameraW :: Vec3,
    cameraDefocusAngle :: Double,
    cameraDefocusU :: Vec3,
    cameraDefocusV :: Vec3
  }

mkCamera :: CameraArgs -> Camera
mkCamera
  ( CameraArgs
      { cameraArgsAspectRatio,
        cameraArgsImageWidth,
        cameraArgsSamplesPerPixel,
        cameraArgsMaxDepth,
        cameraArgsVerticalAngle,
        cameraArgsLookFrom,
        cameraArgsLookAt,
        cameraArgsVup,
        cameraArgsDefocusAngle,
        cameraArgsFocusDist
      }
    ) =
    let imageHeight = let val = round (fromIntegral cameraArgsImageWidth / cameraArgsAspectRatio) in max val 1
        theta :: Double
        theta = degreesToRadian cameraArgsVerticalAngle

        viewPortHeight :: Double
        viewPortHeight = 2.0 * tan (theta / 2) * cameraArgsFocusDist

        viewPortWidth :: Double
        viewPortWidth = viewPortHeight * (fromIntegral cameraArgsImageWidth / fromIntegral imageHeight)

        center :: Point
        center = cameraArgsLookFrom

        w :: Vec3
        w = unitVec3 (cameraArgsLookFrom - cameraArgsLookAt)

        u :: Vec3
        u = unitVec3 (crossVec3 cameraArgsVup w)

        v :: Vec3
        v = crossVec3 w u

        viewPortU :: Vec3
        viewPortU = mulVec3 viewPortWidth u

        viewPortV :: Vec3
        viewPortV = -mulVec3 viewPortHeight v

        pixelDeltaU :: Vec3
        pixelDeltaU = viewPortU `divVec3` fromIntegral cameraArgsImageWidth

        pixelDeltaV :: Vec3
        pixelDeltaV = viewPortV `divVec3` fromIntegral imageHeight

        viewPortUpperLeft :: Vec3
        viewPortUpperLeft = center - mulVec3 cameraArgsFocusDist w - viewPortU `divVec3` 2 - viewPortV `divVec3` 2

        pixel00Loc :: Vec3
        pixel00Loc = viewPortUpperLeft + 0.5 `mulVec3` (pixelDeltaU + pixelDeltaV)

        defocusAngle :: Double
        defocusAngle = degreesToRadian cameraArgsDefocusAngle

        defocusRadius :: Double
        defocusRadius = cameraArgsFocusDist * tan (defocusAngle / 2)
     in Camera
          { cameraAspectRatio = cameraArgsAspectRatio,
            cameraImageWidth = cameraArgsImageWidth,
            cameraImageheight = imageHeight,
            cameraCenter = center,
            cameraPixel00Loc = pixel00Loc,
            cameraPixelDeltaU = pixelDeltaU,
            cameraPixelDeltaV = pixelDeltaV,
            cameraSamplesPerPixel = cameraArgsSamplesPerPixel,
            cameraPixelSamplesScale = 1 / fromIntegral cameraArgsSamplesPerPixel,
            cameraMaxDepth = cameraArgsMaxDepth,
            cameraU = u,
            cameraV = v,
            cameraW = w,
            cameraDefocusAngle = cameraArgsDefocusAngle,
            cameraDefocusU = mulVec3 defocusRadius u,
            cameraDefocusV = mulVec3 defocusRadius v
          }

render :: Camera -> AnyHittable -> StdGen -> IO ()
render cam@(Camera {cameraImageWidth, cameraImageheight}) world gen = do
  putStr ("P3\n" ++ show cameraImageWidth ++ " " ++ show cameraImageheight ++ "\n255\n")
  let colors = evalState (getPixelColors cam world) gen
  mapM_ writeColor colors

getPixelColors :: Camera -> AnyHittable -> RandomState [Color]
getPixelColors cam@(Camera {cameraImageWidth, cameraImageheight}) world = do
  sequence [getColor cam world i j | j <- [0 .. cameraImageheight - 1], i <- [0 .. cameraImageWidth - 1]]

getColor :: Camera -> AnyHittable -> Int -> Int -> RandomState Color
getColor cam@(Camera {cameraSamplesPerPixel, cameraPixelSamplesScale}) world i j =
  do
    randomColors <- replicateM cameraSamplesPerPixel (getRandomColor cam world i j)
    return (cameraPixelSamplesScale `mulVec3` sum randomColors)

getRandomColor :: Camera -> AnyHittable -> Int -> Int -> RandomState Color
getRandomColor cam world i j =
  do
    randomRay <- getRandomRay cam i j
    rayColor randomRay (cameraMaxDepth cam) world

getRandomRay :: Camera -> Int -> Int -> RandomState Ray
getRandomRay
  ( Camera
      { cameraCenter,
        cameraPixel00Loc,
        cameraPixelDeltaU,
        cameraPixelDeltaV,
        cameraDefocusAngle,
        cameraDefocusU,
        cameraDefocusV
      }
    )
  i
  j = do
    offset <- sampleSquare
    let pixelCenterU = (fromIntegral i + x offset) `mulVec3` cameraPixelDeltaU
    let pixelCenterV = (fromIntegral j + y offset) `mulVec3` cameraPixelDeltaV
    let pixelCenter = cameraPixel00Loc + pixelCenterU + pixelCenterV
    rayOrigin <-
      if cameraDefocusAngle <= 0
        then return cameraCenter
        else defocusDiskSample
    let rayDirection = pixelCenter - rayOrigin

    return (Ray {origin = rayOrigin, direction = rayDirection})
    where
      sampleSquare = (\x y -> Vec3 (x - 0.5) (y - 0.5) 0) <$> randomDoubleUnit <*> randomDoubleUnit
      defocusDiskSample =
        ( \(Vec3 x y _) ->
            cameraCenter + mulVec3 x cameraDefocusU + mulVec3 y cameraDefocusV
        )
          <$> getRandomInUnitDisk

rayColor :: Ray -> Int -> AnyHittable -> RandomState Color
rayColor _ 0 _ = return 0
rayColor ray depth (AnyHittable world) =
  case hit world ray (Interval 0.001 posInfinity) of
    Just record@(HitRecord {hitRecordMat}) -> do
      scattered <- scatter hitRecordMat ray record
      case scattered of
        Nothing -> return 0
        Just (attenuation, scatteredRay) ->
          (* attenuation) <$> rayColor scatteredRay (depth - 1) (AnyHittable world)
    Nothing ->
      let a = (1 + y (unitVec3 (direction ray))) / 2
       in return $ (1 - a) `mulVec3` 1 + a `mulVec3` Vec3 0.5 0.7 1.0
