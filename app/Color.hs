module Color where

import Interval (Interval (Interval), intervalClamps)
import Vec3

type Color = Vec3

colorRatio :: Double
colorRatio = 255.999

getColorString :: Color -> String
getColorString c = show (x c) ++ " " ++ show (y c) ++ " " ++ show (z c)

linearToGamma :: Double -> Double
linearToGamma linearComponent
  | linearComponent > 0 = sqrt linearComponent
  | otherwise = 0

writeColor :: Color -> IO ()
writeColor c =
  let intensity = Interval 0 0.999
      normalizedColor = colorRatio `mulVec3` (c `applyToVec3` (intervalClamps intensity . linearToGamma))
      rByte :: Int
      rByte = floor (x normalizedColor)
      gByte :: Int
      gByte = floor (y normalizedColor)
      bByte :: Int
      bByte = floor (z normalizedColor)
   in putStrLn (show rByte ++ " " ++ show gByte ++ " " ++ show bByte)