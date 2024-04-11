module Color where

import Vec3

type Color = Vec3

colorRatio :: Double
colorRatio = 255.999

getColorString :: Color -> String
getColorString c = show (x c) ++ " " ++ show (y c) ++ " " ++ show (z c)

writeColor :: Color -> IO ()
writeColor c =
  let normalizedColor = colorRatio `mulVec3` c
      rByte :: Int
      rByte = round (x normalizedColor)
      gByte :: Int
      gByte = round (y normalizedColor)
      bByte :: Int
      bByte = round (z normalizedColor)
   in putStrLn (show rByte ++ " " ++ show gByte ++ " " ++ show bByte)