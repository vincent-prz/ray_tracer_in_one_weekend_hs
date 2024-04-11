module Main where

import Color (Color, writeColor)
import System.IO (hPutStrLn, stderr)
import Vec3 (Vec3 (..))

imageWidth :: Int
imageWidth = 256

imageHeight :: Int
imageHeight = 256

getColor :: Int -> Int -> Color
getColor i j =
  let red = fromIntegral i / fromIntegral (imageWidth - 1)
      green = fromIntegral j / fromIntegral (imageHeight - 1)
      blue = 0
   in Vec3 {x = red, y = green, z = blue}

displayLine :: Int -> IO ()
displayLine j = do
  hPutStrLn stderr ("Scanlines remaining: " ++ show (imageHeight - j))
  mapM_ writeColor [getColor i j | i <- [0 .. imageWidth - 1]]

main :: IO ()
main = do
  putStr ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255\n")
  mapM_ displayLine [0 .. imageHeight - 1]