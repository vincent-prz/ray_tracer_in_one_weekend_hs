module Main where

import System.IO (hPutStrLn, stderr)

imageWidth :: Int
imageWidth = 256

imageHeight :: Int
imageHeight = 256

colorRatio :: Double
colorRatio = 255.999

data Color = Color {r :: Int, g :: Int, b :: Int}

getColorString :: Color -> String
getColorString c = show (r c) ++ " " ++ show (g c) ++ " " ++ show (b c)

getColor :: Int -> Int -> Color
getColor x y =
  let red = colorRatio * fromIntegral x / fromIntegral (imageWidth - 1)
      green = colorRatio * fromIntegral y / fromIntegral (imageHeight - 1)
      blue = 0
   in Color {r = round red, g = round green, b = blue}

displayLine :: Int -> IO ()
displayLine y = do
  hPutStrLn stderr ("Scanlines remaining: " ++ show (imageHeight - y))
  mapM_ (putStrLn . getColorString) [getColor x y | x <- [0 .. imageWidth - 1]]

main :: IO ()
main = do
  putStr ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255\n")
  mapM_ displayLine [0 .. imageHeight - 1]