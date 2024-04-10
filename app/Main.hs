module Main where

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

main :: IO ()
main = do
  putStr ("P3\n" ++ show imageWidth ++ " " ++ show imageHeight ++ "\n255\n")
  let colors = [getColor x y | y <- [0 .. imageHeight - 1], x <- [0 .. imageWidth - 1]]
  let colorStrings = map getColorString colors
  mapM_ putStrLn colorStrings
