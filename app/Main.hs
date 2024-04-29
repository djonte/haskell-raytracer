module Main (main) where

-- import Lib not needed yet?
import Graphics.Image as I
import Linear.V3

type Color = V3 Double -- this might not be needed
type Point = V3 Double

data Ray = Ray {
  origin :: Point,
  direction :: V3 Double
}

at :: Ray -> V3 Double -> Point
at ray t = origin ray + direction ray * t

main :: IO ()
main = do
  let aspect_ratio = 16.0 / 9.0
  let width = 800
  let height = let
    temp = toInt (width / aspect_ratio)
    in if temp < 1 then 1 else temp

  vw_width = 2.0
  vw_height = vw_width * (fromIntegral width / height)
  

  let grad_color :: Image VU RGB Double
      grad_color = makeImageR VU (256, 256) (uncurry gradient) -- uncurry will transform gradient into (Int, Int) -> ...

  writeImage "app/images/grad_color.png" grad_color


gradient :: Int -> Int -> Pixel RGB Double
gradient i j =
  let 
    (width, height) = (256, 256) -- width and height of image
    r = fromIntegral j / (width - 1) / 2-- this should be read that the % of red increases with the column j
    g = fromIntegral i / (height - 1) / 2
    b = 1 - fromIntegral j / (width - 1) -- it follows that b <= 1 since r, g <= 1
   in PixelRGB r g b
