module Main (main) where

-- import Lib not needed yet?
import Graphics.Image as I

main :: IO ()
main = do
  let grad_color :: Image VU RGB Double
      grad_color = makeImageR VU (256, 256) (uncurry gradient) -- uncurry will transform gradient into (Int, Int) -> ...

  writeImage "app/images/grad_color.png" grad_color


gradient :: Int -> Int -> Pixel RGB Double
gradient i j =
  let 
    (width, height) = (256, 256) -- width and height of image
    r = fromIntegral i / (width - 1)
    g = fromIntegral j / (height - 1)
    b = r * g -- it follows that b <= 1 since r, g <= 1
   in PixelRGB r g b
