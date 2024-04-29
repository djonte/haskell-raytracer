module Main (main) where

-- import Lib not needed yet?
import Graphics.Image as I
import Linear.V3

-- Aliases for V3
type Color = V3 Double -- this might not be needed

type Point = V3 Double

-- Ray data type, has an origin and a direction
data Ray = Ray
  { origin :: Point,
    direction :: V3 Double
  }

-- Returns the ray's coordinates depending on t
at :: Ray -> V3 Double -> Point
at ray t = origin ray + direction ray * t

-- Function calculates the color of a given ray
rayColor :: Ray -> Color
rayColor ray = V3 0 0 0

main :: IO ()
main = do
  let aspect_ratio :: Double
      aspect_ratio = 16.0 / 9.0 -- 16/9 aspect ratio
  let width :: Integer
      width = 800 -- image width
  let height :: Integer
      height =
        let temp = floor (fromIntegral width / aspect_ratio)
         in max temp 1 -- height should be at least 1
  let vw_width :: Double
      vw_width = 2.0 -- Viewport width
  let vw_height :: Double
      vw_height = vw_width * (fromIntegral width / fromIntegral height) -- Viewport height
  let grad_color :: Image VU RGB Double
      grad_color = makeImageR VU (256, 256) (uncurry gradient) -- uncurry will transform gradient into (Int, Int) -> ...
  writeImage "app/images/grad_color.png" grad_color

gradient :: Int -> Int -> Pixel RGB Double
gradient i j =
  let (width, height) = (256, 256) -- width and height of image
      r = fromIntegral j / (width - 1) / 2 -- this should be read that the % of red increases with the column j
      g = fromIntegral i / (height - 1) / 2
      b = 1 - fromIntegral j / (width - 1) -- it follows that b <= 1 since r, g <= 1
   in PixelRGB r g b
