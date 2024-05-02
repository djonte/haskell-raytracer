module Lib
    ( Color
    , Point
    , Ray(..)
    , RayTracer(..)
    , at
    , rayColor
    , rayGradient
    , gradient
    ) where

import Control.Lens hiding (at)
import Graphics.Image as I
import Linear.Metric as L (normalize)
import Linear.V3
import Linear.Vector

-- Aliases for V3
type Color = V3 Double -- this might not be needed

type Point = V3 Double

-- Ray data type, has an origin and a direction
data Ray = Ray
  { origin :: Point,
    direction :: V3 Double
  }

-- RayTracer data type to hold relevant constants (those that need to be visible at several places)
data RayTracer = RayTracer
  { px00_loc :: Point,
    px_delta_u :: V3 Double,
    px_delta_v :: V3 Double,
    camera_center :: Point
  }

-- Returns the ray's coordinates depending on t
at :: Ray -> V3 Double -> Point
at ray t = origin ray + direction ray * t

-- Function calculates the color of a given ray
rayColor :: Ray -> Color
rayColor ray =
  let unit_dir = L.normalize $ direction ray
      a = 0.5 * (unit_dir ^. _y + 1.0)
      -- toColor = V3 1.0 1.0 1.0 og 
      -- fromColor = V3 0.5 0.7 1.0
      fromColor = V3 (127/255) (220/255) (232/255)
      toColor = V3 (13 / 255) (70 / 255) (158 / 255)
   in (1.0 - a) *^ toColor + a *^ fromColor

-- Takes a RayTracer, a pixel location i and j, returns a PixelRGB type with the color for that ray
rayGradient :: RayTracer -> Int -> Int -> Pixel RGB Double
rayGradient rt j i =
  -- j is rows, i is columns
  let px_center = px00_loc rt + (fromIntegral i * px_delta_u rt) + (fromIntegral j * px_delta_v rt)
      ray_direction = px_center - camera_center rt
      ray = Ray (camera_center rt) ray_direction
      px_color = rayColor ray
   in pxRGB px_color

-- Custom functions so Color as a vector can be used
pxRGB :: Color -> Pixel RGB Double
pxRGB color = PixelRGB (color ^. _x) (color ^. _y) (color ^. _z)

-- First gradient function
gradient :: Int -> Int -> Pixel RGB Double
gradient i j =
  let (width, height) = (256, 256) -- width and height of image
      r = fromIntegral j / (width - 1) / 2 -- this should be read that the % of red increases with the column j
      g = fromIntegral i / (height - 1) / 2
      b = 1 - fromIntegral j / (width - 1) -- it follows that b <= 1 since r, g <= 1
   in PixelRGB r g b
