module Camera
  ( rayColor,
    render,
    Camera (..),
    createCamera,
  )
where

import Control.Lens hiding (at)
import Graphics.Image as I
import Lib hiding (center)
import Linear.Metric as L (normalize)
import Linear.V3
import Linear.Vector

data Camera = Camera
  { imageHeight :: Integer,
    center :: Point,
    px00Loc :: Point,
    pxDeltaU :: V3 Double,
    pxDeltaV :: V3 Double
  }

createCamera :: Double -> Integer -> Camera
createCamera aspectRatio imageWidth =
  let imageHeight = let temp = floor (fromIntegral imageWidth / aspectRatio) in max temp 1 -- height should be at least 1
  -- Camera
      focal_length = 1.0
      vw_height = 2.0 -- Viewport width
      vw_width = vw_height * (fromIntegral imageWidth / fromIntegral imageHeight) -- Viewport height
      camera_center = V3 0 0 0

      -- Viewport vectors
      vw_u = V3 vw_width 0 0 -- vector u is along the x axis, therefor same as viewport width in x axis
      vw_v = V3 0 (-vw_height) 0 -- negative to inverse y axis (since the image function has y=0 in the top)

      -- "viewport width/height per pixel"
      px_delta_u = vw_u / fromIntegral imageWidth
      px_delta_v = vw_v / fromIntegral imageHeight

      -- top left corner of view port in relation to camera center
      vw_top_left = camera_center - V3 0 0 focal_length - vw_u / 2.0 - vw_v / 2.0
      px00_loc = vw_top_left + 0.5 * (px_delta_u + px_delta_v) -- offset from side is half of delta
   in Camera imageHeight camera_center px00_loc px_delta_u px_delta_v

-- Custom functions so Color as a vector can be used
pxRGB :: Color -> Pixel RGB Double
pxRGB color = PixelRGB (color ^. _x) (color ^. _y) (color ^. _z)

-- Function calculates the color of a given ray
-- Uses hitSphere to check if a sphere is hit by the ray
rayColor :: (Object a) => Ray -> HitList a -> Color
rayColor ray world =
  let unitDir = L.normalize $ direction ray
      a = 0.5 * (unitDir ^. _y + 1.0)
      fromColor = V3 (127 / 255) (220 / 255) (232 / 255)
      toColor = V3 (13 / 255) (70 / 255) (158 / 255)
   in case hit world ray (Interval 0 infinity) (HitRecord (V3 0 0 0) (V3 0 0 0) 0 False) of
        Nothing -> a *^ fromColor + (1.0 - a) *^ toColor
        Just hr -> 0.5 *^ (normal hr + V3 1 1 1)

-- Takes a RayTracer, a pixel location i and j, returns a PixelRGB type with the color for that ray
render :: (Object a) => Camera -> HitList a -> Int -> Int -> Pixel RGB Double
render cam world j i =
  -- j is rows, i is columns
  let pxCenter = px00Loc cam + (fromIntegral i * pxDeltaU cam) + (fromIntegral j * pxDeltaV cam)
      rayDirection = pxCenter - center cam
      ray = Ray (center cam) (L.normalize rayDirection)
      px_color = rayColor ray world
   in pxRGB px_color
