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
import System.Random

data Camera = Camera
  { imageHeight :: Integer,
    pixelSamplesScale :: Double,
    center :: Point,
    px00Loc :: Point,
    pxDeltaU :: V3 Double,
    pxDeltaV :: V3 Double
  }

createCamera :: Double -> Integer -> Integer -> Camera
createCamera aspectRatio imageWidth samplesPerPixel =
  let imageHeight = let temp = floor (fromIntegral imageWidth / aspectRatio) in max temp 1 -- height should be at least 1
      pixel_samples_scale :: Double
      pixel_samples_scale = 1.0 / fromIntegral samplesPerPixel
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
   in Camera imageHeight pixel_samples_scale camera_center px00_loc px_delta_u px_delta_v

sampleSquare :: StdGen -> (V3 Double, StdGen)
sampleSquare gen =
  let r1 = randomZeroToOne gen
      r2 = randomZeroToOne (snd r1)
   in (V3 (fst r1 - 0.5) (fst r2 - 0.5) 0, snd r2)

getRay :: Camera -> Integer -> Integer -> StdGen -> (Ray, StdGen)
getRay cam i j gen =
  let (offset, gen') = sampleSquare gen
      pixelSample = px00Loc cam + ((fromIntegral i + offset ^. _x) *^ pxDeltaU cam) + ((fromIntegral j + offset ^. _y) *^ pxDeltaV cam)
      rayOrigin = center cam
      rayDirection = pixelSample - rayOrigin
   in (Ray rayOrigin rayDirection, gen')

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

sample :: (Object a) => Camera -> HitList a -> Integer -> Integer -> Integer -> StdGen -> Color
sample cam world samplesPerPixel i j gen = sample' (V3 0.0 0.0 0.0) samplesPerPixel gen
  where
    sample' color samples gen'
      | samples == 0 = color
      | otherwise =
          let (ray, gen'') = getRay cam i j gen'
              pxColor = color + rayColor ray world
           in sample' pxColor (samples - 1) gen''

-- Takes a RayTracer, a pixel location i and j, returns a PixelRGB type with the color for that ray
render :: (Object a) => Camera -> HitList a -> Int -> Int -> Pixel RGB Double
render cam world j i =
  -- j is rows, i is columns
  let pxCenter = px00Loc cam + (fromIntegral i * pxDeltaU cam) + (fromIntegral j * pxDeltaV cam)
      rayDirection = pxCenter - center cam
      ray = Ray (center cam) (L.normalize rayDirection)
      gen = mkStdGen 42
      px_color = pixelSamplesScale cam *^ sample cam world 100 (fromIntegral i) (fromIntegral j) gen
      intensity = Interval 0.000 0.999
      px_color_new = V3 (clamp intensity (px_color ^. _x)) (clamp intensity (px_color ^. _y)) (clamp intensity (px_color ^. _z))
   in pxRGB px_color_new
