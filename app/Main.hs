module Main (main) where

import Graphics.Image
import Lib
import Linear.V3

main :: IO ()
main = do
  let aspect_ratio :: Double
      aspect_ratio = 16.0 / 9.0 -- 16/9 aspect ratio
  let width :: Integer
      width = 400 -- image width
  let height :: Integer
      height =
        let temp = floor (fromIntegral width / aspect_ratio)
         in max temp 1 -- height should be at least 1

  -- Camera
  let focal_length :: Double
      focal_length = 1.0
  let vw_height :: Double
      vw_height = 2.0 -- Viewport width
  let vw_width :: Double
      vw_width = vw_height * (fromIntegral width / fromIntegral height) -- Viewport height
  let camera_center :: Point
      camera_center = V3 0 0 0

  -- Viewport vectors
  let vw_u :: V3 Double
      vw_u = V3 vw_width 0 0 -- vector u is along the x axis, therefor same as viewport width in x axis
  let vw_v :: V3 Double
      vw_v = V3 0 (-vw_height) 0 -- negative to inverse y axis (since the image function has y=0 in the top)

  -- "viewport width/height per pixel"
  let px_delta_u :: V3 Double
      px_delta_u = vw_u / fromIntegral width
  let px_delta_v :: V3 Double
      px_delta_v = vw_v / fromIntegral height

  -- top left corner of view port in relation to camera center
  let vw_top_left :: V3 Double
      vw_top_left = camera_center - V3 0 0 focal_length - vw_u / 2.0 - vw_v / 2.0
  let px00_loc :: Point
      px00_loc = vw_top_left + 0.5 * (px_delta_u + px_delta_v) -- offset from side is half of delta

  -- Store constants
  let rt :: RayTracer
      rt = RayTracer px00_loc px_delta_u px_delta_v camera_center

  -- World
  let world :: HitList Sphere
      world =
        HitList
          [ Sphere (V3 0 (-100.5) (-1)) 100,
            Sphere (V3 0 0 (-1)) 0.7
          ]

  -- Image generation
  let image :: Image VU RGB Double
      image = makeImageR VU (fromInteger height, fromInteger width) (uncurry (rayGradient rt world)) -- uncurry will transform gradient into (Int, Int) -> ...
  writeImage "app/images/raytracing.jpg" image
