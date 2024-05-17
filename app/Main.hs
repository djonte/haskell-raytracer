module Main (main) where

import Camera
import Graphics.Image
import Lib
import Linear.V3

main :: IO ()
main = do
  -- Camera setup
  let width :: Integer
      aspectRatio :: Double
      width = 400
      aspectRatio = 16.0 / 9.0
      samplesPerPixel = 100
      camera = createCamera aspectRatio width samplesPerPixel
  -- World
  let world :: HitList Sphere
      world =
        HitList
          [ Sphere (V3 0 (-100.5) (-1)) 100,
            Sphere (V3 (-0.9) (-0.1) (-1.3)) 0.4,
            Sphere (V3 (1.3) (-0.1) (-1.5)) 0.4,
            Sphere (V3 (-0.0) 0 (-1)) 0.5
          ]

  -- Image generation
  let image :: Image VU RGB Double
      image = makeImageR VU (fromInteger (imageHeight camera), fromInteger width) (uncurry (render camera world)) -- uncurry will transform gradient into (Int, Int) -> ...
  writeImage "app/images/raytracing.jpg" image
