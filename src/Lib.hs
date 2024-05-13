module Lib
  ( Color,
    Point,
    Ray (..),
    RayTracer (..),
    at,
    rayColor,
    rayGradient,
    gradient,
  )
where

import Control.Lens hiding (at)
import Graphics.Image as I
import Linear.Metric as L (Metric (dot, quadrance), normalize)
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
  { px00Loc :: Point,
    pxDeltaU :: V3 Double,
    pxDeltaV :: V3 Double,
    cameraCenter :: Point
  }

data HitRecord = HitRecord
  { p :: Point,
    normal :: V3 Double,
    t :: Double
  }

class Object a where
  hit :: a -> Ray -> Double -> Double -> HitRecord -> Maybe HitRecord

data Sphere = Sphere
  { center :: Point,
    radius :: Double
  }

instance Object Sphere where
  -- hit function for a sphere
  -- returns a hit record if the sphere is hit by the ray, otherwise Nothing
  hit sphere ray tmin tmax hr =
    let oc = center sphere - origin ray
        a = quadrance $ direction ray
        h = dot (direction ray) oc
        c = quadrance oc - radius sphere ** 2
        disc = h * h - a * c
     in let sqrtd = sqrt disc
            result
              | disc < 0 = Nothing
              | root1 <= tmin || root1 >= tmax = if root2 <= tmin || root2 >= tmax then Nothing else Just hr' -- if root1 is not in the interval, check root2
              | otherwise = Just hr' -- if root1 is in the interval, return the hit record
              where
                root1 = (h - sqrtd) / a -- first root
                root2 = (h + sqrtd) / a -- second root
                hr' =
                  let t = if root1 > tmin && root1 < tmax then root1 else root2
                      p' = at ray t
                      normal = (p hr - center sphere) ^/ radius sphere
                   in HitRecord p' normal t
         in result

-- Returns the ray's coordinates depending on t
at :: Ray -> Double -> Point
at ray t = origin ray + t *^ direction ray

-- Takes a center point, a radius and a ray and calculates whether the ray hits
-- the circle formed by the center point and radius
hitSphere :: Point -> Double -> Ray -> Double
hitSphere center radius ray =
  let oc = center - origin ray
      a = quadrance $ direction ray
      h = dot (direction ray) oc
      c = quadrance oc - radius * radius
      disc = h * h - a * c
   in if disc < 0
        then -1.0
        else (h - sqrt disc) / a

-- Function calculates the color of a given ray
-- Uses hitSphere to check if a sphere is hit by the ray
rayColor :: Ray -> Color
rayColor ray =
  let unitDir = L.normalize $ direction ray
      a = 0.5 * (unitDir ^. _y + 1.0)
      fromColor = V3 (127 / 255) (220 / 255) (232 / 255)
      toColor = V3 (13 / 255) (70 / 255) (158 / 255)
      t = hitSphere (V3 0.0 0.0 (-1.0)) 0.5 ray
      n = L.normalize (at ray t - V3 0.0 0.0 (-1.0))
      diff = 0.5 * (n ^. _z + 1)
      color = 0.5 * V3 0 (2 * (diff ** 8)) (2 * (diff ** 8))
   in if t > 0.0
        then color
        else (1.0 - a) *^ toColor + a *^ fromColor

-- Takes a RayTracer, a pixel location i and j, returns a PixelRGB type with the color for that ray
rayGradient :: RayTracer -> Int -> Int -> Pixel RGB Double
rayGradient rt j i =
  -- j is rows, i is columns
  let pxCenter = px00Loc rt + (fromIntegral i * pxDeltaU rt) + (fromIntegral j * pxDeltaV rt)
      rayDirection = pxCenter - cameraCenter rt
      ray = Ray (cameraCenter rt) (L.normalize rayDirection)
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
