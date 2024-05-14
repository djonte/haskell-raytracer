module Lib
  ( Color,
    Point,
    Ray (..),
    RayTracer (..),
    at,
    rayColor,
    rayGradient,
    gradient,
    HitList (..),
    Sphere (..),
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
    t :: Double,
    frontFace :: Bool
  }

setFaceNormal :: HitRecord -> Ray -> V3 Double -> HitRecord
setFaceNormal hr ray outwardNormal =
  if dot (direction ray) outwardNormal < 0
    then hr {frontFace = True, normal = outwardNormal}
    else hr {frontFace = False, normal = -outwardNormal}

degreesToRadians :: (Floating a) => a -> a
degreesToRadians degrees = degrees * pi / 180

class Object a where
  hit :: a -> Ray -> Double -> Double -> HitRecord -> Maybe HitRecord

data Sphere = Sphere
  { center :: Point,
    radius :: Double
  }

newtype HitList a = HitList {objects :: [a]}

clear :: HitList a
clear = HitList [] -- redundant?

instance (Object a) => Object (HitList a) where
  hit (HitList []) _ _ _ _ = Nothing
  hit (HitList l) ray tmin tmax hr = hit' l ray tmin tmax hr False tmax
    where
      hit' [] _ _ _ hr' hitAnything _ = if hitAnything then Just hr' else Nothing
      hit' (x : xs) ray' tmin' tmax' hr' hitAnything closest =
        let result = hit x ray' tmin' tmax' hr'
         in case result of
              Nothing -> hit' xs ray' tmin' tmax' hr' hitAnything closest
              Just hitRec -> hit' xs ray' tmin' tmax' hitRec True (t hitRec) -- investigate which hr is being used... hit shouldnt use hr so this should be fine

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
                      normal = (p' - center sphere) ^/ radius sphere
                      newHr = hr {p = p', normal = normal, t = t}
                   in setFaceNormal newHr ray normal
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

infinity :: Double
infinity = read "Infinity"

-- Function calculates the color of a given ray
-- Uses hitSphere to check if a sphere is hit by the ray
rayColor :: (Object a) => Ray -> HitList a -> Color
rayColor ray world =
  let unitDir = L.normalize $ direction ray
      a = 0.5 * (unitDir ^. _y + 1.0)
      fromColor = V3 (127 / 255) (220 / 255) (232 / 255)
      toColor = V3 (13 / 255) (70 / 255) (158 / 255)
   in case hit world ray 0 infinity (HitRecord (V3 0 0 0) (V3 0 0 0) 0 False) of
        Nothing -> a *^ fromColor + (1.0 - a) *^ toColor
        Just hr -> 0.5 *^ (normal hr + V3 1 1 1)

-- Takes a RayTracer, a pixel location i and j, returns a PixelRGB type with the color for that ray
rayGradient :: (Object a) => RayTracer -> HitList a -> Int -> Int -> Pixel RGB Double
rayGradient rt world j i =
  -- j is rows, i is columns
  let pxCenter = px00Loc rt + (fromIntegral i * pxDeltaU rt) + (fromIntegral j * pxDeltaV rt)
      rayDirection = pxCenter - cameraCenter rt
      ray = Ray (cameraCenter rt) (L.normalize rayDirection)
      px_color = rayColor ray world
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
