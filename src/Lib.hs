module Lib
  ( Color,
    Point,
    Ray (..),
    at,
    gradient,
    Object,
    HitList (..),
    Sphere (..),
    Interval (..),
    HitRecord (..),
    infinity,
    hit,
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

data HitRecord = HitRecord
  { p :: Point,
    normal :: V3 Double,
    t :: Double,
    frontFace :: Bool
  }

data Interval = Interval
  { tmin :: Double,
    tmax :: Double
  }

infinity :: Double
infinity = read "Infinity"

size interval = tmax interval - tmin interval

contains interval x = x >= tmin interval && x <= tmax interval

surrounds interval x = x > tmin interval && x < tmax interval

universe = Interval (-infinity) infinity

setFaceNormal :: HitRecord -> Ray -> V3 Double -> HitRecord
setFaceNormal hr ray outwardNormal =
  if dot (direction ray) outwardNormal < 0
    then hr {frontFace = True, normal = outwardNormal}
    else hr {frontFace = False, normal = -outwardNormal}

degreesToRadians :: (Floating a) => a -> a
degreesToRadians degrees = degrees * pi / 180

class Object a where
  hit :: a -> Ray -> Interval -> HitRecord -> Maybe HitRecord

data Sphere = Sphere
  { center :: Point,
    radius :: Double
  }

newtype HitList a = HitList {objects :: [a]}

clear :: HitList a
clear = HitList [] -- redundant?

instance (Object a) => Object (HitList a) where
  hit (HitList []) _ _ _ = Nothing
  hit (HitList l) ray interval hr = hit' l ray interval hr False (tmax interval)
    where
      hit' [] _ _ hr' hitAnything _ = if hitAnything then Just hr' else Nothing
      hit' (x : xs) ray' interval' hr' hitAnything closest =
        let result = hit x ray' interval' hr'
         in case result of
              Nothing -> hit' xs ray' interval' hr' hitAnything closest
              Just hitRec -> hit' xs ray' interval' hitRec True (t hitRec) -- investigate which hr is being used... hit shouldnt use hr so this should be fine

instance Object Sphere where
  -- hit function for a sphere
  -- returns a hit record if the sphere is hit by the ray, otherwise Nothing
  hit sphere ray interval hr =
    let oc = center sphere - origin ray
        a = quadrance $ direction ray
        h = dot (direction ray) oc
        c = quadrance oc - radius sphere ** 2
        disc = h * h - a * c
     in let sqrtd = sqrt disc
            result
              | disc < 0 = Nothing
              | not (surrounds interval root1) = if not (surrounds interval root2) then Nothing else Just hr' -- if root1 is not in the interval, check root2
              | otherwise = Just hr' -- if root1 is in the interval, return the hit record
              where
                root1 = (h - sqrtd) / a -- first root
                root2 = (h + sqrtd) / a -- second root
                hr' =
                  let t = if root1 > tmin interval && root1 < tmax interval then root1 else root2
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

-- First gradient function
gradient :: Int -> Int -> Pixel RGB Double
gradient i j =
  let (width, height) = (256, 256) -- width and height of image
      r = fromIntegral j / (width - 1) / 2 -- this should be read that the % of red increases with the column j
      g = fromIntegral i / (height - 1) / 2
      b = 1 - fromIntegral j / (width - 1) -- it follows that b <= 1 since r, g <= 1
   in PixelRGB r g b
