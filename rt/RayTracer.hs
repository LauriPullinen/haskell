module RayTracer (render, createRay) where

import Camera
import Geometry
import Light
import Scene
import Vector

import Control.Applicative
import Data.Maybe
import Prelude hiding (div)

data Result = Result { shape :: Shape
                     , traced :: Ray
                     , distance :: Double }

render :: Camera -> Scene -> [Vector]
render cam scene = map (shade scene) results
    where
      results = map (trace (geometry scene)) rays
      rays = map (createRay cam) pixels
      pixels = pixelBuffer cam

minResult :: Maybe Result -> Maybe Result -> Maybe Result
minResult Nothing Nothing = Nothing
minResult a Nothing = a
minResult Nothing b = b
minResult (Just a) (Just b)
  | distance a < distance b = Just a
  | otherwise = Just b

trace :: [Shape] -> Ray -> Maybe Result
trace [] _       = Nothing
trace (x:xs) ray = minResult (fmap (Result x ray) t) rest
    where t = intersect ray x
          rest = trace xs ray

createRay :: Camera -> Pixel -> Ray
createRay cam p = Ray (eye cam) (unit $ pixelAsPoint cam p `sub` eye cam)

shade :: Scene -> Maybe Result -> Vector
shade _ Nothing = Vector3 0 0 0
shade scene (Just result) = if obstructed
  then Vector3 0 0 0
  else color light `mult` power light `mult` abs (dot normal lightDir) `div` (lightDistance ^ 2)
  where
    obstructed = fromMaybe False $ (< lightDistance) <$> obstructionDistance
    obstructionDistance = distance <$> lightResult
    lightResult = trace (geometry scene) lightRay -- Maybe Result
    lightRay = Ray impact lightDir
    lightDistance = len impactToLight
    lightDir = unit impactToLight
    impactToLight = point light `sub` impact
    normal = getNormal (shape result) impact
    impact = origin ray `add` (direction ray `mult` distance result) `sub` (direction ray `mult` 1e-8)
    light = head $ lights scene -- TODO: Random light
    ray = traced result
