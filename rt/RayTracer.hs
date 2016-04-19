module RayTracer (render, createRay) where

import Camera
import Geometry
import Scene
import Vector

import Data.Maybe

data Result = Result { shape :: Shape
                     , distance :: Double }

render :: Camera -> Scene -> [Double]
render cam scene = map (fromMaybe 0) distances
    where
      distances = map (fmap ((*50) . distance)) results
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
trace (x:xs) ray = minResult (fmap (Result x) t) rest
    where t = intersect ray x
          rest = trace xs ray

createRay :: Camera -> Pixel -> Ray
createRay cam p = Ray (eye cam) (unit $ pixelAsPoint cam p `sub` eye cam)
