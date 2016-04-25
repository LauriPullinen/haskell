module RayTracer (createRay, trace) where

import Camera
import Geometry
import Object
import RayTracerResult
import Vector

trace :: [Object] -> Ray -> Maybe Result
trace [] _       = Nothing
trace (x:xs) ray = minResult (fmap (Result x ray) t) rest
    where t = intersect ray (shape x)
          rest = trace xs ray

createRay :: Camera -> Pixel -> Ray
createRay cam p = Ray (eye cam) (unit $ pixelAsPoint cam p `sub` eye cam)
