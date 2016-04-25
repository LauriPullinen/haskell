module RayTracer (createPrimaryRay, createMirrorReflectionRay, trace) where

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

createPrimaryRay :: Camera -> Pixel -> Ray
createPrimaryRay cam p = Ray (eye cam) (unit $ pixelAsPoint cam p `sub` eye cam)

createMirrorReflectionRay :: Result -> Ray
createMirrorReflectionRay result = Ray impact outgoing
  where
    impact = getImpact result
    incoming = direction (traced result)
    normal = getImpactNormal result
    outgoing = incoming `sub` mult normal (2 * dot incoming normal)
