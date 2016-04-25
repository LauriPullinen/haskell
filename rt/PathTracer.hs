module PathTracer (renderPathTracing) where

import Camera
import Geometry
import RayTracer
import Scene
import Shader
import Vector

renderPathTracing :: Camera -> Scene -> Int -> [Vector]
renderPathTracing camera scene depth = map (pathTrace scene) rays
  where
    rays = map (createPrimaryRay camera) pixels
    pixels = pixelBuffer camera

pathTrace :: Scene -> Ray -> Vector
pathTrace scene ray = color
  where
    objects = geometry scene
    result = trace objects ray
    color = shade scene result
