module Main (main) where

import RayTracer
import Vector
import Camera
import Geometry
import Scene

main = putStrLn $ show image
  where
    image = render camera scene
--    rays = map (createRay camera) buffer
--    buffer = pixelBuffer camera
    camera = Camera {
      eye = Vector3 (-5) 0 0,
      dir = Vector3 1 0 0,
      up = Vector3 0 0 1,
      right = Vector3 0 0 1,
      width = 10,
      height = 10
    }
    scene = Scene [Sphere (Vector3 0 0 0) 1]
