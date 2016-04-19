module Main (main) where

import Camera
import Geometry
import Image
import RayTracer
import Scene
import Vector

main = writeFloating "image.bmp" w h $ concatMap (replicate 4) image
  where
    image = render camera scene
    rays = map (createRay camera) buffer
    buffer = pixelBuffer camera
    camera = Camera {
      eye = Vector3 (-5) 0 0,
      dir = Vector3 1 0 0,
      up = Vector3 0 0 1,
      right = Vector3 0 1 0,
      width = w,
      height = h,
      fov = 90.0
    }
    scene = Scene [Sphere (Vector3 0 0 0) 3]
    w = 800
    h = 600
