module Renderer (render) where

import Camera
import Geometry
import RayTracer
import Scene
import Shader
import Vector

render :: Camera -> Scene -> [Vector]
render cam scene = map (shade scene) results
    where
      results = map (trace (geometry scene)) rays
      rays = map (createRay cam) pixels
      pixels = pixelBuffer cam
