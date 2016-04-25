module Renderer (Renderer(..), render) where

import Camera
import Geometry
import PathTracer
import RayTracer
import Scene
import Shader
import Vector

data Renderer = PathTracer { depth :: Int } deriving (Read, Show)

render :: Renderer -> Camera -> Scene -> [Vector]
render (PathTracer depth) cam scene = renderPathTracing cam scene depth
