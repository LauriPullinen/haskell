module Renderer (Renderer(..), render) where

import Camera
import Geometry
import PathTracer
import RayTracer
import Scene
import Shader
import Vector

data Renderer = PathTracer { depth :: Int, gamma :: Double } deriving (Read, Show)

render :: Renderer -> Camera -> Scene -> [Vector]
render (PathTracer depth gamma) cam scene =
  map (gammaCorrect gamma) $ renderPathTracing cam scene depth

gammaCorrect :: Double -> Vector -> Vector
gammaCorrect gamma vec = pow vec (1 / gamma)
