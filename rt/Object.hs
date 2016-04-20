module Object (Object(..)) where

import Material
import Geometry (Shape)

data Object = Object {
  shape :: Shape,
  material :: Material
} deriving (Read, Show)
