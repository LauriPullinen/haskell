module Material (Material(..)) where

import Vector

data Material = Material {
  color :: Vector
} deriving (Read, Show)
