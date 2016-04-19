module Light (Light(..)) where

import Vector

data Light = PointLight {
  point :: Vector,
  color :: Vector,
  power :: Double
} deriving (Read, Show)
