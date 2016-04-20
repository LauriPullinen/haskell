module Scene (Scene(..)) where

import Object
import Light

data Scene = Scene {
  geometry :: [Object],
  lights :: [Light]
} deriving (Read, Show)
