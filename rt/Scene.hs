module Scene (Scene(..)) where

import Geometry (Shape)
import Light

data Scene = Scene { geometry :: [Shape], lights :: [Light] } deriving (Show)
