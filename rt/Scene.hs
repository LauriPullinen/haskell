module Scene (Scene(..)) where

import Geometry (Shape)

data Scene = Scene { geometry :: [Shape] } deriving (Show)
