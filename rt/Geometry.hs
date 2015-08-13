module Geometry (Shape(..), Ray(..), intersect) where

import Vector

data Ray = Ray Vector Vector deriving (Show)

data Shape = Sphere Vector Double deriving (Show)

intersect :: Ray -> Shape -> Maybe Double
intersect (Ray rO rD) (Sphere sO sR)
    | b < 0     = Nothing
    | otherwise = Just $ min (a + sqrt b) (a - sqrt b)
    where
      a = - (rD `dot` deltaO)
      b = (rD `dot` deltaO)**2 - (len deltaO)**2 + sR**2
      deltaO = rO `sub` sO
