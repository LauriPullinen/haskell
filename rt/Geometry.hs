module Geometry (Shape(..), Ray(..), intersect, getNormal) where

import Vector

data Ray = Ray { origin::Vector, direction::Vector } deriving (Show)

data Shape = Sphere Vector Double deriving (Read, Show)

intersect :: Ray -> Shape -> Maybe Double
intersect (Ray rO rD) (Sphere sO sR)
    | b < 0     = Nothing
    | t < 0     = Nothing
    | otherwise = Just t
    where
      t = min (a + sqrt b) (a - sqrt b)
      a = - (rD `dot` deltaO)
      b = (rD `dot` deltaO)**2 - (len deltaO)**2 + sR**2
      deltaO = rO `sub` sO

getNormal :: Shape -> Vector -> Vector
getNormal (Sphere sO _) impact = unit (impact `sub` sO)
