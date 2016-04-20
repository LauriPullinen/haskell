module Geometry (Shape(..), Ray(..), intersect, getNormal) where

import Math
import Vector

data Ray = Ray { origin::Vector, direction::Vector } deriving (Show)

data Shape = Sphere Vector Double |
  Triangle Vector Vector Vector deriving (Read, Show)

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
-- Möller-Trumbore Algorithm
intersect (Ray origin dir) (Triangle a b c)
  | abs det < epsilon = Nothing -- Ray and Plane parallel
  | u < 0 || u > 1    = Nothing
  | v < 0 || u+v > 1  = Nothing
  | t < epsilon       = Nothing
  | otherwise         = Just t
  where
    edgeAB = sub b a
    edgeAC = sub c a
    p = cross dir edgeAC
    det = dot edgeAB p
    invDet = 1 / det
    tvec = origin `sub` a
    u = dot tvec p * invDet -- Barycentric coordinate u
    qvec = cross tvec edgeAB
    v = (dot dir qvec) * invDet -- Barycentric coordinate v
    t = (dot edgeAC qvec) * invDet

getNormal :: Shape -> Vector -> Vector
getNormal (Sphere sO _) impact = unit (impact `sub` sO)
getNormal (Triangle a b c) _ = unit $ cross (b `sub` a) (c `sub` a)
