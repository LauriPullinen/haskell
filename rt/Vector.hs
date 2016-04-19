module Vector (Vector(..), len, dot, add, sub, mult, div, unit, toIntegrals) where

import Prelude hiding (div)

data Vector = Vector3 Double Double Double |
  Vector4 Double Double Double Double deriving (Read, Show)

len :: Vector -> Double
len (Vector3 x y z) = sqrt (x**2 + y**2 + z**2)

dot :: Vector -> Vector -> Double
dot (Vector3 x y z) (Vector3 a b c) = x*a + y*b + z*c

add :: Vector -> Vector -> Vector
add (Vector3 x y z) (Vector3 a b c) = Vector3 (x+a) (y+b) (z+c)

sub :: Vector -> Vector -> Vector
sub a b = add a (neg b)

mult :: Vector -> Double ->  Vector
mult (Vector3 x y z) m = Vector3 (x*m) (y*m) (z*m)

neg :: Vector -> Vector
neg = (`mult` (-1))

div :: Vector -> Double -> Vector
div v a = v `mult` (recip a)

unit :: Vector -> Vector
unit v = v `div` (len v)

toIntegrals :: (Integral a) => Vector -> [a]
toIntegrals (Vector3 x y z) = map round [x, y, z, 1]
toIntegrals (Vector4 x y z w) = map round [x, y, z, w]
