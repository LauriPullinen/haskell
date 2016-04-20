module Vector (Vector(..), len, dot, add, sub, mult, div, unit, toBytes, hadamardProd) where

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

toBytes :: (Integral a) => Vector -> [a]
toBytes (Vector3 x y z) = map toByte [x, y, z, 1]
toBytes (Vector4 x y z w) = map toByte [x, y, z, w]

toByte :: (Integral a) => Double -> a
toByte x
  | x >= 1.0  = 255
  | x <= 0.0  = 0
  | otherwise = round (x * 256)

-- component-wise product of vectors
hadamardProd :: Vector -> Vector -> Vector
hadamardProd (Vector3 x y z) (Vector3 a b c) = Vector3 (x*a) (y*b) (z*c)
