module Camera (Camera(..), Pixel, pixelBuffer, pixelAsPoint) where

import Vector

type Pixel = (Int, Int)

pixelBuffer :: Camera -> [Pixel]
pixelBuffer camera = [(x, y) | y <- [0..h], x <- [0..w] ]
  where
    h = height camera
    w = width camera

data Camera = Camera {
  eye :: Vector,
  dir :: Vector,
  up :: Vector,
  right :: Vector,
  width :: Int,
  height :: Int
}

pixelAsPoint :: Camera -> Pixel -> Vector
pixelAsPoint cam (i, j) = mult (right cam) nI `add`
  mult (up cam) nJ `add`
  eye cam `add`
  dir cam
    where
      nI = fromIntegral i / fromIntegral (width cam) - 0.5
      nJ = fromIntegral j / fromIntegral (height cam) - 0.5
