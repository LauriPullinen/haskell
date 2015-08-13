module Camera (Camera(eye), Pixel, pixelAsPoint) where

import Vector

type Pixel = (Int, Int)

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
