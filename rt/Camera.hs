module Camera (Camera(..), Pixel, pixelBuffer, pixelAsPoint) where

import Vector

type Pixel = (Int, Int)

pixelBuffer :: Camera -> [Pixel]
pixelBuffer camera = [(x, y) | y <- [0..h-1], x <- [0..w-1] ]
  where
    h = height camera
    w = width camera

data Camera = Camera {
  eye :: Vector,
  dir :: Vector,
  up :: Vector,
  right :: Vector,
  width :: Int,
  height :: Int,
  fov :: Double
} deriving (Show)

pixelAsPoint :: Camera -> Pixel -> Vector
pixelAsPoint cam (i, j) = mult (right cam) pixelX `add`
  mult (up cam) pixelY `add`
  eye cam `add`
  dir cam
    where
      pixelX = (2 * ndcX - 1) * t * aspectRatio
      pixelY = (2 * ndcY - 1) * t
      ndcX = (fromIntegral i + 0.5) / fromIntegral (width cam)
      ndcY = (fromIntegral j + 0.5) / fromIntegral (height cam)
      aspectRatio = fromIntegral (width cam) / fromIntegral (height cam)
      t = tan (fov cam / 2 * pi / 180)
    --  nI = fromIntegral i / fromIntegral (width cam) - 0.5
    --  nJ = fromIntegral j / fromIntegral (height cam) - 0.5
