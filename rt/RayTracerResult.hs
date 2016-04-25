module RayTracerResult (Result(..), minResult) where

import Geometry
import Object

data Result = Result { object :: Object
                     , traced :: Ray
                     , distance :: Double }

minResult :: Maybe Result -> Maybe Result -> Maybe Result
minResult Nothing Nothing = Nothing
minResult a Nothing = a
minResult Nothing b = b
minResult (Just a) (Just b)
  | distance a < distance b = Just a
  | otherwise = Just b
