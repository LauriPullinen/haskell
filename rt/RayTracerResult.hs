module RayTracerResult (Result(..), getImpact, minResult, getImpactNormal) where

import Geometry
import Math
import Object
import Vector

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

getImpact :: Result -> Vector
getImpact result = origin ray `add`
  (direction ray `mult` distance result) `sub`
  (direction ray `mult` epsilon)
    where
      ray = traced result

getImpactNormal :: Result -> Vector
getImpactNormal result = getNormal (shape $ object result) impact
  where
    impact = getImpact result
