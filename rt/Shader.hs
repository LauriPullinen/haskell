module Shader (shade) where

import Camera
import Geometry
import Light
import Material
import Math
import Object
import RayTracer
import RayTracerResult
import Scene
import Vector

import Control.Applicative
import Data.Maybe
import Prelude hiding (div)

shade :: Scene -> Maybe Result -> Vector
shade _ Nothing = Vector3 0 0 0
shade scene (Just result) = if obstructed
  then Vector3 0 0 0
  else Light.color light `hadamardProd`
    brdf objectMaterial normal lightDir (neg $ direction ray) `mult`
    power light `div`
    (lightDistance ^ 2)
  where
    obstructed = fromMaybe False $ (< lightDistance) <$> obstructionDistance
    obstructionDistance = distance <$> lightResult
    lightResult = trace (geometry scene) lightRay -- Maybe Result
    lightRay = Ray impact lightDir
    lightDistance = len impactToLight
    lightDir = unit impactToLight
    impactToLight = point light `sub` impact
    normal = getImpactNormal result
    impact = getImpact result
    light = head $ lights scene -- TODO: Random light
    ray = traced result
    objectMaterial = material $ object result
