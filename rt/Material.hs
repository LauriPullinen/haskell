module Material (Material(..), brdf) where

import Vector

data Material =
  Lambertian { color :: Vector } |
  BlinnPhong {
    diffuseColor :: Vector,
    specularColor :: Vector,
    specularHardiness :: Double
  } deriving (Read, Show)

brdf :: Material -> Vector -> Vector -> Vector -> Vector
brdf (Lambertian color) normal incident reflected =
  color `mult` dot incident normal
brdf (BlinnPhong diffuseColor specularColor hardness) normal incident reflected =
  (diffuseColor `mult` lambertian) `add` (specularColor `mult` specular)
    where
      lambertian = max (dot incident normal) 0
      specular = if lambertian > 0
        then (dot normal halfway) ** hardness
        else 0
      halfway = unit $ incident `add` reflected
