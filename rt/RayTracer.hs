module RayTracer (trace) where

import Camera
import Geometry
import Scene (Scene)
import Vector

render :: [Pixel] -> Camera -> Scene -> [Maybe Double]
render [] _ _ = []
render (p:px) cam scene = trace (createRay cam p) scene : (render px cam scene)

trace :: Ray -> Scene -> Maybe Double
trace ray []     = Nothing
trace ray (x:xs) = min (intersect ray x) (trace ray xs)

createRay :: Camera -> Pixel -> Ray
createRay cam p = Ray (eye cam) (pixelAsPoint cam p `sub` eye cam)
