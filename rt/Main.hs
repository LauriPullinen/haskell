module Main (main) where

import Camera
import Geometry
import Image
import Light
import RayTracer
import Scene
import Vector

main = readFile "conf/camera.conf" >>= \ cameraFile ->
       readFile "conf/scene.conf" >>= \ sceneFile ->
       let camera = read cameraFile :: Camera
           scene = read sceneFile :: Scene
           image = render camera scene
           rays = map (createRay camera) buffer
           buffer = pixelBuffer camera
       in write "image.bmp" (width camera) (height camera) $ concatMap toIntegrals image
