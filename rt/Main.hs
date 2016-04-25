module Main (main) where

import Camera
import Geometry
import Image
import Light
import RayTracer
import Renderer
import Scene
import Vector

main = readFile "conf/camera.conf" >>= \ cameraFile ->
       readFile "conf/scene.conf" >>= \ sceneFile ->
       readFile "conf/renderer.conf" >>= \ rendererFile ->
       let camera = read cameraFile :: Camera
           scene = read sceneFile :: Scene
           renderer = read rendererFile :: Renderer
           image = render renderer camera scene
       in write "image.bmp" (width camera) (height camera) $ concatMap toBytes image
