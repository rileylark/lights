import Color exposing (..)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)

import Scene

triangles = List.map (polygon >> filled clearGrey) (Scene.randomTriangles -150 150)

main : Element
main =
  collage 300 300 triangles

clearGrey : Color
clearGrey =
  rgba 111 111 111 0.6
