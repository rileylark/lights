module View (scene) where

import Color exposing (rgb, rgba)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import State
import Signal
import Task

import Scene

scene : Signal.Address State.Action -> Signal.Address (Task.Task () ()) -> State.Model -> Element
scene actionAddress taskAddress state =
    let
        (mouseX, mouseY) = state.mousePosition
        (windowWidth, windowHeight) = state.windowDimensions

        maxX = toFloat windowWidth / 2
        minX = -maxX

        maxY = toFloat windowHeight / 2
        minY = -maxY

        cursorX = toFloat mouseX - maxX
        cursorY = (toFloat mouseY - maxY) * -1 -- mouse y is upside down

        border =
            [ (minX, minY)
            , (maxX, minY)
            , (maxX, maxY)
            , (minX, maxY)
            ]

        rawTriangles = Scene.randomTriangles minX maxX
        triangles = List.map (polygon >> filled darkYellow) rawTriangles

        rawLightMaps = Scene.fuzzyLights (cursorX, cursorY) <| border :: rawTriangles
        lightMaps =
            List.map (polygon >> gradient (lightGradient (cursorX, cursorY))) rawLightMaps


        cursor = circle 10 |> filled red |> move (cursorX, cursorY)
        backdrop = polygon border |> filled darkYellow
    in
        collage windowWidth windowHeight ([backdrop] ++ triangles ++ lightMaps)


lightGradient : ( Float, Float ) -> Color.Gradient
lightGradient (centerX, centerY) =
    Color.radial (centerX, centerY) 10 (centerX, centerY) 350
    [ (  0, rgb  244 242 1)
    , (  1, rgba 228 199 0 0)
    ]

red : Color.Color
red =
    rgb 255 0 0

darkYellow : Color.Color
darkYellow = rgb 20 20 0

clearGrey : Color.Color
clearGrey =
  rgba 111 111 111 0.6

yellow : Color.Color
yellow = rgb 255 255 0