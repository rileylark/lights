module Levels.View (scene) where

import Color exposing (rgb, rgba)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import State
import Signal
import Task

import Scene

scene : State.Model -> Element
scene state =
    let
        (mouseX, mouseY) = state.mousePosition
        (windowWidth, windowHeight) = state.windowDimensions

        levelShapes = state.gameState.shapes
        levelBorder = state.gameState.border
        
        triangles = List.map (polygon >> filled darkYellow) levelShapes
        
        lightPosition = state.gameState.lightPosition

        rawLightMaps = Scene.fuzzyLights lightPosition <| levelBorder :: levelShapes
        lightMaps =
            List.map (polygon >> gradient (lightGradient lightPosition)) rawLightMaps

        backdrop = polygon levelBorder |> filled darkYellow
        
        detectors = List.map detector state.gameState.detectors
    in
        collage windowWidth windowHeight ([backdrop] ++ triangles ++ lightMaps ++ detectors )


detector d =
    circle 5 |> filled red |> move d.position
    
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