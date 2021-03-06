module Levels.View (scene) where

import Color exposing (rgb, rgba)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import State
import Signal
import Task

scene : State.Model -> Element
scene state =
    let
        (mouseX, mouseY) = state.mousePosition
        (windowWidth, windowHeight) = state.windowDimensions

        (level, calculated) = state.gameState
        
        triangles = List.map (polygon >> filled darkYellow) level.shapes
        
        lightPosition = level.lightPosition

        lightMaps =
            List.map (polygon >> gradient (lightGradient lightPosition)) calculated.lightMaps

        backdrop = polygon level.border |> filled darkYellow
        
        detectors = List.concat <| List.map detector calculated.detectors
    in
        collage windowWidth windowHeight ([backdrop] ++ triangles ++ lightMaps ++ detectors )


detector (d, lit) =
    let
        satisfiedColor = 
            if lit == d.litGoal then
                Color.green
            else
                Color.red
                
        detectorColor =
            if d.litGoal == 0 then
                Color.black 
            else
                Color.yellow 
                
        satisfied = circle 5 |> filled satisfiedColor |> move d.position
        detector = circle 15 |> filled detectorColor |> move d.position
    in
        [detector, satisfied]
    
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