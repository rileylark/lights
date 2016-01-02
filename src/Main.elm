import Color exposing (rgb, rgba)
import Graphics.Collage exposing (..)
import Graphics.Element exposing (..)
import Mouse
import Window
import Time

import Scene


type alias GameInput =
    { mouse: (Int, Int)
    , window: (Int, Int)
    }

main : Signal Element
main =
    let
        constructInputs mousePosition windowDimensions =
            { mouse = mousePosition
            , window = windowDimensions
            }

        inputs = Signal.map2 constructInputs Mouse.position Window.dimensions

        frames = Signal.sampleOn (Time.fps 60) inputs
    in

        Signal.map scene frames

scene : GameInput -> Element
scene input =
    let
        (mouseX, mouseY) = input.mouse
        (windowWidth, windowHeight) = input.window

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
