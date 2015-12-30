import Color exposing (..)
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
        
        rawTriangles = Scene.randomTriangles minX maxX
        triangles = List.map (polygon >> filled clearGrey) rawTriangles
        
        rawLines = Scene.spiderWeb (cursorX, cursorY) rawTriangles
        
        lineToForm (point1, point2) =
            traced (solid red) (segment point1 point2)
            
        lines = List.map lineToForm rawLines
        
        
        cursor = circle 10 |> filled red |> move (cursorX, cursorY)
    in
        collage windowWidth windowHeight (lines ++ triangles ++ [cursor] )

red : Color
red =
    rgb 255 0 0
    
clearGrey : Color
clearGrey =
  rgba 111 111 111 0.6
