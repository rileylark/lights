module State where

import Levels.Level01 as Level01
import Levels.Common

type Action = 
    MouseMoved (Int, Int)
    | WindowSized (Int, Int)
    
type alias Model =
    { windowDimensions : (Int, Int)
    , mousePosition : (Int, Int)
    , gameState : Levels.Common.BakedLevelState
    }
    
initialState = 
    { windowDimensions = (600, 600)
    , mousePosition = (50, 200)
    , gameState = (Level01.initialState, Levels.Common.calculateLevel Level01.initialState)
    }
    
update action state =
    case action of
        MouseMoved newPosition -> 
            let
                cursorPosition = convertMousePosition state.windowDimensions newPosition
                (level, baked) = state.gameState
            in
            { state 
                | mousePosition = newPosition 
                , gameState = Levels.Common.update (Levels.Common.MoveLight cursorPosition) level
            }
                
        WindowSized newDimensions ->
            { state |
                windowDimensions = newDimensions 
            }
            
convertMousePosition : (Int, Int) ->  (Int, Int) -> (Float, Float)
convertMousePosition (windowWidth, windowHeight) (mouseX, mouseY) =
    let
        maxX = toFloat windowWidth / 2
        maxY = toFloat windowHeight / 2

        cursorX = toFloat mouseX - maxX
        cursorY = (toFloat mouseY - maxY) * -1 -- mouse y is upside down
        
    in
        (cursorX, cursorY)
    