module State where

import Levels.Level01 as Level01
import Levels.Common

type Action = 
    MouseMoved (Int, Int)
    | WindowSized (Int, Int)
    
type alias Model =
    { windowDimensions : (Int, Int)
    , mousePosition : (Int, Int)
    , gameState : Levels.Common.LevelState
    }
    
initialState = 
    { windowDimensions = (600, 600)
    , mousePosition = (50, 200)
    , gameState = Level01.initialState
    }
    
update action state =
    case action of
        MouseMoved newPosition -> 
            { state |
                mousePosition = newPosition 
            }
                
        WindowSized newDimensions ->
            { state |
                windowDimensions = newDimensions 
            }
    