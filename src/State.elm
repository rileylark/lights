module State where

type Action = 
    MouseMoved (Int, Int)
    | WindowSized (Int, Int)
    
type alias Model =
    { windowDimensions : (Int, Int)
    , mousePosition : (Int, Int)
    }
    
initialState = 
    { windowDimensions = (600, 600)
    , mousePosition = (50, 200)
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
    