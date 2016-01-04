module Levels.Common where

type alias Point = (Float, Float)
type alias Shape = List (Float, Float)

type LitState = Lit Int -- lit by how many lights?
    
type alias Detector = 
    { position : Point
    , lit :
        { goal : LitState
        , current : LitState 
        }
    }
    
type alias LevelState =
    { lightPosition : Point
    , shapes : List Shape
    , border : Shape
    , detectors : List Detector
    }
    
type LevelAction =
    MoveLight Point
    
update : LevelAction -> LevelState -> LevelState
update action oldState =
    case action of
        MoveLight newPosition ->
            { oldState |
                lightPosition = newPosition
            }