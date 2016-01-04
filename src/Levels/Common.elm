module Levels.Common where

type alias Point = (Float, Float)
type alias Shape = List (Float, Float)

type alias LevelState =
    { lightPosition : Point
    , shapes : List Shape
    , border : Shape
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