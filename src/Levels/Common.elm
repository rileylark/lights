module Levels.Common where

import Scene 

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
    , calculated : 
        { lightMaps : List Shape }
    }
    
type LevelAction =
    MoveLight Point
    
update : LevelAction -> LevelState -> LevelState
update action oldState =
    let 
        intermediateState = 
            case action of
                MoveLight newPosition ->
                    { oldState |
                        lightPosition = newPosition
                    }
    in
        { intermediateState |
            calculated = {
                lightMaps = calculateLightMaps intermediateState
            }
        }
            
calculateLightMaps levelState = 
    Scene.fuzzyLights levelState.lightPosition <| levelState.border :: levelState.shapes