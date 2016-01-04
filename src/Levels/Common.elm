module Levels.Common where

import Scene 

type alias Point = (Float, Float)
type alias Shape = List (Float, Float)

type LitState = Lit Int -- lit by how many lights?
    
type alias Detector = 
    { position : Point
    , lit :
        { goal : LitState
        }
    }
    
type alias LevelState =
    { lightPosition : Point
    , shapes : List Shape
    , border : Shape
    , detectors : List Detector    
    }
    
type alias BakedLevelState = 
    (   LevelState  
    ,   {   lightMaps : List Shape 
        ,   detectors : List (Detector, LitState)
        }
    )
    
type LevelAction =
    MoveLight Point
    
update : LevelAction -> LevelState -> BakedLevelState
update action oldState =
    let 
        intermediateState = 
            case action of
                MoveLight newPosition ->
                    { oldState |
                        lightPosition = newPosition
                    }
    in
        (intermediateState, calculateLevel intermediateState)
        
calculateLevel levelState = 
    { lightMaps = calculateLightMaps levelState 
    , detectors = calculateDetectors levelState
    }
        
calculateDetectors levelState =
    []
        
calculateLightMaps levelState = 
    Scene.fuzzyLights levelState.lightPosition <| levelState.border :: levelState.shapes