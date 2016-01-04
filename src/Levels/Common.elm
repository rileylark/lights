module Levels.Common where

import Scene 
import GameMath

type alias Point = (Float, Float)
type alias Shape = List (Float, Float)

type alias LitState = Int -- lit by how many lights?
    
type alias Detector = 
    { position : Point
    , litGoal : Int
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
    let 
        obstacleSegments = List.concat <| List.map GameMath.makeSegments levelState.shapes
        
        calcDetector detector =
            let
                visible = GameMath.visible detector.position levelState.lightPosition obstacleSegments
                numVisible = if visible then 1 else 0
            in
                (detector, numVisible)
            
    in
        List.map calcDetector levelState.detectors
        
calculateLightMaps levelState = 
    Scene.fuzzyLights levelState.lightPosition <| levelState.border :: levelState.shapes