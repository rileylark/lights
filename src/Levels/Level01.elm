module Levels.Level01 where

import Levels.Common

import Scene
 
initialState : Levels.Common.LevelState
initialState = 
    let
        detector (x, y) goal = { position = (x, y), litGoal = goal }
        lit position = detector position 1
        shaded position = detector position 0
    in
        { lightPosition = (150, 0)
        , shapes = Scene.randomTriangles -200 200
        , border =
            [ (-200, -200)
            , (200, -200)
            , (200, 200)
            , (-200, 200)
            ]
        , detectors = 
            [ lit (50, 50)
            , shaded (-50, -50)
            , lit (-50, 50)
            , shaded (100, -40)
            ]
        }
