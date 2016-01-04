module Levels.Level01 where

import Levels.Common

import Scene
 
initialState : Levels.Common.LevelState
initialState = 
    { lightPosition = (150, 0)
    , shapes = Scene.randomTriangles -200 200
    , border =
        [ (-200, -200)
        , (200, -200)
        , (200, 200)
        , (-200, 200)
        ]
    }
