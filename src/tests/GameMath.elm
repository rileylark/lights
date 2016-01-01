module Tests.GameMath where

import GameMath
import ElmTest exposing (..)


i = (1, 0)
j = (0, 1)


gameMathSuite = suite "GameMath Tests" 
    [ dot2d 
    , intersection 
    ]

dot2d = test "dot2d" 
    ( assertEqual 0 (GameMath.dot2d i j)
    )
    
intersection = 
    let        
        upAndRightRay = 
            { startAt = (0, 0)
            , goTowards = (5, 5)
            }
            
        downAndLeftRay = 
            { startAt = (0, 0)
            , goTowards = (-5, -5)
            }
            
        crossSegment = 
            ( (1, 4), (4, 1) )
            
        crazySegment = 
            ( (-1, 1), (-5, 5) )
    in
        suite "intersection" 
            [ test "do intersect" <| assertEqual (Just (2.5, 2.5)) (GameMath.intersect upAndRightRay crossSegment)
            , test "miss from wrong way" <| assertEqual Nothing (GameMath.intersect downAndLeftRay crossSegment)
            , test "total miss" <| assertEqual Nothing (GameMath.intersect upAndRightRay crazySegment)
            ]
    
