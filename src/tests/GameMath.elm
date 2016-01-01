module Tests.GameMath where

import GameMath
import ElmTest


i = (1, 0)
j = (0, 1)


suite = ElmTest.suite "GameMath Tests" 
    [ dot2d 
    , intersection 
    ]

dot2d = ElmTest.test "dot2d" 
    ( ElmTest.assertEqual 0 (GameMath.dot2d i j)
    )
    
intersection = 
    let        
        upAndRightRay = 
            { startAt = (0, 0)
            , goTowards = (5, 5)
            }
            
        crossSegment = 
            ( (1, 4), (4, 1) )
            
        crazySegment = 
            ( (-1, 1), (-5, 5) )
    in
        ElmTest.suite "intersection" 
            [ ElmTest.assertEqual (Just (2.5, 2.5)) (GameMath.intersect upAndRightRay crossSegment)
            , ElmTest.assertEqual Nothing (GameMath.intersect upAndRightRay crazySegment)
            ]
    
