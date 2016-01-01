module Scene where

import Random 
import GameMath

initialSeed = Random.initialSeed 124

randomPoint lower upper = Random.pair (Random.float lower upper) (Random.float lower upper)

makePoint seed lower upper = 
    Random.generate (randomPoint lower upper) seed

randomShape seed lower upper= 
    let
        addPoint : Int -> (List (Float, Float), Random.Seed) -> (List (Float, Float), Random.Seed)
        addPoint _ (currentList, currentSeed) =
            let (nextPoint, nextSeed) = makePoint currentSeed lower upper
            in (nextPoint :: currentList, nextSeed)
        
    in
        List.foldr addPoint ([], seed) [1..3]

randomTriangles lower upper = 
    let
        range = upper - lower
        numTriangles = 5
        
        addShape index (currentList, currentSeed) =
            let 
                min = lower + range * ((index - 1) / numTriangles)
                max = lower + range * (index / numTriangles)
                
                (nextShape, nextSeed) = randomShape currentSeed min max
            in (nextShape :: currentList, nextSeed)
    
        (shapes, nextSeed) = List.foldr addShape ([], initialSeed) [1..5]
        
    in
        shapes
        
spiderWeb : 
    (Float, Float) 
    -> List (List (Float, Float)) 
    -> List (Float, Float)
    
spiderWeb centerPoint listOfShapes =
    let
        borders : List ((Float, Float), (Float, Float))
        borders = 
            [ ((-2000, 2000), (2000, 2000))
            , ((2000, 2000), (2000, -2000))
            , ((2000, -2000), (-2000, -2000))
            , ((-2000, -2000), (-2000, 2000))
            ]
            
        collisionSegments = List.append borders <| List.concat <| List.map GameMath.makeSegments listOfShapes
            
        getIntersectionsForShape : 
            List (Float, Float) 
            -> List (Float, Float)
        getIntersectionsForShape shape  =
            List.concat <| List.map (GameMath.castSpray centerPoint collisionSegments) shape
            
        intersectionLists : List (List (Float, Float))
        intersectionLists = List.map getIntersectionsForShape listOfShapes
    in
        List.concat intersectionLists