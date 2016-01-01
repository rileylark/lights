module GameMath where

type alias Point = (Float, Float)
type alias Vector = (Float, Float)
type alias Segment = (Point, Point)
type alias Ray = 
    { startAt : Point
    , goTowards : Point
    }
    

cross2d : Vector -> Vector -> Float
cross2d (x1, y1) (x2, y2) =
    x1*y2 - y1*x2
        
dot2d : Vector -> Vector -> Float
dot2d (x1, y1) (x2, y2) =
    x1 * x2 + y1 * y2
    
subtract : Point -> Point -> Vector
subtract (x1, y1) (x2, y2) =
    (x1 - x2, y1 - y2)
    
    
    
{-|  takes a list of points and connects 'em in order w/ segments
-}
makeSegments : List Point -> List Segment
makeSegments points =
    let
        maybeHead = List.head points
        maybeTail = List.tail points
    in
        case maybeTail of
            Just tail -> 
                case maybeHead of
                    Just head -> 
                        List.map2 (,) points (tail ++ [head])
                    Nothing -> [] -- this is impossible?
            Nothing -> []
            
findClosest : Point -> List Point -> Maybe Point
findClosest (x, y) targets =
    let 
        distances = List.map (\(tx, ty) -> 
            let distance = (tx-x)^2 + (ty-y)^2
            in (distance, (tx, ty)))
            targets
        
        maybeMinimum = List.minimum distances
        
    in 
        case maybeMinimum of
            Just (smallestDistance, closestPoint) -> Just closestPoint
            Nothing -> Nothing
            
castSpray : Point -> List Segment -> Point -> List Point
castSpray fromPoint againstSegments throughPoint =
    let
        (x, y) = throughPoint
        right = (x + 0.0001, y + 0.0001)
        left = (x - 0.0001, y - 0.0001)
        
        maybeRays = List.map (cast fromPoint againstSegments) [right, left]
        
        accumulateJusts maybe acc =
            case maybe of 
                Just val -> val :: acc
                Nothing -> acc
                
    in
        List.foldr accumulateJusts [] maybeRays
    
    
cast : Point -> List Segment -> Point -> Maybe Point
cast fromPoint againstSegments throughPoint  =
    let
        ray = { startAt = fromPoint, goTowards = throughPoint}
        maybeIntersections = List.map (intersect ray) againstSegments
        
        defaulted = List.map (Maybe.withDefault (100000, 100000)) maybeIntersections

    in
        findClosest fromPoint defaulted
        
intersect : Ray -> Segment -> Maybe Point
intersect ray segment = 
    let 
        rayDirection = subtract ray.goTowards ray.startAt
        
        (segmentA, segmentB) = segment
        segmentDirection = subtract segmentB segmentA
        
    in
        if cross2d rayDirection segmentDirection == 0 then
            Nothing -- they're parallel or collinear
        else  
            let
                (rayDx, rayDy) = rayDirection
            
                v1 = subtract ray.startAt segmentA
                v2 = subtract segmentB segmentA
                v3 = (-rayDy, rayDx)
                
                t = (cross2d v2 v1) / (dot2d v2 v3)
                u = (dot2d v1 v3) / (dot2d v2 v3)
                
            in
                if t >= 0 && 0 <= u && u <= 1 then
                    let (rayStartX, rayStartY) = ray.startAt
                    in Just (rayStartX + t * rayDx, rayStartY + t * rayDy)
                else 
                    Nothing -- their lines cross but the rays & segments don't

sortClockwise : Point -> List Point -> List Point
sortClockwise (originX, originY) pointsToSort =
    let
        angle (toX, toY) = -1 * atan2 (toY - originY) (toX - originX)
        addAngle point = { angle = angle point, point = point}
        pointsWithAngles = List.map addAngle pointsToSort
        sorted = List.sortBy .angle pointsWithAngles
        
    in
        List.map .point sorted
        