module GameMath where

type alias Point = (Float, Float)
type alias Vector = (Float, Float)
type alias Segment = (Point, Point)
type alias Ray = 
    { startAt : Point
    , goTowards : Point
    }
    

cross2d : Vector -> Vector -> Float
cross2d vec1 vec2 =
    let
        (x1, y1) = vec1
        (x2, y2) = vec2
    in
        x1*y2 - y1*x2
        
dot2d : Vector -> Vector -> Float
dot2d (x1, y1) (x2, y2) =
    x1 * x2 + y1 * y2
    
subtract : Point -> Point -> Vector
subtract p1 p2 =
    let
        (x1, y1) = p1
        (x2, y2) = p2
    in
        (x1 - x2, y1 - y2)

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
                t = (cross2d (subtract segmentA ray.startAt) rayDirection) / (cross2d rayDirection segmentDirection)
                u = (cross2d (subtract segmentA ray.startAt) rayDirection) / (cross2d segmentDirection rayDirection)
                (rayDirectionX, rayDirectionY) = rayDirection
                (rayStartX, rayStartY) = ray.startAt
            in
                Just (rayStartX + t * rayDirectionX, rayStartY + t * rayDirectionY)
