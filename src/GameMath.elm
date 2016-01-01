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
