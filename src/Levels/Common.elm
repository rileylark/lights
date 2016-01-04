module Levels.Common where

type alias Point = (Float, Float)
type alias Shape = List (Float, Float)

type alias LevelState =
    { lightPosition : Point
    , shapes : List Shape
    , border : Shape
    }