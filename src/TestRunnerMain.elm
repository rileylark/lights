module Main where

import Task

import Console exposing (..)
import ElmTest exposing (consoleRunner)

import Tests.GameMath

tests : IO ()
tests = 
    consoleRunner Tests.GameMath.gameMathSuite


port runner : Signal (Task.Task x ())
port runner = 
    Console.run tests