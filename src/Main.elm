import View
import Mouse
import Window
import Time
import Task
import Signal
import Graphics.Element

import CycleApp
import State

    
appConfig : CycleApp.Config State.Action State.Model Graphics.Element.Element
appConfig = 
    { initialState = State.initialState
    , update = State.update
    , view = View.scene
    }

app = CycleApp.create appConfig

main : Signal Graphics.Element.Element
main = app.output

mouseMoves = Signal.map State.MouseMoved Mouse.position
windowSizes = Signal.map State.WindowSized Window.dimensions

actionDispatcher = Signal.map (\action -> Signal.send app.notificationAddress action)

port tasks : Signal (Task.Task () ())
port tasks = Signal.mergeMany 
    [ actionDispatcher windowSizes
    , actionDispatcher mouseMoves
    , app.tasksToRun
    ]


