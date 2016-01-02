module CycleApp
    ( create
    , App
    , Config
    ) where

{-| This module is modeled after StartApp and the usual Elm Architecture,
but has a different take on Effects and how you should define things that 
your users can do.
There's a complete example you can use to get started below, but here's an 
excerpt that highlights the biggest difference between CycleApp and StartApp.
    addTwo notifyApp =
        notifyApp AddOne
        `Task.andThen` \_ -> (notifyApp AddOne)
        
Here `addTwo` is a function that describes dispatching _two_ notifications 
(actions), which is much harder to describe in StartApp. We also get full access
to the Task api here, which would let us describe multiple async API calls 
chained together in a single place. We think this is clearer than the 
corresponding StartApp way of describing multi-step actions.
Here's the full example
    module DriverMain where
    import Html exposing (..)
    import Task exposing (Task)
    import Html.Events
    import CycleApp
    -- Wiring the app together the CycleApp way
    app : CycleApp.App Html.Html
    app = CycleApp.create (
        { initialState = 0
        , update = update
        , view = view 
        })
     
    main :  Signal Html.Html
    main =  app.output
    port taskRunner : Signal (Task () ())
    port taskRunner = app.tasksToRun
    -- Model and Notifications (which change the current state)
    type alias Model = Int
    type Notification = 
        AddOne 
        | SubtractOne
    update : Notification -> Model -> Model
    update notification state =
        case notification of
            AddOne -> 
                state + 1     -- update only returns the new state
            SubtractOne -> 
                state - 1
    -- a simple view with a corresponding InstructionFactory
    view : Signal.Address (CycleApp.InstructionFactory Notification) -> Model -> Html.Html
    view instructionFactoryAddress state =
        div [ Html.Events.onClick instructionFactoryAddress addTwo]
            [ text ("Current count: " ++ toString state) 
            ]
            
    addTwo : CycleApp.InstructionFactory Notification
    addTwo notifyApp =
        notifyApp AddOne
        `Task.andThen` \_ -> (notifyApp AddOne)
        
In StartApp, your update function returns both a state and a set of Effects.
The state becomes your app's new state, and the set of Effects are run through
a port. This is super-simple in simple situations, but gets pretty gnarly when
you want to make multi-step Effects, or compose Effects.
So we made CycleApp, which follows some ideas from cycle.js . We split up the 
concerns of calculating state and calculating the next tasks that should be run.
With CycleApp, your update function ONLY calculates state, and we provide
a new API for defining tasks that makes it easier to write out a whole workflow
of asynchronous steps like API calls.
# Wire up your App
@docs create, Config, App
# Some types to help explain
@docs Instruction, InstructionFactory
-}

import Signal
import Task exposing (Task)


{-| You need a Config to make a CycleApp. We need you to specify:
* initialState
* update, a function that can update your initialState to the next state
* view, a function that can render a view based on your model.
Notes about `view`:
* Your output type is probably Html.Html
* We pass you an Address where you can send new InstructionFactories from your 
view, so that your view can not only describe what it looks like but also 
give Instructions for what should happen when a user clicks a button or types
in an input, etc.
-}
type alias Config notification model output = 
    { initialState : model
    , update : notification -> model -> model
    , view 
        :  Signal.Address notification 
        -> Signal.Address (Task () ()) 
        -> model -> output
    }
    
{-| CycleApp.create returns an App. You should render the output and run the 
tasks we send along tasksToRun.
Note: your output is probably Html.Html, and if you are making a normal web app
with elm, you should probably set `main = app.output` in your main module.
-}
type alias App notification output=
    { output : Signal output -- to render
    , tasksToRun : Signal (Task () ()) -- to hook up to a port
    , notificationAddress : Signal.Address notification -- in case you want to send your own actions in
    }
    
{-| CycleApp.create returns a CycleApp.App. You should render the output and run the 
tasks we send along tasksToRun.
Just like with `StartApp`, it should pretty much always look like this:
    app =
        CycleApp.create (
            { initialState = initialState
            , view = view
            , update = update 
            })
        
    main =
        app.output
        
    port tasks : Signal (Task () ())
    port tasks =
        app.tasksToRun
-}
create : Config notification model output -> App notification output
create config =
    let     
        spawn : Task () () -> Task () ()
        spawn task =
            (Task.spawn task)
            `Task.andThen` (\threadId -> Task.succeed ())
            |> Task.mapError (\_ -> ())
            
        instructionMailbox : Signal.Mailbox (Task () ())
        instructionMailbox =
            Signal.mailbox <| spawn <| Task.succeed ()
         
        actionsMailbox : Signal.Mailbox (Maybe notification)
        actionsMailbox = Signal.mailbox Maybe.Nothing
        
        notify : notification -> Task () ()
        notify notification =
            Signal.send actionsMailbox.address (Just notification) 
        
        taskAddress =
            Signal.forwardTo instructionMailbox.address spawn
             
        -- update : (Maybe notification) -> model -> model
        update maybeAction model = 
         case maybeAction of
             Just notification -> 
                 config.update notification model
             Nothing -> model

        -- model : Signal model
        model = Signal.foldp update config.initialState actionsMailbox.signal
             
        notificationAddress = 
            Signal.forwardTo actionsMailbox.address Just
    in
        { output = Signal.map (config.view notificationAddress taskAddress) model    
        , tasksToRun = instructionMailbox.signal 
        , notificationAddress = notificationAddress
        }