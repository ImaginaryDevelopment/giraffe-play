module Client

open Elmish
open Elmish.React

open Fable.Core.JsInterop
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Thoth.Json

open Shared


open Fulma
open System.Text
module KeyCode =
    let enter = 13.
let onKeyDown keyCode action =
    OnKeyDown (fun (ev:Fable.Import.React.KeyboardEvent) ->
        if ev.keyCode = keyCode then
            ev.preventDefault()
            action ev
    )
// The model holds data that you want to keep track of while the application is running
// in this case, we are keeping track of a counter
// we mark it as optional, because initially it will not be available from the client
// the initial value will be requested from server
type ClientMode =
    |Regular
    |P5Mode

type Model = { ClientMode:ClientMode; Counter: Counter option; PortInput:int option; ValidationError:string option; PingStatus:PingStatus option ; P5Model: P5Routing.Model}

type CounterMsg =
    | Increment
    | Decrement
    | InitialCountLoaded of Result<Counter, exn>
// The Msg type defines what events/actions can occur while the application is running
// the state of the application changes *only* in reaction to these events
type PorterMsg =
    | PortChanged of string
    | PortCheck
    | PortCheckLoaded of Result<PingStatus,exn>
type Msg =
    |CounterMsg of CounterMsg
    |PorterMsg of PorterMsg
    |P5Msg of P5Routing.Msg
    |ChangeMode of ClientMode


let initialCounter = fetchAs<Counter> "/api/init" (Decode.Auto.generateDecoder())

// defines the initial state and initial command (= side-effect) of the application
let init () : Model * Cmd<Msg> =

    let loadCountCmd =
        Cmd.ofPromise
            initialCounter
            []
            (Ok >> InitialCountLoaded)
            (Error >> InitialCountLoaded)
    let p5Model,cmd = P5Routing.P5Impl.init()
    let initialModel =
        {ClientMode=Regular; Counter = None; PortInput = None; ValidationError = None; PingStatus=None;P5Model=p5Model}
    initialModel, Cmd.batch [(loadCountCmd |> Cmd.map CounterMsg) ; cmd |> Cmd.map P5Msg]

let listenerCheck port =
    fetchAs<PingStatus> (sprintf "/api/ping?port=%i" port) (Decode.Auto.generateDecoder())


// The update function computes the next state of the application based on the current state and the incoming events/messages
// It can also run side-effects (encoded as commands) like calling the server via Http.
// these commands in turn, can dispatch messages to which the update function will react.
let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
    let onIfP5ModeLeaving nextModel=
        match nextModel with
        | {ClientMode=P5Mode} -> ()
        | _ ->
            match currentModel with
            | {ClientMode=P5Mode} ->
                P5Routing.P5Impl.cleanUp()
        nextModel
    let nextModel,cmd =
        match msg with
        | ChangeMode mode ->
            {currentModel with ClientMode=mode}, Cmd.none
        | P5Msg msg ->
            let model,cmd = P5Routing.P5Impl.update msg currentModel.P5Model
            {currentModel with P5Model = model}, cmd |> Cmd.map P5Msg
        | CounterMsg cm ->
            match currentModel.Counter, cm with
            | Some counter, Increment ->
                let nextModel = { currentModel with Counter = Some { Value = counter.Value + 1 } }
                nextModel, Cmd.none
            | Some counter, Decrement ->
                let nextModel = { currentModel with Counter = Some { Value = counter.Value - 1 } }
                nextModel, Cmd.none
            | _, InitialCountLoaded (Ok initialCount) ->
                let nextModel = {currentModel with Counter = Some initialCount; PortInput=None; ValidationError=None;PingStatus= None}
                nextModel, Cmd.none
        | PorterMsg (PortChanged raw) ->
            match System.Int32.TryParse(raw) with
            | true, x ->
                {currentModel with PortInput = Some x;ValidationError=None}, Cmd.none
            | false, _ -> {currentModel with ValidationError= Some "Failed to read port"}, Cmd.none
        | PorterMsg PortCheck ->
            match currentModel.PortInput with
            | None ->
                {currentModel with ValidationError=Some "No valid port to check"}, Cmd.none
            | Some port ->
                let cmd = Cmd.ofPromise (listenerCheck port) [] (Ok >> PortCheckLoaded) (Error >> PortCheckLoaded)
                {currentModel with ValidationError = None}, cmd |> Cmd.map PorterMsg
        | PorterMsg (PortCheckLoaded (Ok ps)) ->
            {currentModel with PingStatus = Some ps}, Cmd.none
        | PorterMsg (PortCheckLoaded (Error ex)) ->
            {currentModel with ValidationError = Some <| sprintf "%A" ex}, Cmd.none
    let nextModel =
        (nextModel,[ onIfP5ModeLeaving])
        ||> List.fold(fun nextModel f ->
            f nextModel
        )
    nextModel, cmd



let safeComponents =
    let components =
        span [ ]
           [
             a [ Href "https://github.com/giraffe-fsharp/Giraffe" ] [ str "Giraffe" ]
             str ", "
             a [ Href "http://fable.io" ] [ str "Fable" ]
             str ", "
             a [ Href "https://elmish.github.io/elmish/" ] [ str "Elmish" ]
             str ", "
             a [ Href "https://fulma.github.io/Fulma" ] [ str "Fulma" ]
           ]

    p [ ]
        [ strong [] [ str "SAFE Template" ]
          str " powered by: "
          components ]

let show = function
| { Counter = Some counter } -> string counter.Value
| { Counter = None   } -> "Loading..."

let button txt onClick =
    Button.button
        [ Button.IsFullWidth
          Button.Color IsPrimary
          Button.OnClick onClick ]
        [ str txt ]
let regularMode model dispatch =
  Container.container []
      [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
            [ Heading.h3 [] [ str ("Press buttons to manipulate counter: " + show model) ] ]
        Columns.columns []
            [ Column.column [] [ button "-" (fun _ -> dispatch (CounterMsg Decrement)) ]
              Column.column [] [ button "+" (fun _ -> dispatch (CounterMsg Increment)) ] ]
        Label.label [] [str "Enter port to check"]
        Input.number [Input.Placeholder "Ex: 8080"
                      Input.Value (string model.PortInput)
                    //   Input.Modifiers [ Modifier.TextTransform TextTransform.UpperCase ]
                      Input.Color (if model.ValidationError.IsSome then Color.IsDanger else Color.IsSuccess)
                      Input.Props [ OnChange (fun ev -> dispatch (Msg.PorterMsg(PortChanged !!ev.target?value))); onKeyDown KeyCode.enter (fun _ -> dispatch (PorterMsg PortCheck)) ] ]
        Label.label [] [ model.ValidationError |> Option.defaultValue null |> str ]
        Label.label [] [ model.PingStatus |> Option.map (fun ps -> sprintf "Port %i - %s" ps.Port ps.PortStatus) |> Option.defaultValue "" |> str]
      ]
let view (model : Model) (dispatch : Msg -> unit) =
    div []
        [ Navbar.navbar [ Navbar.Color IsPrimary ]
            [ Navbar.Item.div [ ]
                [ Heading.h2 [] [ str "SAFE Template" ] ]
              Navbar.Item.div [][
                  div [] [ button "Home" (fun _ -> dispatch <| ChangeMode ClientMode.Regular)]
                  div [] [ button "P5" (fun _ -> dispatch <| ChangeMode ClientMode.P5Mode )]

              ]
            ]
          (match model.ClientMode with
            | Regular -> regularMode model dispatch
            | P5Mode ->
                printfn "Running in P5 mode"
                P5Routing.Run.view model.P5Model (P5Msg >> dispatch)
          )
          Footer.footer [ ]
                [ Content.content [ Content.Modifiers [ Modifier.TextAlignment (Screen.All, TextAlignment.Centered) ] ]
                    [ safeComponents ] ]
        ]

#if DEBUG
open Elmish.Debug
open Elmish.HMR
#endif

Program.mkProgram init update view
#if DEBUG
|> Program.withConsoleTrace
|> Program.withHMR
#endif
|> Program.withReact "elmish-app"
#if DEBUG
|> Program.withDebugger
#endif
|> Program.run
