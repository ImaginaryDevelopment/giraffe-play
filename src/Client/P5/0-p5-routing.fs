namespace P5Routing
open Elmish
open Elmish.React

open Fable.Core.JsInterop
open Fable.Import
open Fable.Helpers.React
open Fable.Helpers.React.Props
open Fable.PowerPack.Fetch

open Fulma
open P5

// allow for non-challenge indexes
type Model = {Index:string;Sketch:SketchWrapper option}

type Msg =
    |Navigate of index:string
module P5Impl =

    let init () : Model * Cmd<Msg> =
        {Index=null;Sketch=None},Cmd.none

    let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
        match msg with
        | Navigate ind ->
            {currentModel with Index = ind},Cmd.none
    let cleanUp() =
        SketchWrapper.CleanUp()
module Run =
    let view model (dispatch:Msg->unit) =
        let hasModel =
            // hack attempt to dispose previous drawing
            if Browser.window?index <> model.Index then
                match model.Index with
                | null | "" | "0" ->
                    Some P5.Sample.P5_0
                | "136-perlinnoise" ->
                    None
                | _ -> None
                |> function
                    |Some x ->
                        printfn "Setting up setup and draw routines"
                        x()

                        // P5.p5(500,500,x.setup,x.draw)
                        // P5.p5 x
                        |> ignore
                        true
                    |None ->
                        // if not <| isNull Browser.window?p5Instance then
                        //     Browser.window


                        false
            else true
        printfn "HasModel? %A" hasModel
        div [] [
            Heading.h3 [] [str model.Index]
            Container.container [][
                Content.content [] [
                    div [] [ str <| if hasModel then null else "No matching model found" ]
                ]
            ]

        ]

