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
open Fable.Import
open Fable.Import.JS

// allow for non-challenge indexes
type Model = {Index:string}

type Msg =
    |Navigate of index:string

module P5Impl =

    let init () : Model * Cmd<Msg> =
        {Index=null},Cmd.none

    let update (msg : Msg) (currentModel : Model) : Model * Cmd<Msg> =
        match msg with
        | Navigate ind ->
            {currentModel with Index = ind},Cmd.none
    let cleanUp() =
        printfn"Clean up called"
        SketchWrapper.CleanUp()
        Browser.window?index <- null
module Run =
    type CodingTrain = {Index:string; Link:string option; Initializer:(unit->SketchWrapper) option}
    let initP5 = lazy(
        let e = Browser.document.createElement("script")
        // s?type <- "text/javascript"
        e?src<-"https://cdnjs.cloudflare.com/ajax/libs/p5.js/0.7.3/p5.js"
        Browser.document.body.appendChild(e)
        |> ignore<Browser.Node>
    )
    let ct index init link = {Index = index;Initializer=init;Link=link}
    let models = [
        ct "001-starfield" (Some P5.P001_Starfield.p5_001) (Some "17WoOqgXsRM")
        ct "002-mengersponge" (Some P5.P002_MengerSponge.p5_002) (Some "LG8ZK-rRkXo")
        ct "136-perlinnoise" (Some P5.PerlinNoise.P5_136) (Some "ZI1dmHv3MeM")
    ]

    let view (model:Model) (dispatch:Msg->unit) =
        let hadValue = initP5.IsValueCreated
        // initP5.Force()
        let isRevisit = Browser.window?index = model.Index

        let hasModel,fSketchOpt =
            // hack attempt to dispose previous drawing

            match model.Index with
            | null | "home" ->
                Some P5.Sample.P5_0
            | i ->
                models

                |> List.tryFind(fun x -> x.Index = i)
                |> function
                    |Some x ->
                        x.Initializer
                    | None -> None
            |> function
                | None -> false,None
                | Some x -> true,Some x
        fSketchOpt
        |> Option.iter(fun fSk ->
            if hadValue then fSk() |> ignore<SketchWrapper>
            // lazy loading the script tag has problems
            else JS.setTimeout (fun () -> fSk () |> ignore<SketchWrapper>) 200 |> ignore
        )
        Browser.window?index<-model.Index
        let navLink name = a [ Href <| sprintf "#%s" name; OnClick (fun _ -> dispatch (Navigate name)) ][str name]
        let ytLink ident = a [ Href <| sprintf "https://www.youtube.com/watch?v=%s" ident; Target "_"] [ str "Watch"]
        div [] [
            Heading.h3 [] [str model.Index]
            Container.container [][
                Content.content [] [
                    div [] [ str <| (if hasModel then model.Index elif isNull model.Index then "home" else "No matching model found") ]
                    ol [] (
                        navLink "home" ::
                        (models
                            |> List.map(fun x ->
                                li [] [
                                    // if model.Index <> name then
                                        yield navLink x.Index
                                        match x.Link with
                                        | None -> ()
                                        | Some link ->
                                            yield str " - "
                                            yield ytLink link
                                    // else yield label [] [str name]
                                ]
                            )
                        )
                    )
                ]
            ]

        ]

