module P5

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open Shared

module GlobalMode =
    [<Emit("fill($0)")>]
    let fill (_:int) : unit = jsNative
    [<Emit("rect($0,$1,$2,$3)")>]
    let rect (_:int) (_:int) (_:int) (_:int) : unit = jsNative
    [<Emit("frameRate($0)")>]
    let frameRate (_:int) : unit = jsNative
    [<Emit("createCanvas($0,$1)")>]
    let createCanvas (_:int) (_:int) : unit = jsNative

[<AllowNullLiteral>]
type Sketch =
    abstract member createCanvas: int -> int -> unit
    abstract member background: int -> unit
    abstract member fill: int -> unit
    abstract member rect: int -> int -> int -> int -> unit
    abstract member frameRate : int -> unit
    abstract member remove:unit->unit

[<Emit("new p5($0)")>]
let private p5(sk:Sketch -> unit):obj = jsNative

module P5Impl =
    let windowSk = PropertyWrap<Sketch option>((fun () -> Browser.window?sk), fun v -> Browser.window?sk<-v)
type SketchWrapper(setup:Sketch -> unit, draw:Sketch-> unit) =
    let mutable sk = None
    let mutable p5Instance = None
    let f sketch =
        sketch?draw<- fun () -> draw sketch
        sketch?setup<- fun () -> setup sketch
        sk <- Some sketch
        P5Impl.windowSk.Value <- Some sketch

    do
    // assuming we will only have 1 on the screen at a time for now
        SketchWrapper.CleanUp()
        Browser.window?sk <- sk
        p5Instance <- Some (p5 f)
    member __.Dispose() =
        sk
        |> Option.iter(fun sk -> sk.remove())

    static member CleanUp() =
        if not <| isNull Browser.window?sk then
            printfn "Cleaning up a sketch"
            let sk:Sketch = Browser.window?sk
            sk.remove()
            Browser.window?sk<- null


module Sample =

    let P5_0 () =
        let setup (sk:Sketch) =
            printfn "Setup is running"
            sk.createCanvas 500 500
            sk.frameRate 15

        let draw(sk:Sketch) =
            sk.fill 255
            sk.rect 10 10 100 200
        SketchWrapper(setup, draw)


