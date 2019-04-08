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
    abstract member createCanvas: x:int -> width:int -> unit
    abstract member background: color:int -> unit
    abstract member fill: color:int -> unit
    abstract member rect: int -> int -> int -> int -> unit
    abstract member frameRate : int -> unit
    abstract member remove:unit->unit
    abstract member translate: width:float -> height:float -> unit
    abstract member stroke: int -> unit
    abstract member noFill: unit -> unit
    abstract member beginShape: unit -> unit
    abstract member vertex: x:float * y:float-> unit
    abstract member endShape: unit -> unit
    abstract member width: float with get,set
    abstract member height: float with get,set
    abstract member TWO_PI:float

[<Emit("new p5($0)")>]
let private p5(sk:Sketch -> unit):obj = jsNative

module P5Impl =
    let windowSk = PropertyWrap<Sketch option>((fun () -> Browser.window?sk), fun v -> Browser.window?sk<-v)
// wrapper to help manage instance mode https://github.com/processing/p5.js/wiki/Global-and-instance-mode
type SketchWrapper(setup:Sketch -> unit, draw:Sketch-> unit, ?target) =
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
    [<Emit("new p5($0,$1)")>]
    static member private p5(sk:Sketch -> unit,?element:Browser.Element):obj = jsNative

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
            sk.createCanvas 600 600
            sk.frameRate 15

        let draw(sk:Sketch) =
            sk.background 0
            sk.translate (sk.width / 2.0) (sk.height / 2.0)
            sk.stroke 255
            sk.noFill()
            sk.beginShape()
            [0.0 .. 0.01 .. sk.TWO_PI]
            |> Seq.iter(fun a ->
                let r = 100.0
                let x:float = r * System.Math.Cos(a)
                let y:float = r * System.Math.Sin(a)
                sk.vertex(x,y)
                ()
            )
            sk.endShape()
        SketchWrapper(setup, draw)


