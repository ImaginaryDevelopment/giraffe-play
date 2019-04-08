namespace P5

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
type ICanvas =
    abstract member remove:unit -> unit

[<AllowNullLiteral>]
type ISketchCore =
    abstract member TWO_PI:float
    abstract member CLOSE:string

    abstract member width: float with get,set
    abstract member height: float with get,set
    abstract member canvas: ICanvas

    abstract member cos:float -> float
    abstract member createCanvas: x:int -> width:int -> unit
    abstract member background: color:int -> unit
    abstract member draw:unit -> unit
    abstract member fill: color:int -> unit
    abstract member rect: int -> int -> int -> int -> unit
    abstract member frameRate : int -> unit
    abstract member remove:unit->unit
    abstract member translate: width:float -> height:float -> unit
    abstract member stroke: int -> unit
    abstract member beginShape: unit -> unit
    abstract member endShape: unit -> unit
    abstract member endShape: string -> unit
    abstract member map: n:float * start1:float * stop1:float * start2:float * stop2:float -> float
    abstract member map: n:float * start1:float * stop1:float * start2:float * stop2:float * withinBounds:bool -> float
    abstract member noFill: unit -> unit
    abstract member noLoop: unit -> unit
    abstract member noise: float -> float
    abstract member noise: x:float*y:float -> float
    abstract member noise: x:float*y:float*z:float -> float
    abstract member noiseSeed:int -> unit

    abstract member redraw: unit -> unit
    abstract member sin:float -> float
    abstract member vertex: x:float * y:float-> unit

[<AllowNullLiteral>]
type ISlider =
    abstract member value:unit -> float
[<AllowNullLiteral>]
type ISketchDom = // addOn
        abstract member createSlider: min:float*max:float*value:float*step:float -> ISlider


[<AllowNullLiteral>]
type ISketch =
    interface
        inherit ISketchCore
        inherit ISketchDom
    end

[<AllowNullLiteral>]
type IP5 =
    abstract member remove:unit -> unit
    abstract member draw:unit -> unit
    abstract member redraw:n:int -> unit
    abstract member redraw:unit -> unit

[<RequireQualifiedAccess>]
module P5Impl =
    [<Emit("new p5($0)")>]
    let p5(sk:ISketch -> unit):IP5= jsNative
    let fromDynamic<'t> (name:string):PropertyWrap<'t option> = PropertyWrap<_ option>((fun () -> Browser.window?(name)), fun v -> Browser.window?(name) <- v )
    let sk = PropertyWrap<ISketch option>((fun () -> Browser.window?sk), fun v -> Browser.window?sk<-v)
    let p5i = fromDynamic<IP5>("p5i")

// wrapper to help manage instance mode https://github.com/processing/p5.js/wiki/Global-and-instance-mode
type SketchWrapper(setup:ISketch -> unit, draw:ISketch-> unit, fOnce, ?target) =
    let mutable sk = None
    let mutable p5Instance = None
    let f sketch =
        sketch?draw <- fun () -> draw sketch
        sketch?setup <- fun () -> setup sketch
        fOnce
        |> Option.iter(fun f ->
            f sketch
        )
        sk <- Some sketch
        P5Impl.sk.Value <- Some sketch

    do
    // assuming we will only have 1 on the screen at a time for now
        SketchWrapper.CleanUp()
        let p5i' =
            match target with
            | None -> P5Impl.p5(f)
            | Some eId -> SketchWrapper.p5(f,Browser.document.getElementById eId) // "canvas"))
        p5Instance <- Some p5i'
        P5Impl.p5i.Value <- Some p5i'


    [<Emit("new p5($0,$1)")>]
    static member private p5(sk:ISketch -> unit,?element:Browser.Element):IP5= jsNative

    member __.Dispose() =
        sk
        |> Option.iter(fun sk -> sk.remove())
    static member Redraw() =
        match P5Impl.sk.Value with
        | None -> None
        | Some null -> eprintfn "Said some, but was null";None
        | Some sk ->
            printfn "Calling draw!"
            sk.redraw()
            Some ()

    static member CleanUp() =
        match P5Impl.sk.Value with
        | None -> ()
        | Some null -> eprintfn "Said some, but was null"
        | Some sk ->
            printfn "Cleaning up a sketch"
            sk.canvas.remove()
            sk.remove()
            P5Impl.sk.Value <- None


module Sample =

    let P5_0 () =
        let setup (sk:ISketch) =
            printfn "Setup is running"
            sk.createCanvas 600 600
            sk.frameRate 15

        let draw(sk:ISketch) =
            sk.background 0
            sk.translate (sk.width / 2.0) (sk.height / 2.0)
            sk.stroke 255
            sk.noFill()
            sk.beginShape()
            [0.0 .. 0.1 .. sk.TWO_PI]
            |> Seq.iter(fun a ->
                let r = float <| System.Random().Next(50,100)
                let x:float = r * System.Math.Cos(a)
                let y:float = r * System.Math.Sin(a)
                sk.vertex(x,y)
                ()
            )
            sk.endShape(sk.CLOSE)
            sk.noLoop()
        SketchWrapper(setup, draw,None)


