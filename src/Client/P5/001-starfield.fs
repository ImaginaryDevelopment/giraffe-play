module P5.Starfield

open System

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open P5
type Star(width,height,?seedOpt) =
    let random =
        match seedOpt with
        | None -> Random()
        | Some i -> Random(i)
    let floatNext min max = random.Next(min,max) |> float
    let x = floatNext 0 width
    let y = floatNext 0 height
    let z = float width
    member this.update() = ()
    member this.show(sk:ISketch) =
        sk.fill(255)
        sk.noStroke()
        let sx = sk.map(x / z, 0.0, 1.0, 0.0, float width)
        let sy = sk.map(y / z, 0.0, 1.0, 0.0, float width)
        sk.ellipse(sx,sy,8.0,8.0)
        ()

let p5_001() =
    let w,h = 800,800
    let stars = ResizeArray()

    let mutable speed = 0.0
    let setup (sk:ISketch) =
        sk.createCanvas w h
        [0..100] |> List.iter(fun _ -> stars.Add (Star(w,h)))
        ()
    let draw (sk:ISketch) =
        sk.background(0)
        stars |> Seq.iter(fun x -> x.update();x.show sk)
        // star.update()
        ()

    SketchWrapper(setup,draw,None)