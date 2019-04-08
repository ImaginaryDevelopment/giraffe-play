module P5.Starfield

open System

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open P5
type Star(fSpeed, width,height,?seedOpt) =
    let random =
        match seedOpt with
        | None -> Random()
        | Some i -> Random(i)
    let floatNext min max = random.Next(min,max) |> float
    let mutable x = floatNext (width * -1) width
    let mutable y = floatNext (height * -1) height
    let mutable z = float width
    let mutable pz = z
    member __.update() =
        z <- z - (fSpeed())
        if z < 1.0 then
            z <- float width
            x <- floatNext (width * -1) width
            pz <- z
    member this.show(sk:ISketch) =
        sk.fill(255)
        sk.noStroke()
        let sx = sk.map(x / z, 0.0, 1.0, 0.0, float width)
        let sy = sk.map(y / z, 0.0, 1.0, 0.0, float width)
        let r = sk.map(float z, 0.0, float width, 16.0, 0.0)
        // sk.ellipse(sx,sy,r,r)
        let px = sk.map(x / pz, 0.0, 1.0, 0.0, float width)
        let py = sk.map(y / pz, 0.0, 1.0, 0.0, float width)
        pz <- z
        sk.stroke(255)
        sk.line(px,py,sx,sy)
        ()

let p5_001() =
    let w,h = 800,800
    let count = 100
    let stars = Array.zeroCreate 800

    let mutable speed = 15.0
    let setup (sk:ISketch) =
        sk.createCanvas w h
        stars |> Seq.iteri(fun i _ ->stars.[i] <- (Star((fun () -> speed),w,h))
        )
    let draw (sk:ISketch) =
        speed <- sk.map(sk.mouseX, 0.0, float w, 0.0, 20.0)
        sk.background(0)
        sk.translate(float w / 2.0, float h / 2.0)
        stars |> Seq.iter(fun x -> x.update();x.show sk)
        // star.update()
        ()

    SketchWrapper(setup,draw,None)