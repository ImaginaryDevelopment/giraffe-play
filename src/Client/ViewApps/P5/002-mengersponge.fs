module P5.P002_MengerSponge

open System

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open P5

[<AllowNullLiteral>]
type Box(pv:IPVector,r:float)=

    member __.PVector = pv
    static member GenerateBoxes sk (b:Box) = b.Generate sk
    member __.Generate (sk:ISketch) =
        let boxes = ResizeArray()
        let range =[-1..1]
        for x in range do
            for y in range do
            for z in range do
            if abs x + abs y + abs z > 1 then
                let newR = r / 3.0
                let b = Box(sk.createVector (pv.x + float x * newR) (pv.y + float y *newR) (pv.z+ float z*newR), newR)
                boxes.Add b
            // else printfn "not generating box for (%i,%i,%i)" x y z
        boxes

    member __.Show (sk:ISketch) =
        sk.push()
        sk.translate1 pv.x pv.y pv.z
        sk.noStroke()
        sk.fill 255
        sk.box r
        sk.pop()
        ()


let p5_002() =
    let w,h = 600,600
    let mutable a = 0.0
    let mutable b:Box = null
    let mutable sponge = ResizeArray<Box>()
    let mutable clicks = 0
    let generateFirstBox :SketchDelegate =
        fun sk ->
            b <- Box(sk.createVector 0.0 0.0 0.0,200.0)

    let mousePressed sk () =
        printfn "We had %i box(es) moving at %f" sponge.Count a
        clicks <- clicks + 1
        if clicks % 4 > 0 then
            let next = ResizeArray<_>()
            sponge
            |> Seq.iter(Box.GenerateBoxes sk >> next.AddRange)
            sponge <- next
        else
            sponge.Clear()
            sponge.Add b
        printfn "We now have %i box(es)" sponge.Count



    let setup (sk:ISketch) =
        sk.createCanvas1 w h sk.WEBGL
        generateFirstBox sk
        sponge.Add b
        Sketch.setOnMousePressed true sk mousePressed
    let draw (sk:ISketch) =
        sk.background 51
        sk.stroke 255
        sk.noFill()
        sk.lights()

        // on my screen these higher divisors were necessary to get it anywhere near centered
        sk.translate(sk.width /20.0, sk.height /20.0)
        sk.rotateX(a)
        sk.rotateY(a * 0.4)
        sk.rotateZ(a * 0.1)
        sponge
        |> Seq.iter(fun x -> x.Show sk)
        a <- a + 0.01
        ()

    SketchWrapper(setup,draw,None)