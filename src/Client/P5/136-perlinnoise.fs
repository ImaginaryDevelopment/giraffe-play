module P5.PerlinNoise

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open P5

let P5_136() =
    let mutable slider = null
    let mutable phase = 0.0
    let setup (sk:ISketch) =
        printfn "Setup is running"
        sk.createCanvas 600 600
        slider <- sk.createSlider(0.0,10.0,0.0,0.1)
        sk.noiseSeed(System.Random().Next())
        // sk.frameRate 15
        // slider <-

    let draw(sk:ISketch) =
        let cos = sk.cos
        let sin = sk.sin
        sk.background 0
        sk.translate (sk.width / 2.0) (sk.height / 2.0)
        sk.stroke 255
        sk.noFill()
        sk.beginShape()
        let noiseMax = slider.value()

        [0.0 .. 0.1 .. sk.TWO_PI]
        |> Seq.iter(fun a ->
            let xoff = sk.map(cos(a + phase),-1.0,1.0,0.0,noiseMax)
            let yoff = sk.map(sin(a + phase ),-1.0,1.0,0.0,noiseMax)
            let n = sk.noise (xoff,yoff)
            let r = sk.map(n,0.0,1.0,100.0,200.0)
            let x:float = r * cos a
            let y:float = r * sin a
            sk.vertex(x,y)
            ()
        )
        // sk.CLOSE makes sure there is a line to close the figure
        sk.endShape(sk.CLOSE)
        phase <- phase + 0.1
        // sk.noLoop()
    let mousePressed (sk:ISketch) =
        printfn "Mouse pressed"
        sk?mousePressed <- fun () -> sk.redraw()
    SketchWrapper(setup, draw, Some mousePressed)
