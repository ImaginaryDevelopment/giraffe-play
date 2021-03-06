module P5.PerlinNoise

open Fable.Core
open Fable.Core.JsInterop
open Fable.Import

open P5

// https://github.com/CodingTrain/website/blob/master/CodingChallenges/CC_136_Polar_Noise_Loop_1/Processing/CC_136_Polar_Noise_Loop_1/CC_136_Polar_Noise_Loop_1.pde
// this doesn't match the repo, but the repo doesn't include the other things he added
let P5_136() =
    let mutable slider = null
    let mutable phase = 0.0
    let mutable zoff = 0.0
    let setSeed:SketchDelegate =
        fun sk ->
            sk.noiseSeed(System.Random().Next())

    let setup (sk:ISketch) () =
        printfn "Setup is running"
        sk.createCanvas 600 600
        slider <- sk.createSlider(0.0,10.0,0.0,0.1)
        Browser.window?slider <- slider
        slider.value 5.0
        setSeed sk
        phase <- 0.0
        zoff <- 0.0
        sk.frameRate 15
        // slider <-

    let draw (sk:ISketch) () =
        let cos = sk.cos
        let sin = sk.sin
        sk.background 0
        sk.translate (sk.width / 2.0, sk.height / 2.0)
        sk.stroke 255
        sk.noFill()
        sk.beginShape()
        let noiseMax = slider.value()

        [0.0 .. 0.1 .. sk.TWO_PI]
        |> Seq.iter(fun a ->
            let xoff = sk.map(cos(a + phase),-1.0,1.0,0.0,noiseMax)
            let yoff = sk.map(sin(a + phase ),-1.0,1.0,0.0,noiseMax)
            let n = sk.noise (xoff,yoff,zoff)
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
        Sketch.setOnMousePressed true sk (fun sk () ->
            // he used redraw which doesn't appear to work, this does
            setSeed sk
        )
    SketchWrapper(setup, draw, Some mousePressed)
