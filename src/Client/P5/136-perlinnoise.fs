module P5.PerlinNoise
open P5

let P5_136() =
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
    SketchWrapper(setup, draw)
