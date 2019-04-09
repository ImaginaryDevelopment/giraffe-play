open System
open System.IO
open System.Threading.Tasks

open Microsoft.AspNetCore
open Microsoft.AspNetCore.Builder
open Microsoft.AspNetCore.Hosting
open Microsoft.Extensions.DependencyInjection

open FSharp.Control.Tasks.V2
open Giraffe


open Shared
open Shared.StringPatterns
open System.Net.Sockets


let tryGetEnv = System.Environment.GetEnvironmentVariable >> function null | "" -> None | x -> Some x

let publicPath = Path.GetFullPath "../Client/public"
let port = "SERVER_PORT" |> tryGetEnv |> Option.map uint16 |> Option.defaultValue 8065us

let getInitCounter () : Task<Counter> = task { return { Value = 42 } }
let getPing port : Task<PingStatus> = task {
    use tcp = new TcpClient()
    printfn "Checking port %i" port
    try
        tcp.Connect("127.0.0.1", port)
        return {Port=port;IsOpen=true;Error=null;When=DateTime.UtcNow}
    with ex ->
        return {Port=port;IsOpen=false;Error=ex.Message;When=DateTime.UtcNow}

}
let webApp =
    let printPart msg:HttpHandler= warbler (fun _ next ctx -> printfn "%s" msg; Task.FromResult None )
    let pingHandler:HttpHandler =
        fun next ctx ->
            printfn "checking for port"
            match ctx.GetQueryStringValue "port" with
            | Ok (ParseInt port) ->
                task{
                    let! pingResult = getPing port
                    return! Successful.OK pingResult next ctx
                }
            | Error ex ->
                ServerErrors.INTERNAL_ERROR ex next ctx
            | Ok x ->
                ServerErrors.INTERNAL_ERROR x next ctx
    choose [
        route "/api/hello" >=> text "hi"
        route "/api/init" >=>
            fun next ctx ->
                task {
                    let! counter = getInitCounter()
                    return! Successful.OK counter next ctx
                }
        // route "/hello" >=> printPart "world"
        route "/api/ping"
            >=> responseCaching (Public (TimeSpan.FromSeconds 2.0)) None (Some [| "port" |])
            // >=> warbler (fun (next,ctx) -> ServerErrors.INTERNAL_ERROR "warbled")
            >=> warbler (fun _ -> printfn "warbling!"; pingHandler)
            // warbler (fun (_,ctx) next -> pingHandler (next ctx) ctx))
        ServerErrors.NOT_IMPLEMENTED ()
    ]

let configureApp (app : IApplicationBuilder) =
    app.UseDefaultFiles()
       .UseStaticFiles()
       .UseGiraffe webApp

let configureServices (services : IServiceCollection) =
    services.AddGiraffe() |> ignore
    services.AddSingleton<Giraffe.Serialization.Json.IJsonSerializer>(Thoth.Json.Giraffe.ThothSerializer()) |> ignore

WebHost
    .CreateDefaultBuilder()
    .UseWebRoot(publicPath)
    .UseContentRoot(publicPath)
    .Configure(Action<IApplicationBuilder> configureApp)
    .ConfigureServices(configureServices)
    .UseUrls("http://0.0.0.0:" + port.ToString() + "/")
    .Build()
    .Run()