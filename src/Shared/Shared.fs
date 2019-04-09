namespace Shared
open System

type Counter = { Value : int }

type PingStatus = {Port:int;IsOpen:bool;Error:string;When:DateTime}
type PropertyWrap<'t>(fGetter,fSetter) =
    member __.Value
        with get():'t = fGetter()
        and set (v:'t) = fSetter v
module Option =
    let ofValueString =
        function
        | null | "" -> None
        | x when String.IsNullOrWhiteSpace x -> None
        | x -> Some x
module StringPatterns =
    let (|ValueString|_|) =
        Option.ofValueString
    let (|ParseInt|_|) =
        Option.ofValueString
        >> Option.map System.Int32.TryParse
        >> function
            |Some (true,x) -> Some x
            | _ -> None

