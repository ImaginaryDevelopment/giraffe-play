namespace Shared

type Counter = { Value : int }

type PingStatus = {Port:int;PortStatus:string}
type PropertyWrap<'t>(fGetter,fSetter) =
    member __.Value
        with get():'t = fGetter()
        and set (v:'t) = fSetter v

module StringPatterns =
    let (|ParseInt|_|) =
        function
        |null | "" -> None
        | x -> Some x
        >> Option.map System.Int32.TryParse
        >> function
            |Some (true,x) -> Some x
            | _ -> None

