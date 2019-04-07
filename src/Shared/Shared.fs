namespace Shared

type Counter = { Value : int }

type PingStatus = {Port:int;PortStatus:string}

module StringPatterns =
    let (|ParseInt|_|) =
        function
        |null | "" -> None
        | x -> Some x
        >> Option.map System.Int32.TryParse
        >> function
            |Some (true,x) -> Some x
            | _ -> None

