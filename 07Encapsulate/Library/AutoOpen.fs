module Acme.Components

[<AutoOpen>]
module private Utilities =
    let swap (x, y) = (y, x)

swap (3, 4) |> ignore
