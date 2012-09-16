namespace Website

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Html

[<AutoOpen>]
module HelloWorld =
    [<JavaScript>]
    let HelloWorld () =
        let welcome = P [Text "Welcome"]
        Div [
            welcome
            Input [Attr.Type "Button"; Attr.Value "Click me!"]
            |>! OnClick (fun e args ->
                welcome.Text <- "Hello, world!")
        ]

type MyControl() =
    inherit Web.Control()

    [<JavaScript>]
    override this.Body = HelloWorld () :> _