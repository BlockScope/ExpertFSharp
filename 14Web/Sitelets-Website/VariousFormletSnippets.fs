namespace Website

open IntelliFactory.WebSharper

module FormletSnippets =
    open IntelliFactory.WebSharper.Formlet
    open IntelliFactory.WebSharper.Html

    [<JavaScript>]
    let RunInBlock title f formlet =
        let output = Div []
        formlet
        |> Formlet.Run (fun res ->
            let elem = f res
            output -< [ elem ] |> ignore)
        |> fun form ->
            Div [Attr.Style "float:left;margin-right:20px;width:300px;min-height:200px;"] -< [
 
                H5 [Text title]
                Div [form]
                output
            ]

    [<JavaScript>]
    let RunSnippet title formlet =
        formlet
        |> RunInBlock title (fun s ->
            Div [
                P ["You entered: " + s |> Text]
            ])

    [<JavaScript>]
    let Snippet1 = Controls.Input "initial value"

    [<JavaScript>]
    let Snippet1a =
        Formlet.Yield (fun name -> name)
        <*> (Controls.Input ""
             |> Validator.Is (fun s -> s.Length > 3) "Enter a valid name")

    [<JavaScript>]
    let Snippet1b =
        Formlet.Yield (fun name -> name)
        <*> (Controls.Input ""
             |> Validator.IsNotEmpty "Enter a valid name"
             |> Enhance.WithFormContainer)

    [<JavaScript>]
    let Snippet1c =
        Formlet.Yield (fun name -> name)
        <*> (Controls.Input ""
             |> Validator.IsNotEmpty "Enter a valid name"
             |> Enhance.WithFormContainer
             |> Enhance.WithSubmitAndResetButtons)

    [<JavaScript>]
    let Snippet1d =
        Formlet.Yield (fun name -> name)
        <*> (Controls.Input ""
             |> Validator.IsNotEmpty "Enter a valid name"
             |> Enhance.WithValidationIcon
             |> Enhance.WithErrorSummary "Errors"
             |> Enhance.WithSubmitAndResetButtons
             |> Enhance.WithFormContainer)

    [<JavaScript>]
    let Snippet1e =
        Formlet.Yield (fun name -> name)
        <*> (Controls.Input ""
             |> Validator.IsNotEmpty "Enter a valid name"
             |> Enhance.WithValidationIcon
             |> Enhance.WithTextLabel "Name"
             |> Enhance.WithSubmitAndResetButtons
             |> Enhance.WithFormContainer)

    [<JavaScript>]
    let Snippet1f =
        Formlet.Yield (fun name -> name)
        <*> (Controls.Input ""
             |> Validator.IsNotEmpty "Enter a valid name"
             |> Enhance.WithValidationIcon
             |> Enhance.WithLabelAndInfo "Name" "Enter your name"
             |> Enhance.WithSubmitAndResetButtons
             |> Enhance.WithFormContainer)

module Formlets =
    open FormletSnippets
    open IntelliFactory.WebSharper.Html

    type Snippet() = 
        inherit Web.Control()

        [<JavaScript>]
        override this.Body = RunSnippet "Snippet1" Snippet1 :> _

    type Snippets() = 
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
            // The later works, the former not.
            //IntelliFactory.Html.Tags.Div
            //IntelliFactory.WebSharper.Html.Default.Div
            Div [
                RunSnippet "Snippet1" Snippet1
                RunSnippet "Snippet1a" Snippet1a
                RunSnippet "Snippet1b" Snippet1b
                RunSnippet "Snippet1c" Snippet1c
                RunSnippet "Snippet1d" Snippet1d
                RunSnippet "Snippet1e" Snippet1e
                RunSnippet "Snippet1f" Snippet1f
            ] :> _

module VariousFormletSnippets =
    open IntelliFactory.Html
    open IntelliFactory.WebSharper.Sitelets
    
    type Action = | Home

    module Pages =
        let SnippetsPage =
            Content.PageContent <| fun ctx ->
                { Page.Default with
                    Title = Some "Formlet snippets"
                    Body =
                        [
                            H1 [Text "Snippets"]
                            Div [new Formlets.Snippets()]
                        ]
                }

    let EntireSite =
        Sitelet.Content "/home" Action.Home Pages.SnippetsPage

    type Website() =
        interface IWebsite<Action> with
            member this.Sitelet = EntireSite
            member this.Actions = []
