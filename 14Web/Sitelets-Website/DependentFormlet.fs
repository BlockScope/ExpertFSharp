namespace Website

open IntelliFactory.WebSharper

module DependentFormletSnippets =
    open IntelliFactory.WebSharper.Formlet
    open IntelliFactory.WebSharper.Html

    [<JavaScript>]
    let input (label : string) (err : string) = 
        Controls.Input ""
        |> Validator.IsNotEmpty err
        |> Enhance.WithValidationIcon
        |> Enhance.WithTextLabel label

    [<JavaScript>]
    let inputInt (label : string) (err : string) = 
        Controls.Input ""
        |> Validator.IsInt err
        |> Enhance.WithValidationIcon
        |> Enhance.WithTextLabel label

    [<JavaScript>]
    let Snippet2 : Formlet<string * int> =
        Formlet.Yield (fun name age -> name, age |> int)
        <*> input "Name" "Please enter your name"
        <*> inputInt "Age" "Please enter a valid age"
        |> Enhance.WithSubmitAndResetButtons
        |> Enhance.WithFormContainer

    [<JavaScript>]
    let Snippet3a =
        Formlet.Yield (fun name age -> name, age |> int)
        <*> input "Name" "Please enter your name"
        <*> inputInt "Age" "Please enter a valid age"
        |> Enhance.WithLegend "Person"
        |> Enhance.WithTextLabel "Person"
        |> Enhance.Many
        |> Enhance.WithLegend "People"
        |> Enhance.WithSubmitAndResetButtons
        |> Enhance.WithFormContainer

    [<JavaScript>]
    let Snippet4 =
        Formlet.Do {
            let! name = input "Name" "Please enter your name"
            let! age = inputInt "Age" "Please enter a valid age"
            return name, age |> int
        }
        |> Enhance.WithSubmitAndResetButtons
        |> Enhance.WithFormContainer

    [<JavaScript>]
    let Snippet4b =
        Formlet.Do {
            let! name = input "Name" "Please enter your name"
                        |> Enhance.WithSubmitAndResetButtons
                        |> Enhance.WithFormContainer
            let! age =  inputInt "Age" "Please enter a valid age"
                        |> Enhance.WithSubmitAndResetButtons
                        |> Enhance.WithFormContainer
            return name, age |> int
        }
        |> Formlet.Flowlet

    [<JavaScript>]
    let RunSnippet title (formlet : Formlet<string*int>) =
        formlet
        |> FormletSnippets.RunInBlock title (fun (s, i) ->
            Div [
                P ["You entered: " + s + " " + i.ToString() |> Text]
            ])

    [<JavaScript>]
    let RunSnippetList title (formlet : Formlet<(string*int) list>) =
        let showOne (x : string*int) =
            match x with
            | (s, i) ->
                Div [
                    P ["You entered: " + s + " " + i.ToString() |> Text]
                ]

        let rec ShowMany xs : Element list =
            match xs with
            | [] -> []
            | y :: ys ->
                [showOne y] @ ShowMany ys
        formlet
        |> FormletSnippets.RunInBlock title (fun l ->
            Div <| ShowMany l)

module DependentFormlets =
    open FormletSnippets
    open DependentFormletSnippets
    open IntelliFactory.WebSharper.Html

    type Snippets() = 
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
            Div [
                RunSnippet "Snippet2" Snippet2
                RunSnippet "Snippet4" Snippet4
                RunSnippet "Snippet4b" Snippet4b
                Div [RunSnippetList "Snippet3a" Snippet3a]
            ]
            :> _

module VariousDependentFormletSnippets =
    open IntelliFactory.Html
    open IntelliFactory.WebSharper.Sitelets
    
    type Action = | Home

    module Pages =
        let SnippetsPage =
            Content.PageContent <| fun ctx ->
                { Page.Default with
                    Title = Some "Dependent formlet snippets"
                    Body =
                        [
                            H1 [Text "Dependent snippets"]
                            Div [new DependentFormlets.Snippets()]
                        ]
                }

    let EntireSite =
        Sitelet.Content "/" Action.Home Pages.SnippetsPage

    type Website() =
        interface IWebsite<Action> with
            member this.Sitelet = EntireSite
            member this.Actions = []
