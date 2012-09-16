﻿namespace Website

open System.IO
open IntelliFactory.WebSharper.Sitelets

module EmbeddedControlSite =
    open IntelliFactory.WebSharper
    open IntelliFactory.Html

    type Action = | Home

    module Skin =
        type Placeholders =
            {
                Title : string
                Body : list<Content.HtmlElement>
            }

        let MainTemplate =
            let path = Path.Combine(__SOURCE_DIRECTORY__, "MyTemplate.html")
            Content.Template<Placeholders>(path)
                .With("title", fun x -> x.Title)
                .With("body", fun x -> x.Body)

        let WithTemplate title body : Content<Action> =
            Content.WithTemplate MainTemplate <| fun context ->
                {
                    Title = title
                    Body = body context
                }

    let EntireSite =
        Skin.WithTemplate "Hello World" <| fun ctx ->
            [
                Div [new Website.MyControl()]
            ]
        |>
        Sitelet.Content "/" Action.Home

    type Website() =
        interface IWebsite<Action> with
            member this.Sitelet = EntireSite
            member this.Actions = []
