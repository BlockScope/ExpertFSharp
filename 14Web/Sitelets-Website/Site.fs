namespace Website

open IntelliFactory.WebSharper

module MySite =
    open IntelliFactory.Html
    open IntelliFactory.WebSharper.Sitelets

    type Action = | MyPage

    module Pages =
        let MyPage =
            Content.PageContent <| fun ctx ->
                {
                    Page.Default with
                        Title = Some "My page"
                        Body =
                            [
                                H1 [Text "Hello world!"]
                            ]
                }

    let EntireSite = Sitelet.Content "/" Action.MyPage Pages.MyPage

    type MyWebsite() =
        interface IWebsite<Action> with
            member this.Sitelet = EntireSite
            member this.Actions = []

[<assembly : Sitelets.Website(typeof<MySite.MyWebsite>)>]
do ()
