namespace Website

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Core

type MyResource() =
    interface Resources.IResource with
        member this.Render ctx writer =
            writer.WriteLine "<script type=\"javascript\" src=\"lib\\my.js\"></script>"

[<assembly : System.Web.UI.WebResource("my.js", "text/javascript")>]
do ()

type MyEmbeddedResource() =
    inherit Resources.BaseResource("my.js")

type MyExternalResource() =
    inherit Resources.BaseResource(@"http:\\your.domain.net", "lib.js", "style.css")

[<Require(typeof<MyResource>)>]
[<Require(typeof<MyEmbeddedResource>)>]
[<Require(typeof<MyExternalResource>)>]
type Hello() =
    inherit Web.Control()

    [<JavaScript>]
    override this.Body = HelloWorld () :> _

module ResourceTrackingSite =
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
                                Div [new Hello()]
                            ]
                }

    let EntireSite = Sitelet.Content "/" Action.MyPage Pages.MyPage

    type Website() =
        interface IWebsite<Action> with
            member this.Sitelet = EntireSite
            member this.Actions = []

// Load the site and you should see in the console that the following
// resources failed to load, where 53025 is the port IIS express is running on ...
//GET http://localhost:53025/lib/my-resource.js 404 (Not Found)
//GET http://localhost:53025/my-embedded-resource.js 404 (Not Found)
//GET http://your.domain.netmy-external.js/
//GET http://your.domain.netstyle.css/
