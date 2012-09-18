namespace Website

open IntelliFactory.WebSharper
open IntelliFactory.WebSharper.Core

// Put "my-render-resource.js" in the root of the web project.
type MyResource() =
    interface Resources.IResource with
        member this.Render ctx writer =
            writer.WriteLine "<script src='my-render-resource.js' type='javascript'></script>"

// Choose a build action for "my-assembly-resource.js" of embedded resource.
[<assembly : System.Web.UI.WebResource("my-assembly-resource.js", "text/javascript")>]
do ()

// Choose a build action for "my-embedded-resource.js" of embedded resource.
type MyEmbeddedResource() =
    inherit Resources.BaseResource("my-embedded-resource.js")

// Unless you own your.domain.net, these resources will render to the page but fail to load.
type MyExternalResource() =
    inherit Resources.BaseResource("http://your.domain.net/", "my-external-resource.js", "style.css")

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
// resources failed to load because your.domain.net doesn't exist
//GET http://your.domain.net/my-external-resource.js
//GET http://your.domain.net/style.css

// Require(typeof<MyEmbeddedResource>) -> "my-embedded-resource.js"
// This file, embedded in this assembly cannot be found. I can check
// that this link is generated from the above require attribute by
// comparing the rendered page when it is commented and uncommented.
//GET http://localhost:53025/WebResource.axd?d=cwbnMROIooSe6Tcq4dK184tFmO5LTVbRJsfEiwc7T4mh2jc_yJJa9XYr2gKExSH9QB1G0yWOj8hTXRGPvDiMtpfOYURxfTJh7HKHcYhKB0CtWu7lZOC8fDcauoMS0siNbNm2pFH2CpYZ-sEyQzBhwg2&t=634836383055770902 404 (Not Found)

// Commenting and uncommenting the following does not change the rendered html output.
// Choose a build action for "my-assembly-resource.js" of embedded resource.
//[<assembly : System.Web.UI.WebResource("my-assembly-resource.js", "text/javascript")>]
//do ()
