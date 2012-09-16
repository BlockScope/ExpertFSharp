namespace Website

module SiteletSumCombinator =
    open IntelliFactory.Html
    open IntelliFactory.WebSharper.Sitelets

    type Action =
        | [<CompiledName "home">] MyPage
        | Protected
        | Login of Action option
        | Logout

    module Pages =
        /// A helper function to create a hyperlink.
        let (=>) title href = A [HRef href] -< [Text title]

        /// A helper function to create a 'fresh' URL with a random parameter
        /// in order to make sure that browsers don't show a cached version.
        let R url = url + "?d=" + System.Uri.EscapeUriString (System.DateTime.Now.ToString())

        module Utils =
            let SimpleContent title content = Content.PageContent <| fun ctx ->
                {
                    Page.Default with
                        Title = Some title
                        Body =
                            [
                                match UserSession.GetLoggedInUser() with
                                | None ->
                                    yield "Login" => ctx.Link (Action.Login (Some Action.MyPage))
                                | Some user ->
                                    yield "Log out ["+user+"]" => R (ctx.Link Action.Logout)
                            ] @ content ctx
                }

        let MyPage = Utils.SimpleContent "My Page" <| fun ctx ->
            [
                H1 [Text "Hello world!"]
                "Protected content" => R (ctx.Link Action.Protected)
            ]

        let ProtectedPage = Utils.SimpleContent "Protected Page" <| fun ctx ->
            [
                H1 [Text "This is protected content!"]
                "Go back" => (ctx.Link Action.MyPage)
            ]

        let LoginPage action = Utils.SimpleContent "Login Page" <| fun ctx ->
            let redirectUrl =
                match action with
                | None -> Action.MyPage
                | Some action -> action
                |> ctx.Link
                |> R
            [
                H1 [Text "You have been logged in magically..."]
                "Proceed further" => redirectUrl
            ]

    let NonProtected = Sitelet.Infer <| function
        | Action.MyPage -> Pages.MyPage

        // Log in a user as "visitor" without requiring anything
        | Action.Login action ->
            UserSession.LoginUser "visitor"
            Pages.LoginPage action

        // Log out the "visitor" user and redirect to home
        | Action.Logout ->
            UserSession.Logout ()
            Content.Redirect Action.MyPage

        | Action.Protected ->
            match UserSession.GetLoggedInUser() with
            | None -> Content.ServerError
            | Some _ -> Pages.ProtectedPage

    let Protected =
        let filter : Sitelet.Filter<Action> =
            {
                VerifyUser = fun _ -> true
                LoginRedirect = Some >> Action.Login
            }

        Sitelet.Protect filter <|
            Sitelet.Content "/protected" Action.Protected Pages.ProtectedPage

    let EntireSite = Protected <|> NonProtected

    type Website() =
        interface IWebsite<Action> with
            member this.Sitelet = EntireSite
            member this.Actions = []


