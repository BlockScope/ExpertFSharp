namespace Website

type Order =
    {
        ItemName : string
        Quantity : int
    }
    static member Dummy() =
        {ItemName = "N/A"; Quantity = 0}

module Orders =
    let private id = ref 0
    let Store = ref Map.empty

    let Save (id : int) (order : Order) =
        Store := (!Store).Add(id, order)

    let FindById (id : int) =
        if (!Store).ContainsKey id then
            Some <| (!Store).[id]
        else
            None

    let Delete (id : int) =
        if (!Store).ContainsKey id then
            Store := (!Store).Remove id

    let GetId () =
        id := !id + 1
        !id

open IntelliFactory.WebSharper

type Action =
    | CreateOrderForm
    | CreateOrder of Order
    | DeleteOrder of int
    | GetOrder of int
    | ListOrders
    | UpdateOrder of int * Order

module Skin =
    open System.Web
    open IntelliFactory.Html
    open IntelliFactory.WebSharper.Sitelets

    let TemplateLoadFrequency = Content.Template.PerRequest

    type Page = {Body : list<Content.HtmlElement>}

    let MainTemplate =
        let path = HttpContext.Current.Server.MapPath("~/Main.html")
        Content.Template<Page>(path, TemplateLoadFrequency)
            .With("body", fun x -> x.Body)

    let WithTemplate body : Content<Action> =
        Content.WithTemplate MainTemplate <| fun context ->
            {
                Body = body context
            }

module Client =
    open IntelliFactory.WebSharper.Formlet
    open IntelliFactory.WebSharper.Html

    [<JavaScript>]
    let OrderForm orderPostUrl =
        Formlet.Yield (fun title qty -> {ItemName = title; Quantity = qty})
        <*> (Controls.Input ""
            |> Validator.IsNotEmpty "Must enter a title"
            |> Enhance.WithTextLabel "Title")
        <*> (Controls.Input ""
            |> Validator.IsInt "Must enter a valid quantity"
            |> Formlet.Map int
            |> Enhance.WithTextLabel "Quantity")
        |> Enhance.WithSubmitAndResetButtons
        |> Enhance.WithErrorSummary "Errors"
        |> Enhance.WithFormContainer
        |> Enhance.WithJsonPost
            {
                Enhance.JsonPostConfiguration.EncodingType = None
                Enhance.JsonPostConfiguration.ParameterName = "order"
                Enhance.JsonPostConfiguration.PostUrl = Some orderPostUrl
            }

    type OrderFormControl(orderPostUrl : string) =
        inherit Web.Control()

        [<JavaScript>]
        override this.Body =
            Div [
                OrderForm(orderPostUrl)
            ] :> _

module Pages =
    open IntelliFactory.Html
    open IntelliFactory.WebSharper.Sitelets

    let ( => ) text url =
        A [HRef url] -< [Text text]

    let Links (ctx : Context<Action>) =
        UL [
            LI ["Home" => ctx.Link Action.ListOrders]
            LI ["New"  => ctx.Link Action.CreateOrderForm]
        ]

    let CreateOrderFormPage =
        Skin.WithTemplate <| fun ctx ->
            [
                H1 [Text "Create order"]
                Links ctx
                HR []
                Div [
                    new Client.OrderFormControl(
                        Order.Dummy() |> Action.CreateOrder |> ctx.Link
                    )
                ]
            ]

    let ListOrdersPage =
        Skin.WithTemplate <| fun ctx ->
            [
                H1 [Text "Orders"]
                Links ctx
                HR []
                UL <|
                    ((!Orders.Store)
                    |> Map.toList
                    |> Seq.map (fun (id, order) ->
                        LI [
                            A [HRef <| ctx.Link (Action.GetOrder id)] -< [
                                sprintf "#%d: %s [%d]" id order.ItemName order.Quantity
                                |> Text
                            ]
                        ]
                    ))
            ]

    let GetOrder id =
        if (!Orders.Store).ContainsKey id then
            Content.CustomContent <| fun ctx ->
                {
                    Http.Response.Status = Http.Status.Ok
                    Http.Response.Headers = [Http.Header.Custom "Content-type" "application/json"]
                    Http.Response.WriteBody = fun stream ->
                        use writer = new System.IO.StreamWriter(stream)
                        let order = (!Orders.Store).[id]
                        let encoder = Web.Shared.Json.GetEncoder(typeof<Order>)
                        order
                        |> encoder.Encode
                        |> Web.Shared.Json.Pack
                        |> Core.Json.Stringify
                        |> writer.Write
                        writer.Close()
                }
        else
            Content.NotFound

module MySite =
    open IntelliFactory.WebSharper.Sitelets
    open UrlHelpers

    let (|PATH|_|) (uri : System.Uri) = Some <| uri.LocalPath

    let MySitelet =
        Sitelet.Sum [
            Sitelet.Content "/create" Action.CreateOrderForm Pages.CreateOrderFormPage
            Sitelet.Content "/orders" Action.ListOrders Pages.ListOrdersPage
        ]
        <|>
        {
            Router = Sitelets.Router.New
                <| function
                    | POST (values, PATH @"/order") ->
                        try
                            let decoder = Web.Shared.Json.GetDecoder<Order>()
                            let order =
                                values
                                |> Map.ofList
                                |> fun map -> map.["order"]
                                |> Core.Json.Parse
                                |> decoder.Decode
                            Some <| Action.CreateOrder order
                        with
                        | _ ->
                            None
                    | GET (values, SPLIT_BY '/' ["order"; INT id]) ->
                        Some <| Action.GetOrder id
                    | PUT (values, SPLIT_BY '/' ["/order"; INT id]) ->
                        try
                            let decoder = Web.Shared.Json.GetDecoder<Order>()
                            let order =
                                values
                                |> Map.ofList
                                |> fun map -> map.["order"]
                                |> Core.Json.Parse
                                |> decoder.Decode
                            Some <| Action.UpdateOrder(id, order)
                        with
                        | _ ->
                            None
                    | DELETE (values, SPLIT_BY '/' ["/order"; INT id]) ->
                        Some <| Action.DeleteOrder id
                    | _ ->
                        None
                <| function
                    | Action.CreateOrder order ->
                        Some <| System.Uri(@"/order", System.UriKind.Relative)
                    | Action.DeleteOrder id
                    | Action.GetOrder id
                    | Action.UpdateOrder (id, _) ->
                        Some <| System.Uri(sprintf @"/order/%d" id, System.UriKind.Relative)
                    | _ ->
                        None

            Controller =
                {
                    Handle = function
                        | Action.CreateOrder order ->
                            Orders.Save (Orders.GetId()) order
                            Content.Redirect Action.ListOrders
                        | Action.DeleteOrder id ->
                            Orders.Delete id
                            Content.Redirect Action.ListOrders
                        | Action.GetOrder id ->
                            Pages.GetOrder id
                        | Action.UpdateOrder (id, order) ->
                            Orders.Save id order
                            Content.Redirect Action.ListOrders
                        | _ ->
                            failwith "unmatched"
                }
        }

    type MyWebsite() =
        interface IWebsite<Action> with
            member this.Sitelet = MySitelet
            member this.Actions = []

[<assembly : Sitelets.Website(typeof<MySite.MyWebsite>)>]
do ()
