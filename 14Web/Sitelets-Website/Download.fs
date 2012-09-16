namespace Website

open IntelliFactory.WebSharper
open System.IO

module DownloadSite =
    open IntelliFactory.WebSharper.Sitelets

    type Action = | Download

    module Pages =
        let streamFile virtualPath (stream : Stream) =
            let filename = System.Web.HttpContext.Current.Server.MapPath(virtualPath)
            use fs = File.OpenRead(filename)
            use bs = new BufferedStream(fs)
            let rec loop() =
                let b = bs.ReadByte()
                if b >= 0 then
                    stream.WriteByte((byte)b)
                    loop()
            loop()

        let DownloadPage =
            Content.CustomContent <| fun ctx ->
                let cd = "attachment; filename=\"myfile.zip\""
                {
                     Status = Http.Status.Ok
                     Headers = [Http.Header.Custom "Content-Disposition" cd]
                     WriteBody = streamFile("~/myfile.zip")
                }

    let EntireSite = Sitelet.Content "/" Action.Download Pages.DownloadPage

    type Website() =
        interface IWebsite<Action> with
            member this.Sitelet = EntireSite
            member this.Actions = []
