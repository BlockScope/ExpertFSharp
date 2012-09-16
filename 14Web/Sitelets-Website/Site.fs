namespace Website

open IntelliFactory.WebSharper

// Choose to uncomment only one of these lines ...
[<assembly : Sitelets.Website(typeof<MySite.Website>)>]
//[<assembly : Sitelets.Website(typeof<DownloadSite.Website>)>]
do ()
