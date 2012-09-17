namespace Website

open IntelliFactory.WebSharper

// Choose to uncomment only one of these lines ...
//[<assembly : Sitelets.Website(typeof<OnePageSite.Website>)>]
//[<assembly : Sitelets.Website(typeof<DownloadSite.Website>)>]
//[<assembly : Sitelets.Website(typeof<DynamicTemplateSite.Website>)>]
//[<assembly : Sitelets.Website(typeof<EmbeddedControlSite.Website>)>]
//[<assembly : Sitelets.Website(typeof<CombiningSitelets.Website>)>]
//[<assembly : Sitelets.Website(typeof<CompiledNameOfAction.Website>)>]
//[<assembly : Sitelets.Website(typeof<SiteletSumCombinator.Website>)>]
//[<assembly : Sitelets.Website(typeof<VariousFormletSnippets.Website>)>]
[<assembly : Sitelets.Website(typeof<VariousDependentFormletSnippets.Website>)>]
do ()

