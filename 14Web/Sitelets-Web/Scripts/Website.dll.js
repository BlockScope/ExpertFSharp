(function()
{
 var Global=this,Runtime=this.IntelliFactory.Runtime,WebSharper,Html,Default,List,EventsPervasives,Website,HelloWorld;
 Runtime.Define(Global,{
  Website:{
   HelloWorld:{
    HelloWorld:function()
    {
     var welcome,x,_this,_this1,f,x1;
     welcome=Default.P(List.ofArray([Default.Text("Welcome")]));
     return Default.Div(List.ofArray([welcome,(x=Default.Input(List.ofArray([(_this=Default.Attr(),_this.NewAttr("type","Button")),(_this1=Default.Attr(),_this1.NewAttr("value","Click me!"))])),(f=(x1=function()
     {
      return function()
      {
       return welcome.set_Text("Hello, world!");
      };
     },function(arg10)
     {
      return EventsPervasives.Events().OnClick(x1,arg10);
     }),(f(x),x)))]));
    }
   },
   MyControl:Runtime.Class({
    get_Body:function()
    {
     return HelloWorld.HelloWorld();
    }
   })
  }
 });
 Runtime.OnInit(function()
 {
  WebSharper=Runtime.Safe(Global.IntelliFactory.WebSharper);
  Html=Runtime.Safe(WebSharper.Html);
  Default=Runtime.Safe(Html.Default);
  List=Runtime.Safe(WebSharper.List);
  EventsPervasives=Runtime.Safe(Html.EventsPervasives);
  Website=Runtime.Safe(Global.Website);
  return HelloWorld=Runtime.Safe(Website.HelloWorld);
 });
 Runtime.OnLoad(function()
 {
 });
}());
