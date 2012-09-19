(function()
{
 var Global=this,Runtime=this.IntelliFactory.Runtime,WebSharper,Html,Default,List,Website,HasJs,Operators,Modernizr;
 Runtime.Define(Global,{
  Website:{
   HasJs:{
    Test:function()
    {
     return Default.Div(List.ofArray([Default.P(List.ofArray([Default.Text("This is a test using modernizr ...")])),HasJs.report("canvas"),HasJs.report("canvastext"),HasJs.report("webgl"),HasJs.report("audio"),HasJs.report("webaudio"),HasJs.report("video"),HasJs.report("websockets"),HasJs.report("geolocation")]));
    },
    report:function(s)
    {
     var ifyes,ifno;
     ifyes=function()
     {
      var _this;
      return Operators.add(Default.P(List.ofArray([Default.Text(s+" ")])),List.ofArray([Operators.add(Default.Span(List.ofArray([(_this=Default.Attr(),_this.NewAttr("style","color:green;font-style:italic"))])),List.ofArray([Default.Text("true")]))]));
     };
     ifno=function()
     {
      var _this;
      return Operators.add(Default.P(List.ofArray([Default.Text(s+" ")])),List.ofArray([Operators.add(Default.Span(List.ofArray([(_this=Default.Attr(),_this.NewAttr("style","color:red;font-style:italic"))])),List.ofArray([Default.Text("false")]))]));
     };
     if(Modernizr[s])
      {
       return ifyes();
      }
     else
      {
       return ifno();
      }
    }
   },
   HasTester:Runtime.Class({
    get_Body:function()
    {
     return HasJs.Test();
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
  Website=Runtime.Safe(Global.Website);
  HasJs=Runtime.Safe(Website.HasJs);
  Operators=Runtime.Safe(Html.Operators);
  return Modernizr=Runtime.Safe(Global.Modernizr);
 });
 Runtime.OnLoad(function()
 {
 });
}());
