(function()
{
 var Global=this,Runtime=this.IntelliFactory.Runtime,WebSharper,Android,Context,Html,Default,List,T,Android1,Client,Operators;
 Runtime.Define(Global,{
  Android:{
   Client:{
    Main:function(canvas)
    {
     var matchValue,ctx;
     matchValue=Context.Get();
     if(matchValue.$==1)
      {
       ctx=matchValue.$0;
       ctx.Trace({
        $:1
       },"Website","Starting the mobile app..");
       return canvas.set_Text("Android started");
      }
     else
      {
       return canvas.set_Text("No Android context");
      }
    }
   },
   Controls:{
    MainControl:Runtime.Class({
     get_Body:function()
     {
      var x,f,f1;
      x=Default.Div(Runtime.New(T,{
       $:0
      }));
      f=(f1=function(canvas)
      {
       return Client.Main(canvas);
      },function(w)
      {
       return Operators.OnAfterRender(f1,w);
      });
      f(x);
      return x;
     }
    })
   }
  }
 });
 Runtime.OnInit(function()
 {
  WebSharper=Runtime.Safe(Global.IntelliFactory.WebSharper);
  Android=Runtime.Safe(WebSharper.Android);
  Context=Runtime.Safe(Android.Context);
  Html=Runtime.Safe(WebSharper.Html);
  Default=Runtime.Safe(Html.Default);
  List=Runtime.Safe(WebSharper.List);
  T=Runtime.Safe(List.T);
  Android1=Runtime.Safe(Global.Android);
  Client=Runtime.Safe(Android1.Client);
  return Operators=Runtime.Safe(Html.Operators);
 });
 Runtime.OnLoad(function()
 {
 });
}());
