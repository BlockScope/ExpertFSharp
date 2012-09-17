(function()
{
 var Global=this,Runtime=this.IntelliFactory.Runtime,WebSharper,Html,Default,List,T,Operators,Formlet,Formlet1,Website,FormletSnippets,Controls,Data,Enhance;
 Runtime.Define(Global,{
  Website:{
   FormletSnippets:{
    RunInBlock:function(title,f,formlet)
    {
     var output,x,f1,f2,f4;
     output=Default.Div(Runtime.New(T,{
      $:0
     }));
     x=(f1=(f2=function(res)
     {
      var elem,x1,f3;
      elem=f(res);
      x1=Operators.add(output,List.ofArray([elem]));
      f3=function(value)
      {
       value;
      };
      return f3(x1);
     },function(formlet1)
     {
      return Formlet1.Run(f2,formlet1);
     }),f1(formlet));
     f4=function(form)
     {
      var _this,x1,_this1;
      return Operators.add(Default.Div(List.ofArray([(_this=Default.Attr(),_this.NewAttr("style","float:left;margin-right:20px;width:300px;min-height:200px;"))])),List.ofArray([(x1=List.ofArray([Default.Text(title)]),(_this1=Default.Tags(),_this1.NewTag("h5",x1))),Default.Div(List.ofArray([form])),output]));
     };
     return f4(x);
    },
    RunSnippet:function(title,formlet)
    {
     var f,f1;
     f=(f1=function(s)
     {
      var x,f2;
      return Default.Div(List.ofArray([Default.P(List.ofArray([(x="You entered: "+s,(f2=function(x1)
      {
       return Default.Text(x1);
      },f2(x)))]))]));
     },function(formlet1)
     {
      return FormletSnippets.RunInBlock(title,f1,formlet1);
     });
     return f(formlet);
    },
    Snippet1:Runtime.Field(function()
    {
     return Controls.Input("initial value");
    }),
    Snippet1a:Runtime.Field(function()
    {
     var x,x1,f,arg00;
     return Data.$((x=function(name)
     {
      return name;
     },Formlet1.Return(x)),(x1=Controls.Input(""),(f=(arg00=function(s)
     {
      return s.length>3;
     },function(arg20)
     {
      return Data.Validator().Is(arg00,"Enter a valid name",arg20);
     }),f(x1))));
    }),
    Snippet1b:Runtime.Field(function()
    {
     var x,x1,x2,f,f1;
     return Data.$((x=function(name)
     {
      return name;
     },Formlet1.Return(x)),(x1=(x2=Controls.Input(""),(f=function(arg10)
     {
      return Data.Validator().IsNotEmpty("Enter a valid name",arg10);
     },f(x2))),(f1=function(formlet)
     {
      return Enhance.WithFormContainer(formlet);
     },f1(x1))));
    }),
    Snippet1c:Runtime.Field(function()
    {
     var x,x1,x2,x3,f,f1,f2;
     return Data.$((x=function(name)
     {
      return name;
     },Formlet1.Return(x)),(x1=(x2=(x3=Controls.Input(""),(f=function(arg10)
     {
      return Data.Validator().IsNotEmpty("Enter a valid name",arg10);
     },f(x3))),(f1=function(formlet)
     {
      return Enhance.WithFormContainer(formlet);
     },f1(x2))),(f2=function(formlet)
     {
      return Enhance.WithSubmitAndResetButtons(formlet);
     },f2(x1))));
    }),
    Snippet1d:Runtime.Field(function()
    {
     var x,x1,x2,x3,x4,x5,f,f1,f2,f3,f4;
     return Data.$((x=function(name)
     {
      return name;
     },Formlet1.Return(x)),(x1=(x2=(x3=(x4=(x5=Controls.Input(""),(f=function(arg10)
     {
      return Data.Validator().IsNotEmpty("Enter a valid name",arg10);
     },f(x5))),(f1=function(formlet)
     {
      return Enhance.WithValidationIcon(formlet);
     },f1(x4))),(f2=function(formlet)
     {
      return Enhance.WithErrorSummary("Errors",formlet);
     },f2(x3))),(f3=function(formlet)
     {
      return Enhance.WithSubmitAndResetButtons(formlet);
     },f3(x2))),(f4=function(formlet)
     {
      return Enhance.WithFormContainer(formlet);
     },f4(x1))));
    }),
    Snippet1e:Runtime.Field(function()
    {
     var x,x1,x2,x3,x4,x5,f,f1,f2,f3,f4;
     return Data.$((x=function(name)
     {
      return name;
     },Formlet1.Return(x)),(x1=(x2=(x3=(x4=(x5=Controls.Input(""),(f=function(arg10)
     {
      return Data.Validator().IsNotEmpty("Enter a valid name",arg10);
     },f(x5))),(f1=function(formlet)
     {
      return Enhance.WithValidationIcon(formlet);
     },f1(x4))),(f2=function(formlet)
     {
      return Enhance.WithTextLabel("Name",formlet);
     },f2(x3))),(f3=function(formlet)
     {
      return Enhance.WithSubmitAndResetButtons(formlet);
     },f3(x2))),(f4=function(formlet)
     {
      return Enhance.WithFormContainer(formlet);
     },f4(x1))));
    }),
    Snippet1f:Runtime.Field(function()
    {
     var x,x1,x2,x3,x4,x5,f,f1,f2,f3,f4;
     return Data.$((x=function(name)
     {
      return name;
     },Formlet1.Return(x)),(x1=(x2=(x3=(x4=(x5=Controls.Input(""),(f=function(arg10)
     {
      return Data.Validator().IsNotEmpty("Enter a valid name",arg10);
     },f(x5))),(f1=function(formlet)
     {
      return Enhance.WithValidationIcon(formlet);
     },f1(x4))),(f2=function(formlet)
     {
      return Enhance.WithLabelAndInfo("Name","Enter your name",formlet);
     },f2(x3))),(f3=function(formlet)
     {
      return Enhance.WithSubmitAndResetButtons(formlet);
     },f3(x2))),(f4=function(formlet)
     {
      return Enhance.WithFormContainer(formlet);
     },f4(x1))));
    })
   },
   Formlets:{
    Snippet:Runtime.Class({
     get_Body:function()
     {
      return FormletSnippets.RunSnippet("Snippet1",FormletSnippets.Snippet1());
     }
    }),
    Snippets:Runtime.Class({
     get_Body:function()
     {
      return Default.Div(List.ofArray([FormletSnippets.RunSnippet("Snippet1",FormletSnippets.Snippet1()),FormletSnippets.RunSnippet("Snippet1a",FormletSnippets.Snippet1a()),FormletSnippets.RunSnippet("Snippet1b",FormletSnippets.Snippet1b()),FormletSnippets.RunSnippet("Snippet1c",FormletSnippets.Snippet1c()),FormletSnippets.RunSnippet("Snippet1d",FormletSnippets.Snippet1d()),FormletSnippets.RunSnippet("Snippet1e",FormletSnippets.Snippet1e()),FormletSnippets.RunSnippet("Snippet1f",FormletSnippets.Snippet1f())]));
     }
    })
   }
  }
 });
 Runtime.OnInit(function()
 {
  WebSharper=Runtime.Safe(Global.IntelliFactory.WebSharper);
  Html=Runtime.Safe(WebSharper.Html);
  Default=Runtime.Safe(Html.Default);
  List=Runtime.Safe(WebSharper.List);
  T=Runtime.Safe(List.T);
  Operators=Runtime.Safe(Html.Operators);
  Formlet=Runtime.Safe(WebSharper.Formlet);
  Formlet1=Runtime.Safe(Formlet.Formlet);
  Website=Runtime.Safe(Global.Website);
  FormletSnippets=Runtime.Safe(Website.FormletSnippets);
  Controls=Runtime.Safe(Formlet.Controls);
  Data=Runtime.Safe(Formlet.Data);
  return Enhance=Runtime.Safe(Formlet.Enhance);
 });
 Runtime.OnLoad(function()
 {
  FormletSnippets.Snippet1f();
  FormletSnippets.Snippet1e();
  FormletSnippets.Snippet1d();
  FormletSnippets.Snippet1c();
  FormletSnippets.Snippet1b();
  FormletSnippets.Snippet1a();
  FormletSnippets.Snippet1();
 });
}());
