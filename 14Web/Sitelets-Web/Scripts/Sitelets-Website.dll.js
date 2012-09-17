(function()
{
 var Global=this,Runtime=this.IntelliFactory.Runtime,WebSharper,Html,Default,List,String,Website,FormletSnippets,T,Formlet,Data,Formlet1,DependentFormletSnippets,Enhance,Controls,Operators;
 Runtime.Define(Global,{
  Website:{
   DependentFormletSnippets:{
    RunSnippet:function(title,formlet)
    {
     var f,f1;
     f=(f1=Runtime.Tupled(function(tupledArg)
     {
      var s,i,x,f2;
      s=tupledArg[0];
      i=tupledArg[1];
      return Default.Div(List.ofArray([Default.P(List.ofArray([(x="You entered: "+s+" "+String(i),(f2=function(x1)
      {
       return Default.Text(x1);
      },f2(x)))]))]));
     }),function(formlet1)
     {
      return FormletSnippets.RunInBlock(title,f1,formlet1);
     });
     return f(formlet);
    },
    RunSnippetList:function(title,formlet)
    {
     var showOne,ShowMany,f1,f2;
     showOne=Runtime.Tupled(function(x)
     {
      var s,i,x1,f;
      s=x[0];
      i=x[1];
      return Default.Div(List.ofArray([Default.P(List.ofArray([(x1="You entered: "+s+" "+String(i),(f=function(x2)
      {
       return Default.Text(x2);
      },f(x1)))]))]));
     });
     ShowMany=function(xs)
     {
      var ys,y,a,b;
      if(xs.$==1)
       {
        ys=xs.$1;
        y=xs.$0;
        a=List.ofArray([showOne(y)]);
        b=ShowMany(ys);
        return List.append(a,b);
       }
      else
       {
        return Runtime.New(T,{
         $:0
        });
       }
     };
     f1=(f2=function(l)
     {
      var x;
      x=ShowMany(l);
      return Default.Div(x);
     },function(formlet1)
     {
      return FormletSnippets.RunInBlock(title,f2,formlet1);
     });
     return f1(formlet);
    },
    Snippet2:Runtime.Field(function()
    {
     var x,x1,x2,f1,f2;
     x=(x1=Data.$(Data.$((x2=function(name)
     {
      return function(age)
      {
       var f;
       return[name,(f=function(value)
       {
        return value<<0;
       },f(age))];
      };
     },Formlet1.Return(x2)),DependentFormletSnippets.input("Name","Please enter your name")),DependentFormletSnippets.inputInt("Age","Please enter a valid age")),(f1=function(formlet)
     {
      return Enhance.WithSubmitAndResetButtons(formlet);
     },f1(x1)));
     f2=function(formlet)
     {
      return Enhance.WithFormContainer(formlet);
     };
     return f2(x);
    }),
    Snippet3a:Runtime.Field(function()
    {
     var x,x1,x2,x3,x4,x5,x6,f1,f2,f3,f4,f5,f6;
     x=(x1=(x2=(x3=(x4=(x5=Data.$(Data.$((x6=function(name)
     {
      return function(age)
      {
       var f;
       return[name,(f=function(value)
       {
        return value<<0;
       },f(age))];
      };
     },Formlet1.Return(x6)),DependentFormletSnippets.input("Name","Please enter your name")),DependentFormletSnippets.inputInt("Age","Please enter a valid age")),(f1=function(formlet)
     {
      return Enhance.WithLegend("Person",formlet);
     },f1(x5))),(f2=function(formlet)
     {
      return Enhance.WithTextLabel("Person",formlet);
     },f2(x4))),(f3=function(formlet)
     {
      return Enhance.Many(formlet);
     },f3(x3))),(f4=function(formlet)
     {
      return Enhance.WithLegend("People",formlet);
     },f4(x2))),(f5=function(formlet)
     {
      return Enhance.WithSubmitAndResetButtons(formlet);
     },f5(x1)));
     f6=function(formlet)
     {
      return Enhance.WithFormContainer(formlet);
     };
     return f6(x);
    }),
    Snippet4:Runtime.Field(function()
    {
     var x,x1,_builder_,f1,f2;
     x=(x1=(_builder_=Formlet1.Do(),_builder_.Delay(function()
     {
      return _builder_.Bind(DependentFormletSnippets.input("Name","Please enter your name"),function(_arg1)
      {
       return _builder_.Bind(DependentFormletSnippets.inputInt("Age","Please enter a valid age"),function(_arg2)
       {
        var f;
        return _builder_.Return([_arg1,(f=function(value)
        {
         return value<<0;
        },f(_arg2))]);
       });
      });
     })),(f1=function(formlet)
     {
      return Enhance.WithSubmitAndResetButtons(formlet);
     },f1(x1)));
     f2=function(formlet)
     {
      return Enhance.WithFormContainer(formlet);
     };
     return f2(x);
    }),
    Snippet4b:Runtime.Field(function()
    {
     var x,_builder_,f5;
     x=(_builder_=Formlet1.Do(),_builder_.Delay(function()
     {
      var x1,x2,f,f1;
      return _builder_.Bind((x1=(x2=DependentFormletSnippets.input("Name","Please enter your name"),(f=function(formlet)
      {
       return Enhance.WithSubmitAndResetButtons(formlet);
      },f(x2))),(f1=function(formlet)
      {
       return Enhance.WithFormContainer(formlet);
      },f1(x1))),function(_arg1)
      {
       var x3,x4,f2,f3;
       return _builder_.Bind((x3=(x4=DependentFormletSnippets.inputInt("Age","Please enter a valid age"),(f2=function(formlet)
       {
        return Enhance.WithSubmitAndResetButtons(formlet);
       },f2(x4))),(f3=function(formlet)
       {
        return Enhance.WithFormContainer(formlet);
       },f3(x3))),function(_arg2)
       {
        var f4;
        return _builder_.Return([_arg1,(f4=function(value)
        {
         return value<<0;
        },f4(_arg2))]);
       });
      });
     }));
     f5=function(formlet)
     {
      return Formlet1.Flowlet(formlet);
     };
     return f5(x);
    }),
    input:function(label,err)
    {
     var x,x1,x2,f,f1,f2;
     x=(x1=(x2=Controls.Input(""),(f=function(arg10)
     {
      return Data.Validator().IsNotEmpty(err,arg10);
     },f(x2))),(f1=function(formlet)
     {
      return Enhance.WithValidationIcon(formlet);
     },f1(x1)));
     f2=function(formlet)
     {
      return Enhance.WithTextLabel(label,formlet);
     };
     return f2(x);
    },
    inputInt:function(label,err)
    {
     var x,x1,x2,f,f1,f2;
     x=(x1=(x2=Controls.Input(""),(f=Data.Validator().IsInt(err),f(x2))),(f1=function(formlet)
     {
      return Enhance.WithValidationIcon(formlet);
     },f1(x1)));
     f2=function(formlet)
     {
      return Enhance.WithTextLabel(label,formlet);
     };
     return f2(x);
    }
   },
   DependentFormlets:{
    Snippets:Runtime.Class({
     get_Body:function()
     {
      return Default.Div(List.ofArray([DependentFormletSnippets.RunSnippet("Snippet2",DependentFormletSnippets.Snippet2()),DependentFormletSnippets.RunSnippet("Snippet4",DependentFormletSnippets.Snippet4()),DependentFormletSnippets.RunSnippet("Snippet4b",DependentFormletSnippets.Snippet4b()),Default.Div(List.ofArray([DependentFormletSnippets.RunSnippetList("Snippet3a",DependentFormletSnippets.Snippet3a())]))]));
     }
    })
   },
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
  String=Runtime.Safe(Global.String);
  Website=Runtime.Safe(Global.Website);
  FormletSnippets=Runtime.Safe(Website.FormletSnippets);
  T=Runtime.Safe(List.T);
  Formlet=Runtime.Safe(WebSharper.Formlet);
  Data=Runtime.Safe(Formlet.Data);
  Formlet1=Runtime.Safe(Formlet.Formlet);
  DependentFormletSnippets=Runtime.Safe(Website.DependentFormletSnippets);
  Enhance=Runtime.Safe(Formlet.Enhance);
  Controls=Runtime.Safe(Formlet.Controls);
  return Operators=Runtime.Safe(Html.Operators);
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
  DependentFormletSnippets.Snippet4b();
  DependentFormletSnippets.Snippet4();
  DependentFormletSnippets.Snippet3a();
  DependentFormletSnippets.Snippet2();
 });
}());
