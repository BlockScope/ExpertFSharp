(function()
{
 var Global=this,Runtime=this.IntelliFactory.Runtime,WebSharper,Formlet,Data,Formlet1,Controls,Enhance,Html,Default,List,Website,Client;
 Runtime.Define(Global,{
  Website:{
   Client:{
    OrderForm:function(orderPostUrl)
    {
     var x,x1,x2,x3,x4,x5,x6,f,f1,x7,x8,x9,f2,f3,f4,f5,f6,f7,f8,f9,conf,EncodingType;
     x=(x1=(x2=(x3=Data.$(Data.$((x4=function(title)
     {
      return function(qty)
      {
       return{
        ItemName:title,
        Quantity:qty
       };
      };
     },Formlet1.Return(x4)),(x5=(x6=Controls.Input(""),(f=function(arg10)
     {
      return Data.Validator().IsNotEmpty("Must enter a title",arg10);
     },f(x6))),(f1=function(formlet)
     {
      return Enhance.WithTextLabel("Title",formlet);
     },f1(x5)))),(x7=(x8=(x9=Controls.Input(""),(f2=Data.Validator().IsInt("Must enter a valid quantity"),f2(x9))),(f3=(f4=function(value)
     {
      return value<<0;
     },function(formlet)
     {
      return Formlet1.Map(f4,formlet);
     }),f3(x8))),(f5=function(formlet)
     {
      return Enhance.WithTextLabel("Quantity",formlet);
     },f5(x7)))),(f6=function(formlet)
     {
      return Enhance.WithSubmitAndResetButtons(formlet);
     },f6(x3))),(f7=function(formlet)
     {
      return Enhance.WithErrorSummary("Errors",formlet);
     },f7(x2))),(f8=function(formlet)
     {
      return Enhance.WithFormContainer(formlet);
     },f8(x1)));
     f9=(conf=(EncodingType={
      $:0
     },{
      PostUrl:{
       $:1,
       $0:orderPostUrl
      },
      ParameterName:"order",
      EncodingType:EncodingType
     }),function(formlet)
     {
      return Enhance.WithJsonPost(conf,formlet);
     });
     return f9(x);
    },
    OrderFormControl:Runtime.Class({
     get_Body:function()
     {
      return Default.Div(List.ofArray([Client.OrderForm(this.orderPostUrl)]));
     }
    })
   }
  }
 });
 Runtime.OnInit(function()
 {
  WebSharper=Runtime.Safe(Global.IntelliFactory.WebSharper);
  Formlet=Runtime.Safe(WebSharper.Formlet);
  Data=Runtime.Safe(Formlet.Data);
  Formlet1=Runtime.Safe(Formlet.Formlet);
  Controls=Runtime.Safe(Formlet.Controls);
  Enhance=Runtime.Safe(Formlet.Enhance);
  Html=Runtime.Safe(WebSharper.Html);
  Default=Runtime.Safe(Html.Default);
  List=Runtime.Safe(WebSharper.List);
  Website=Runtime.Safe(Global.Website);
  return Client=Runtime.Safe(Website.Client);
 });
 Runtime.OnLoad(function()
 {
 });
}());
