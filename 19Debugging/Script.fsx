open System.Windows.Forms

let form = new Form(Width = 400, Height = 300,
                    Visible = true, Text = "F# Forms Sample")
#if COMPILED
// Run the main code
System.Windows.Forms.Application.Run(form)
#endif

c:\dev\apress\f-3.0code\7Encapsulate\Library>fsc -a --doc:whales.xml whales.fs
//Microsoft (R) F# 3.0 Compiler build 11.0.50522.1
//Copyright (c) Microsoft Corporation. All Rights Reserved.

//You can also generate a simple XML documentation file using the --doc command-line option. You must name the output file. For example, using fsc -a --doc:whales.xml whales.fs for the code in Listing 7-10 generates the file whales.xml containing the following:
//<?xml version="1.0" encoding="utf-8"?>
//<doc>
//<assembly><name>whales</name></assembly>
//<members>
//<member name="">
//
//</member>
//<member name="">
//
//</member>
//<member name="">
//
//</member>
//<member name="T:Whales.Fictional.WhaleKind">
//<summary>
// The three kinds of whales we cover in this release
//</summary>
//</member>
//<member name="P:Whales.Fictional.whales">
//<summary>
// The collected whales
//</summary>
//</member>
//<member name="P:Whales.Fictional.orca">
//<summary>
// This whale is for experimental use only
//</summary>
//</member>
//<member name="P:Whales.Fictional.bluey">
//<summary>
// The backup whale
//</summary>
//</member>
//<member name="P:Whales.Fictional.moby">
//<summary>
// The main whale
//</summary>
//</member>
//<member name="T:Whales.Fictional">
//
//</member>
//</members>
//</doc>

C:\Users\dsyme\Desktop> sn.exe -k whales.snk
//
//Microsoft (R) .NET Framework Strong Name Utility  Version 4.0.30319.17626
//Copyright (c) Microsoft Corporation.  All rights reserved.
//
//Key pair written to whales.snk

C:\Users\dsyme\Desktop> fsc -a --keyfile:whales.snk whales.fs
Microsoft (R) F# 3.0 Compiler build 11.0.50522.1
Copyright (c) Microsoft Corporation. All Rights Reserved.

C:\Users\dsyme\Desktop> gacutil /i whales.dll
//Microsoft (R) .NET Global Assembly Cache Utility.  Version 4.0.30319.17626
//Copyright (c) Microsoft Corporation.  All rights reserved.
//
//Assembly successfully added to the cache

let isPalindrome (str : string) =
    let rec check(s : int, e : int) =
        if s = e then true
        elif str.[s] <> str.[e] then false
        else check(s + 1, e - 1)

    check(0, str.Length - 1)
//val isPalindrome : str:string -> bool

open System.Diagnostics

let isPalindrome (str : string) =
    let rec check(s : int, e : int) =
        Debug.WriteLine("check call")
        Debug.WriteLineIf((s = 0), "check: First call")
        Debug.Assert((s >= 0 || s < str.Length), sprintf "s is out of bounds: %d" s)
        Debug.Assert((e >= 0 || e < str.Length), sprintf "e is out of bounds: %d" e)
        if s = e || s = e + 1 then true
        else if str.[s] <> str.[e] then false
        else check(s + 1, e - 1)
    check(0, str.Length - 1)
//val isPalindrome : str:string -> bool

open System

[<DebuggerDisplay("{re}+{im}i")>]
type MyComplex= {re : double; im : double}

let c = {re = 0.0; im = 0.0}
Console.WriteLine("{0}+{1}i", c.re, c.im)
//type MyComplex =
//  {re: double;
//   im: double;}
//val c : MyComplex = {re = 0.0;
//                     im = 0.0;}
//val it : unit = ()

open System
open System.Threading

let t1 = Thread(fun () -> while true do printf "Thread 1\n")
let t2 = Thread(fun () -> while true do printf "Thread 2\n")

t1.Start(); t2.Start()
//val t1 : Threading.Thread
//val t2 : Threading.Thread

open System
open System.Windows.Forms

let f = new Form(Text = "Hello world")
let b = new Button(Text = "Click me!", Dock = DockStyle.Fill)

b.Click.Add(fun _ ->
    b.Text <- "Click me again"
    MessageBox.Show("Hello world") |> ignore)

f.Controls.Add(b)
f.Show()
Application.Run(f)

open System
open System.Windows.Forms
open System.Drawing

let f = new Form(Text = "Hello world")

f.Paint.Add(fun args ->
    let g = args.Graphics

    for i = 0 to f.Width / 10 do
        g.DrawLine(Pens.Black, i * 10, 0, i * 10, f.Height))

f.Show()
Application.Run(f)

type APoint(angle, radius) =
    member x.Angle = angle
    member x.Radius = radius
    new() = APoint(angle = 0.0, radius = 0.0)
//type APoint =
//  class
//    new : unit -> APoint
//    new : angle:float * radius:float -> APoint
//    member Angle : float
//    member Radius : float
//  end

> let p = APoint();;
//val p : APoint

> p.GetType();;
//val it : System.Type =
//  FSI_0004+APoint
//    {Assembly = FSI-ASSEMBLY, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null;
//     AssemblyQualifiedName = "FSI_0004+APoint, FSI-ASSEMBLY, Version=0.0.0.0, ...}

type APoint(angle, radius) =
    member x.Angle = angle
    member x.Radius = radius
    member x.Stretch (k : double) = APoint(angle = x.Angle, radius = x.Radius + k)
    new() = APoint(angle = 0.0, radius = 0.0)
//type APoint =
//  class
//    new : unit -> APoint
//    new : angle:float * radius:float -> APoint
//    member Stretch : k:double -> APoint
//    member Angle : float
//    member Radius : float
//  end

> p.Stretch(22.0);;
//error FS0039: The field, constructor or member 'Stretch' is not defined

> let p2 = APoint();;
//val p2 : APoint

> p2.GetType();;
//val it : System.Type =
//  FSI_0007+APoint
//    {Assembly = FSI-ASSEMBLY, Version=0.0.0.0, Culture=neutral, PublicKeyToken=null;
//     AssemblyQualifiedName = "FSI_0007+APoint, FSI-ASSEMBLY, Version=0.0.0.0, ...}

#r @"EnvDTE.dll"
#r @"EnvDTE80.dll"
open System.Runtime.InteropServices
let appObj = Marshal.GetActiveObject("VisualStudio.DTE") :?> EnvDTE80.DTE2
printfn "%s" (appObj.ActiveDocument.FullName)

#r @".\..\19Debugging\packages\NUnit.2.6.0.12054\lib\nunit.framework.dll"
open System
open NUnit.Framework

[<TestFixture>]
type Test() =

    let posTests(strings) =
        for s in strings do
            Assert.That(isPalindrome s, Is.True,
                          sprintf "isPalindrome(\"%s\") must return true" s)

    let negTests(strings) =
        for s in strings do
            Assert.That(isPalindrome s, Is.False,
                           sprintf "isPalindrome(\"%s\") must return false" s)

    [<Test>]
    member x.EmptyString () =
        Assert.That(isPalindrome(""), Is.True,
                      "isPalindrome must return true on an empty string")

    [<Test>]
    member x.SingleChar () = posTests ["a"]

    [<Test>]
    member x.EvenPalindrome () = posTests ["aa"; "abba"; "abaaba"]

    [<Test>]
    member x.OddPalindrome () = posTests ["aba"; "abbba"; "abababa"]

    [<Test>]
    member x.WrongString () = negTests ["as"; "F# is wonderful"; "Nice"]
//type Test =
//  class
//    new : unit -> Test
//    member EmptyString : unit -> unit
//    member EvenPalindrome : unit -> unit
//    member OddPalindrome : unit -> unit
//    member SingleChar : unit -> unit
//    member WrongString : unit -> unit
//  end

open System
open NUnit.Framework

[<TestFixture;
  Description("Test fixture for the isPalindrome function")>]
type Test() =
    [<TestFixtureSetUp>]
    member x.InitTestFixture () =
        printfn "Before running Fixture"

    [<TestFixtureTearDown>]
    member x.DoneTestFixture () =
        printfn "After running Fixture"

    [<SetUp>]
    member x.InitTest () =
        printfn "Before running test"

    [<TearDown>]
    member x.DoneTest () =
        Console.WriteLine("After running test")

    [<Test;
      Category("Special case");
      Description("An empty string is palindrome")>]
    member x.EmptyString () =
        Assert.That(isPalindrome(""), Is.True,
                      "isPalindrome must return true on an empty string")
//type Test =
//  class
//    new : unit -> Test
//    member DoneTest : unit -> unit
//    member DoneTestFixture : unit -> unit
//    member EmptyString : unit -> unit
//    member InitTest : unit -> unit
//    member InitTestFixture : unit -> unit
//  end
