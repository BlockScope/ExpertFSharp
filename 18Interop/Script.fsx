#I @"C:\Program Files\Reference Assemblies\Microsoft\Framework\v3.5";;
//--> Added 'C:\Program Files\Reference Assemblies\Microsoft\Framework\v3.5' to library include path

#r "System.Core.dll";;

//Consider the following simple F# program in the Program.fs source file:
open System
let i = 2
Console.WriteLine("Input a number:")
let v = Int32.Parse(Console.ReadLine())
Console.WriteLine(i * v)
//The F# compiler generates an executable that can be disassembled using the ildasm.exe tool
//If you open the main@ method, you find the following code, which is annotated here with the corresponding F# statements:
//.method public static void  main@() cil managed
//{
//  .entrypoint
//  // Code size       38 (0x26)
//  .maxstack  4
//
//   // Console.WriteLine("Input a number:")
//  IL_0000:  ldstr      "Input a number:"
//  IL_0005:  call       void [mscorlib]System.Console::WriteLine(string)
//
//  // let v = Int32.Parse(Console.ReadLine())
//  IL_000a:  call       string [mscorlib]System.Console::ReadLine()
//  IL_000f:  call       int32 [mscorlib]System.Int32::Parse(string)
//  IL_0014:  stsfld     int32 '<StartupCode$ConsoleApplication1>'.$Program::v@4
//
//  // Console.WriteLine(i * v) // Note that i is constant and its value has been inlined
//  IL_0019:  ldc.i4.2
//  IL_001a:  call       int32 Program::get_v()
//  IL_001f:  mul
//  IL_0020:  call       void [mscorlib]System.Console::WriteLine(int32)
//
//// Exits
//  IL_0025:  ret
//} // end of method $Program$Main::main@

> open System;;
> let o = Activator.CreateInstance(Type.GetTypeFromProgID("InternetExplorer.Application"));;
//val o : obj

> let t = o.GetType();;
//val t : Type = System.__ComObject

> t.GetProperty("Visible").SetValue(o, (true :> Object), null);;

> let m = t.GetMethod("Quit");;
//val m : Reflection.MethodInfo

> m.GetParameters().Length;;
//val it : int = 3

> m.GetParameters();;
//val it : ParameterInfo []
//       = [|System.Object& SaveChanges
//             {Attributes = In, Optional, HasFieldMarshal;
//              DefaultValue = System.Reflection.Missing;
//              IsIn = true;
//              IsLcid = false;
//              IsOptional = true;
//              IsOut = false;
//              IsRetval = false;
//              Member =
//                Void Quit(System.Object ByRef,
//                            System.Object ByRef, System.Object ByRef);
//              MetadataToken = 134223449;
//              Name = "SaveChanges";
//              ParameterType = System.Object&;
//              Position = 0;
//              RawDefaultValue = System.Reflection.Missing;};
//           ... more ... |]

> m.Invoke(o, [| null; null; null |]);;
//val it : obj = null

//<OBJECT
//    classid="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
//    codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab"
//    title="My movie" width="640" height="480">
//    <param name="movie" value="MyMovie.swf" />
//    <param name="quality" value="high" />
//</OBJECT>

//c:\>aximp c:\Windows\System32\Macromed\Flash\Flash64_11_4_402_265.ocx
//Microsoft (R) .NET ActiveX Control to Windows Forms Assembly Generator
//[Microsoft .Net Framework, Version 4.0.30319.17929]
//Copyright (c) Microsoft Corporation.  All rights reserved.
//
//
//AxImp Error: Unable to locate input ActiveX library: 'c:\Windows\System32\Macromed\Flash\Flash64_11_4_402_265.ocx'.
//
//c:\>dir c:\Windows\System32\Macromed\Flash\Flash64_11_4_402_265.ocx
// Volume in drive C has no label.
// Volume Serial Number is 64D6-DB09
//
// Directory of c:\Windows\System32\Macromed\Flash
//
//09/09/2012  08:22 PM        12,994,248 Flash64_11_4_402_265.ocx
//               1 File(s)     12,994,248 bytes
//               0 Dir(s)  60,087,279,616 bytes free
//
//c:\>copy c:\Windows\System32\Macromed\Flash\Flash64_11_4_402_265.ocx
//        1 file(s) copied.
//
//c:\>aximp Flash64_11_4_402_265.ocx
//Generated Assembly: c:\ShockwaveFlashObjects.dll
//Generated Assembly: c:\AxShockwaveFlashObjects.dll

#I @"c:\";;
//--> Added 'c:\ ' to library include path

#r "AxShockwaveFlashObjects.dll";;
//--> Referenced 'c:\AxShockwaveFlashObjects.dll'

> open AxShockwaveFlashObjects;;
> open System.Windows.Forms;;

> let f = new Form();;
//val f : Form = System.Windows.Forms.Form, Text: 

> let flash = new AxShockwaveFlash();;
//val flash : AxShockwaveFlash = AxShockwaveFlashObjects.AxShockwaveFlash

> f.Show();;
> flash.Dock <- DockStyle.Fill;;
> f.Controls.Add(flash);;
> flash.LoadMovie(0, "http://laptop.org/img/meshDemo18.swf");;

// Reset F# interactive ...
#r "EnvDTE80"
open System.Runtime.InteropServices

let appObj = Marshal.GetActiveObject("VisualStudio.DTE.11.0") :?> EnvDTE80.DTE2
printfn "%s" appObj.ActiveDocument.FullName
//--> Referenced 'c:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE80.dll'
//val appObj : EnvDTE80.DTE2
//C:\dev\apress\f-3.0code\18Interop\Script.fsx
//val it : unit = ()

// Reset F# interactive ...
#r "EnvDTE"

open System.Runtime.InteropServices
open System.Runtime.InteropServices.ComTypes

[<DllImport("ole32.dll")>]
extern int internal GetRunningObjectTable(uint32 reserved, IRunningObjectTable& pprot)

[<DllImport("ole32.dll")>]
extern int internal CreateBindCtx(uint32 reserved, IBindCtx& pctx)

let FetchVSDTE (pid : int) =
    let mutable prot : IRunningObjectTable = null 
    let mutable pmonkenum : IEnumMoniker = null
    let (monikers : IMoniker[]) =  Array.create 1 null
    let pfeteched = System.IntPtr.Zero
    let mutable (ret  :obj) = null
    let endpid = sprintf ":%d" pid
  
    try
        if (GetRunningObjectTable(0u, &prot) <> 0) || (prot = null) then 
            failwith "Error opening the ROT"
        prot.EnumRunning(&pmonkenum)
        pmonkenum.Reset()
        while pmonkenum.Next(1, monikers, pfeteched) = 0 do
            let mutable (insname : string) = null
            let mutable (pctx : IBindCtx) = null
            CreateBindCtx(0u, &pctx) |> ignore
            (monikers.[0]).GetDisplayName(pctx, null, &insname);
            Marshal.ReleaseComObject(pctx) |> ignore
            if insname.StartsWith("!VisualStudio.DTE") && insname.EndsWith(endpid) then
                prot.GetObject(monikers.[0], &ret) |> ignore
    finally
        if prot <> null then Marshal.ReleaseComObject(prot) |> ignore
        if pmonkenum <> null then Marshal.ReleaseComObject(pmonkenum) |> ignore
    (ret :?> EnvDTE.DTE)
//--> Referenced 'c:\Program Files (x86)\Microsoft Visual Studio 10.0\Common7\IDE\PublicAssemblies\EnvDTE.dll'
//val GetRunningObjectTable :
//  reserved:uint32 *
//  pprot:byref<System.Runtime.InteropServices.ComTypes.IRunningObjectTable> ->
//    int
//val CreateBindCtx :
//  reserved:uint32 *
//  pctx:byref<System.Runtime.InteropServices.ComTypes.IBindCtx> -> int
//val FetchVSDTE : pid:int -> EnvDTE.DTE

// Reset F# interactive ...
#I @"CInteropDLL\Debug"
//--> Added 'C:\dev\apress\f-3.0code\18Interop\CInteropDLL\Debug' to library include path

open System.Runtime.InteropServices

module CInterop =
    [<DllImport("CInteropDLL", CallingConvention = CallingConvention.Cdecl)>]
    extern void HelloWorld()
//module CInterop = begin
//  val HelloWorld : unit -> unit
//end

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"\CInteropDLL\Debug"
CInterop.HelloWorld()

// The above doesn't print to the F# interactive window when run in VS2012 so here it is
// running in fsi from the command line ...
//> #I @"CInteropDLL\Debug"
//- open System.Runtime.InteropServices
//-
//- module CInterop =
//-     [<DllImport("CInteropDLL", CallingConvention = CallingConvention.Cdecl)>]
//-     extern void HelloWorld()
//-
//- System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"\CInteropDLL\Debug"
//- CInterop.HelloWorld();;
//
//--> Added 'c:\dev\apress\f-3.0code\18Interop\CInteropDLL\Debug' to library include path
//
//Hello C world invoked by F#!
//
//module CInterop = begin
//  val HelloWorld : unit -> unit
//end

// Reset F# interactive ...
#I @"CInteropDLL\Debug"
open System.Runtime.InteropServices
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"\CInteropDLL\Debug"

module CInterop =
    [<DllImport("CInteropDLL", CallingConvention = CallingConvention.Cdecl)>]
    extern int Sum(int i, int j)
//module CInterop = begin
//  val Sum : i:int * j:int -> int
//end
//val it : unit = ()

printf "Sum(1, 1) = %d\n" (CInterop.Sum(1, 1));
//Sum(1, 1) = 2
//val it : unit = ()

// Reset F# interactive ...
#I @"CInteropDLL\Debug"
open System.Runtime.InteropServices
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"\CInteropDLL\Debug"

module CInterop =
    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type Complex =
        val mutable re : double
        val mutable im : double

        new(r, i) = {re = r; im = i}

    [<DllImport("CInteropDLL")>]
    extern Complex SumC(Complex c1, Complex c2)

let c1 = CInterop.Complex(1.0, 0.0)
let c2 = CInterop.Complex(0.0, 1.0)

let mutable c3 = CInterop.SumC(c1, c2)
printf "c3 = SumC(c1, c2) = %f + %fi\n" c3.re c3.im
//warning FS0009: Uses of this construct may result in the generation of unverifiable .NET IL code. This warning can be disabled using '--nowarn:9' or '#nowarn "9"'.
//
//module CInterop = begin
//  type Complex =
//    struct
//      new : r:double * i:double -> Complex
//      val mutable re: double
//      val mutable im: double
//    end
//  val SumC : c1:Complex * c2:Complex -> Complex
//end
//
//c3 = SumC(c1, c2) = 1.000000 + 1.000000i
//val it : unit = ()

//struct Foo {
//    int i;
//    char c;
//    short s;
//};

// Reset F# interactive ...
#I @"CInteropDLL\Debug"
open System.Runtime.InteropServices
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"\CInteropDLL\Debug"

module CInterop =
    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type Complex =
        val mutable re : double
        val mutable im : double

        new(r, i) = {re = r; im = i}

    [<DllImport("CInteropDLL")>]
    extern Complex SumC(Complex c1, Complex c2)

    [<DllImport("CInteropDLL")>]
    extern void ZeroC(Complex* c)
//warning FS0009: Uses of this construct may result in the generation of unverifiable .NET IL code. This warning can be disabled using '--nowarn:9' or '#nowarn "9"'.
//
//module CInterop = begin
//  type Complex =
//    struct
//      new : r:double * i:double -> Complex
//      val mutable re: double
//      val mutable im: double
//    end
//  val SumC : c1:Complex * c2:Complex -> Complex
//  val ZeroC : c:nativeptr<Complex> -> unit
//end

let c1 = CInterop.Complex(1.0, 0.0)
let c2 = CInterop.Complex(0.0, 1.0)
//val c1 : CInterop.Complex = FSI_0005+CInterop+Complex
//val c2 : CInterop.Complex = FSI_0005+CInterop+Complex

let mutable c4 = CInterop.SumC(c1, c2)
//val mutable c4 : CInterop.Complex = FSI_0005+CInterop+Complex

printf "c4 = SumC(c1, c2) = %f + %fi\n" c4.re c4.im
//c4 = SumC(c1, c2) = 1.000000 + 1.000000i

CInterop.ZeroC(&&c4)
//warning FS0051: The use of native pointers may result in unverifiable .NET IL code

printf "c4 = %f + %fi\n" c4.re c4.im
//c4 = 0.000000 + 0.000000i

// Reset F# interactive ...
#I @"CInteropDLL\Debug"
open System.Runtime.InteropServices
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"\CInteropDLL\Debug"

module CInterop =
    [<StructLayout(LayoutKind.Sequential)>]
    type ObjComplex =
        val mutable re : double
        val mutable im : double

        new() = {re = 0.0; im = 0.0}
        new(r : double, i : double) = {re = r; im = i}

     [<DllImport("CInteropDLL", EntryPoint = "ZeroC")>]
    extern void ObjZeroC(ObjComplex c)

let oc = CInterop.ObjComplex(2.0, 1.0)
printf "oc = %f + %fi\n" oc.re oc.im
CInterop.ObjZeroC(oc)
printf "oc = %f + %fi\n" oc.re oc.im
//warning FS0009: Uses of this construct may result in the generation of unverifiable .NET IL code. This warning can be disabled using '--nowarn:9' or '#nowarn "9"'.
//module CInterop = begin
//  type ObjComplex =
//    class
//      new : unit -> ObjComplex
//      new : r:double * i:double -> ObjComplex
//      val mutable re: double
//      val mutable im: double
//    end
//  val ObjZeroC : c:ObjComplex -> unit
//end
//val oc : CInterop.ObjComplex
//oc = 2.000000 + 1.000000i
//oc = 0.000000 + 0.000000i


// Reset F# interactive ...
#I @"CInteropDLL\Debug"
open System.Runtime.InteropServices
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"\CInteropDLL\Debug"

[<DllImport("CInteropDLL", CallingConvention = CallingConvention.Cdecl)>]
extern void echo(string s)
//val echo : s:string -> unit
echo "abc"

// The above doesn't print to the F# interactive window, so here it is run from
// the fsi command line ...
//> #I @"CInteropDLL\Debug"
//- open System.Runtime.InteropServices
//- System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"\CInteropDLL\Debug"
//-
//- [<DllImport("CInteropDLL", CallingConvention = CallingConvention.Cdecl)>]
//- extern void echo(string s)
//- echo "abc"
//- ;;
//
//--> Added 'c:\dev\apress\f-3.0code\18Interop\CInteropDLL\Debug' to library include path
//
//abc

// Reset F# interactive ...
#I @"CInteropDLL\Debug"
open System.Runtime.InteropServices
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"\CInteropDLL\Debug"

[<DllImport("CInteropDLL", CallingConvention = CallingConvention.Cdecl)>]
extern void sayhello(System.Text.StringBuilder sb, int sz)

let sb = new System.Text.StringBuilder(50)
sayhello(sb, 50)
printf "%s\n" (sb.ToString())
//--> Added 'C:\dev\apress\f-3.0code\18Interop\CInteropDLL\Debug' to library include path
//
//Hello from C code!
//
//val sayhello : sb:System.Text.StringBuilder * sz:int -> unit
//val sb : System.Text.StringBuilder = Hello from C code!
//val it : unit = ()

// Reset F# interactive ...
#I @"CInteropDLL\Debug"
open System.Runtime.InteropServices
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"\CInteropDLL\Debug"

open System.Text
[<DllImport("CInteropDLL", CallingConvention = CallingConvention.Cdecl)>]
extern void sayhellow([<MarshalAs(UnmanagedType.LPWStr)>]StringBuilder sb, int sz)
//--> Added 'C:\dev\apress\f-3.0code\18Interop\CInteropDLL\Debug' to library include path
//
//val sayhellow : sb:System.Text.StringBuilder * sz:int -> unit

// Reset F# interactive ...
#I @"CInteropDLL\Debug"
open System.Runtime.InteropServices
System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__ + @"\CInteropDLL\Debug"

type Callback = delegate of int -> int

[<DllImport("CInteropDLL", CallingConvention = CallingConvention.Cdecl)>]
extern void transformArray(int[] data, int count, Callback transform)

open System

let anyToString any = sprintf "%A" any
let data = [|1; 2; 3|]
printf "%s\n" (String.Join("; ", (Array.map anyToString data)))

transformArray(data, data.Length, new Callback(fun x -> x + 1))
printf "%s\n" (String.Join("; ", (Array.map anyToString data)))
//--> Added 'C:\dev\apress\f-3.0code\18Interop\CInteropDLL\Debug' to library include path
//
//1; 2; 3
//2; 3; 4
//
//type Callback =
//  delegate of int -> int
//val transformArray : data:int [] * count:int * transform:Callback -> unit
//val anyToString : any:'a -> string
//val data : int [] = [|2; 3; 4|]
//val it : unit = ()

//struct __db {
//        /* ... */
//        DB_ENV *dbenv;              /* Backing environment. */
//        DBTYPE type;               /* DB access method type. */
//        /* ... */
//        int  (*close) __P((DB *, u_int32_t));
//        int  (*cursor) __P((DB *, DB_TXN *, DBC **, u_int32_t));
//        int  (*del) __P((DB *, DB_TXN *, DBT *, u_int32_t));
//        // ...
//}

DB *db;
// BDB call
db->close(db, 0);
// Wrapper call
db_close(db, 0);
