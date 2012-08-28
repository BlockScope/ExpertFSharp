#I @"C:\Program Files\Reference Assemblies\Microsoft\Framework\v3.5";;
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
> let o = Activator.CreateInstance(Type.GetTypeFromProgID("Word.Application"));;
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
//      classid ="clsid:D27CDB6E-AE6D-11cf-96B8-444553540000"
//      codebase="http://download.macromedia.com/pub/shockwave/cabs/flash/swflash.cab"
//      width   ="640" height="480"
//      title   ="My movie">
//   <param name="movie"   value="MyMovie.swf" />
//   <param name="quality" value="high" />
//</OBJECT>


C:\> aximp c:\Windows\System32\Macromed\Flash\Flash32_11_3_370_178.ocx
//Generated Assembly: C:\ShockwaveFlashObjects.dll
//Generated Assembly: C:\AxShockwaveFlashObjects.dll

#I @"c:\";;
//--> Added 'c:\ ' to library include path

#r "AxShockwaveFlashObjects.dll";;
//--> Referenced 'c:\AxShockwaveFlashObjects.dll'

> open AxShockwaveFlashObjects;;

> open System.Windows.Forms;;

> let f = new Form();;
//val f : Form

> let flash = new AxShockwaveFlash();;
//val flash : AxShockwaveFlash
//Binding session to 'c:\AxShockwaveFlashObjects.dll'...

> f.Show();;

> flash.Dock <- DockStyle.Fill;;

> f.Controls.Add(flash);;

> flash.LoadMovie(0, "http://laptop.org/img/meshDemo18.swf");;

#r "EnvDTE"
open System.Runtime.InteropServices
let appObj =Marshal.GetActiveObject("VisualStudio.DTE") :?> EnvDTE80.DTE2
printfn "%s" appObj.ActiveDocument.FullName

#r "EnvDTE"
open System.Runtime.InteropServices
open System.Runtime.InteropServices.ComTypes

[<DllImport("ole32.dll")>]
extern int internal GetRunningObjectTable(uint32 reserved, IRunningObjectTable& pprot)

[<DllImport("ole32.dll")>]
extern int internal CreateBindCtx(uint32 reserved, IBindCtx& pctx)

let FetchVSDTE (pid:int) =
  let mutable prot : IRunningObjectTable = null 
  let mutable pmonkenum : IEnumMoniker = null
  let (monikers:IMoniker[]) =  Array.create 1 null
  let pfeteched = System.IntPtr.Zero
  let mutable (ret:obj) = null
  let endpid = sprintf ":%d" pid
  
  try
    if (GetRunningObjectTable(0u, &prot) <> 0) || (prot = null) then 
        failwith "Error opening the ROT"
    prot.EnumRunning(&pmonkenum)
    pmonkenum.Reset()
    while pmonkenum.Next(1, monikers, pfeteched) = 0 do
      let mutable (insname:string) = null
      let mutable (pctx:IBindCtx) = null
      CreateBindCtx(0u, &pctx) |> ignore
      (monikers.[0]).GetDisplayName(pctx, null, &insname);
      Marshal.ReleaseComObject(pctx) |> ignore
      if insname.StartsWith("!VisualStudio.DTE") && insname.EndsWith(endpid) then
        prot.GetObject(monikers.[0], &ret) |> ignore
  finally
    if prot <> null then Marshal.ReleaseComObject(prot) |> ignore
    if pmonkenum <> null then Marshal.ReleaseComObject(pmonkenum) |> ignore
  (ret :?> EnvDTE.DTE)

//#define CINTEROPDLL_API __declspec(dllexport)
//extern "C" {
//void CINTEROPDLL_API HelloWorld();
//}

//void CINTEROPDLL_API HelloWorld()
//{
//    printf("Hello C world invoked by F#!\n");
//}

open System.Runtime.InteropServices

module CInterop =
    [<DllImport("CInteropDLL", CallingConvention=CallingConvention.Cdecl)>]
    extern void HelloWorld()

CInterop.HelloWorld()

//int CINTEROPDLL_API Sum(int i, int j)
//{
//    return i + j;
//}

module CInterop =
    [<DllImport("CInteropDLL", CallingConvention=CallingConvention.Cdecl)>]
    extern int Sum(int i, int j)

printf "Sum(1, 1) = %d\n" (CInterop.Sum(1, 1));

//typedef struct _Complex {
//    double re;
//    double im;
//} Complex;

//Complex CINTEROPDLL_API SumC(Complex c1, Complex c2)
//{
//    Complex ret;
//    ret.re = c1.re + c2.re;
//    ret.im = c1.im + c2.im;
//    return ret;
//}

module CInterop =
    [<Struct; StructLayout(LayoutKind.Sequential)>]
    type Complex =
        val mutable re:double
        val mutable im:double

        new(r,i) = { re = r; im = i; }

    [<DllImport("CInteropDLL")>]
    extern Complex SumC(Complex c1, Complex c2)

let c1 = CInterop.Complex(1.0, 0.0)
let c2 = CInterop.Complex(0.0, 1.0)

let mutable c3 = CInterop.SumC(c1, c2)
printf "c3 = SumC(c1, c2) = %f + %fi\n" c3.re c3.im;

//struct Foo {
//    int i;
//    char c;
//    short s;
//};

//void CINTEROPDLL_API ZeroC(Complex* c)
//{
//    c->re = 0;
//    c->im = 0;
//}

[<DllImport("CInteropDLL")>]
extern void ZeroC(Complex* c)

let mutable c4 = CInterop.SumC(c1, c2)
printf "c4 = SumC(c1, c2) = %f + %fi\n" c4.re c4.im

CInterop.ZeroC(&&c4)
printf "c4 = %f + %fi\n" c4.re c4.im

module CInterop =
    [<StructLayout(LayoutKind.Sequential)>]
    type ObjComplex =
        val mutable re:double
        val mutable im:double

        new() as x = { re = 0.0; im = 0.0 }
        new(r:double, i:double) as x = { re = r; im = i }

     [<DllImport("CInteropDLL", EntryPoint="ZeroC")>]
    extern void ObjZeroC(ObjComplex c)

let oc = CInterop.ObjComplex(2.0, 1.0)
printf "oc = %f + %fi\n" oc.re oc.im
CInterop.ObjZeroC(oc)
printf "oc = %f + %fi\n" oc.re oc.im

void CINTEROPDLL_API echo(char* str)
{
    puts(str);
}

[<DllImport("CInteropDLL", CallingConvention=CallingConvention.Cdecl)>]
extern void echo(string s);

//void CINTEROPDLL_API sayhello(char* str, int sz)
//{
//    static char* data = "Hello from C code!";
//    int len = min(sz, strlen(data));
//    strncpy(str, data, len);
//    str[len] = 0;
//}

[<DllImport("CInteropDLL", CallingConvention=CallingConvention.Cdecl)>]
extern void sayhello(StringBuilder sb, int sz);

let sb = new StringBuilder(50)

CInterop.sayhello(sb, 50)
printf "%s\n" (sb.ToString())

//void CINTEROPDLL_API sayhellow(wchar_t* str, int sz)
//{
//    static wchar_t* data = L"Hello from C code Wide!";
//    int len = min(sz, wcslen(data));
//    wcsncpy(str, data, len);
//    str[len] = 0;
//}

[<DllImport("CInteropDLL", CallingConvention=CallingConvention.Cdecl)>]
extern void sayhellow([<MarshalAs(UnmanagedType.LPWStr)>]StringBuilder sb, int sz);

typedef int (CALLBACK *TRANSFORM_CALLBACK)(int);

//void CINTEROPDLL_API transformArray(int* data, int count, TRANSFORM_CALLBACK fn)
//{
//    int i;
//    for (i = 0; i < count; i++)
//        data[i] = fn(data[i]);
//}

type Callback = delegate of int -> int

[<DllImport("CInteropDLL", CallingConvention=CallingConvention.Cdecl)>]
extern void transformArray(int[] data, int count, Callback transform);

let data = [| 1; 2; 3 |]
printf "%s\n" (string.Join("; ", (Array.map any_to_string data)))

CInterop.transformArray(data, data.Length, new CInterop.Callback(fun x -> x + 1))
printf "%s\n" (string.Join("; ", (Array.map any_to_string data)))

module MMap =

    open System
    open System.IO
    open System.Runtime.InteropServices
    open Microsoft.FSharp.NativeInterop
    open Printf

    type HANDLE = nativeint
    type ADDR   = nativeint

    [<DllImport("kernel32", SetLastError=true)>]
    extern bool CloseHandle(HANDLE handler)

    [<DllImport("kernel32", SetLastError=true, CharSet=CharSet.Auto)>]
    extern HANDLE CreateFile(string lpFileName,
                             int dwDesiredAccess,
                             int dwShareMode,
                             HANDLE lpSecurityAttributes,
                             int dwCreationDisposition,
                             int dwFlagsAndAttributes,
                             HANDLE hTemplateFile)

    [<DllImport("kernel32", SetLastError=true, CharSet=CharSet.Auto)>]
    extern HANDLE CreateFileMapping(HANDLE hFile,
                                    HANDLE lpAttributes,
                                    int flProtect,
                                    int dwMaximumSizeLow,
                                    int dwMaximumSizeHigh,
                                    string lpName)

    [<DllImport("kernel32", SetLastError=true)>]
    extern ADDR MapViewOfFile(HANDLE hFileMappingObject,
                              int dwDesiredAccess,
                              int dwFileOffsetHigh,
                              int dwFileOffsetLow,
                              int dwNumBytesToMap)

    [<DllImport("kernel32", SetLastError=true, CharSet=CharSet.Auto)>]
    extern HANDLE OpenFileMapping(int dwDesiredAccess,
                                  bool bInheritHandle,
                                  string lpName)

    [<DllImport("kernel32", SetLastError=true)>]
    extern bool UnmapViewOfFile(ADDR lpBaseAddress)

    let INVALID_HANDLE = new IntPtr(-1)
    let MAP_READ    = 0x0004
    let GENERIC_READ = 0x80000000
    let NULL_HANDLE = IntPtr.Zero
    let FILE_SHARE_NONE = 0x0000
    let FILE_SHARE_READ = 0x0001
    let FILE_SHARE_WRITE = 0x0002
    let FILE_SHARE_READ_WRITE = 0x0003
    let CREATE_ALWAYS  = 0x0002
    let OPEN_EXISTING   = 0x0003
    let OPEN_ALWAYS  = 0x0004
    let READONLY = 0x00000002

    type MemMap<'a> (fileName) =

        let ok =
            match typeof<'a>) with
            | ty when ty = typeof<int>)     -> true
            | ty when ty = typeof<int32>)   -> true
            | ty when ty = typeof<byte>)    -> true
            | ty when ty = typeof<sbyte>)   -> true
            | ty when ty = typeof<int16>)   -> true
            | ty when ty = typeof<uint16>)  -> true
            | ty when ty = typeof<int64>)   -> true
            | ty when ty = typeof<uint64>)  -> true
            | _ -> false

        do if not ok then failwithf 
           "the type %s is not a basic blittable type" ((typeof<'a>).ToString())
        let hFile =
           CreateFile (fileName,
                         GENERIC_READ,
                         FILE_SHARE_READ_WRITE,
                         IntPtr.Zero, OPEN_EXISTING, 0, IntPtr.Zero  )
        do if ( hFile.Equals(INVALID_HANDLE) ) then
            Marshal.ThrowExceptionForHR(Marshal.GetHRForLastWin32Error());
        let hMap = CreateFileMapping (hFile, IntPtr.Zero, READONLY, 0,0, null )
        do CloseHandle(hFile) |> ignore
        do if hMap.Equals(NULL_HANDLE) then
            Marshal.ThrowExceptionForHR(Marshal.GetHRForLastWin32Error());

        let start = MapViewOfFile (hMap, MAP_READ,0,0,0)

        do  if ( start.Equals(IntPtr.Zero) ) then
             Marshal.ThrowExceptionForHR(
                  Marshal.GetHRForLastWin32Error())


        member m.AddressOf(i: int) : 'a nativeptr  =
             NativePtr.of_nativeint(start + (nativeint i))

        member m.GetBaseAddress (i:int) : int -> 'a =
            NativePtr.get (m.AddressOf(i))

        member m.Item
            with get(i : int) : 'a = m.GetBaseAddress 0 i

        member m.Close() =
           UnmapViewOfFile(start) |> ignore;
           CloseHandle(hMap) |> ignore

        interface IDisposable with
          member m.Dispose() =
             m.Close()

let mm = new MMap.MemMap<byte>("somefile.txt")

printf "%A\n" (mm.[0])

mm.Close()

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
