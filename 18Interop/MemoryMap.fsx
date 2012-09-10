module MMap =
    open System
    open System.IO
    open System.Runtime.InteropServices
    open Microsoft.FSharp.NativeInterop
    open Printf

    type HANDLE = nativeint
    type ADDR = nativeint

    [<DllImport("kernel32", SetLastError = true)>]
    extern bool CloseHandle(HANDLE handler)

    [<DllImport("kernel32", SetLastError = true, CharSet = CharSet.Auto)>]
    extern HANDLE CreateFile(string lpFileName,
                             int dwDesiredAccess,
                             int dwShareMode,
                             HANDLE lpSecurityAttributes,
                             int dwCreationDisposition,
                             int dwFlagsAndAttributes,
                             HANDLE hTemplateFile)

    [<DllImport("kernel32", SetLastError = true, CharSet = CharSet.Auto)>]
    extern HANDLE CreateFileMapping(HANDLE hFile,
                                    HANDLE lpAttributes,
                                    int flProtect,
                                    int dwMaximumSizeLow,
                                    int dwMaximumSizeHigh,
                                    string lpName)

    [<DllImport("kernel32", SetLastError = true)>]
    extern ADDR MapViewOfFile(HANDLE hFileMappingObject,
                              int dwDesiredAccess,
                              int dwFileOffsetHigh,
                              int dwFileOffsetLow,
                              int dwNumBytesToMap)

    [<DllImport("kernel32", SetLastError = true, CharSet = CharSet.Auto)>]
    extern HANDLE OpenFileMapping(int dwDesiredAccess,
                                  bool bInheritHandle,
                                  string lpName)

    [<DllImport("kernel32", SetLastError = true)>]
    extern bool UnmapViewOfFile(ADDR lpBaseAddress)

    let INVALID_HANDLE = new IntPtr(-1)
    let MAP_READ = 0x0004
    let GENERIC_READ = 0x80000000
    let NULL_HANDLE = IntPtr.Zero
    let FILE_SHARE_NONE = 0x0000
    let FILE_SHARE_READ = 0x0001
    let FILE_SHARE_WRITE = 0x0002
    let FILE_SHARE_READ_WRITE = 0x0003
    let CREATE_ALWAYS = 0x0002
    let OPEN_EXISTING = 0x0003
    let OPEN_ALWAYS = 0x0004
    let READONLY = 0x00000002

    type MemMap<'a when 'a : unmanaged> (fileName) =

        let ok =
            match typeof<'a> with
            | ty when ty = typeof<int> -> true
            | ty when ty = typeof<int32> -> true
            | ty when ty = typeof<byte> -> true
            | ty when ty = typeof<sbyte> -> true
            | ty when ty = typeof<int16> -> true
            | ty when ty = typeof<uint16> -> true
            | ty when ty = typeof<int64> -> true
            | ty when ty = typeof<uint64> -> true
            | _ -> false

        do if not ok then failwithf "the type %s is not a basic blittable type" ((typeof<'a>).ToString())
        let hFile =
            CreateFile (fileName,
                        GENERIC_READ,
                        FILE_SHARE_READ_WRITE,
                        IntPtr.Zero, OPEN_EXISTING, 0, IntPtr.Zero  )
        do if (hFile.Equals(INVALID_HANDLE)) then
            Marshal.ThrowExceptionForHR(Marshal.GetHRForLastWin32Error());
        let hMap = CreateFileMapping (hFile, IntPtr.Zero, READONLY, 0, 0, null)
        do CloseHandle(hFile) |> ignore
        do if hMap.Equals(NULL_HANDLE) then
            Marshal.ThrowExceptionForHR(Marshal.GetHRForLastWin32Error());

        let start = MapViewOfFile (hMap, MAP_READ, 0, 0 ,0)

        do if (start.Equals(IntPtr.Zero)) then
            Marshal.ThrowExceptionForHR(Marshal.GetHRForLastWin32Error())

        member m.AddressOf(i : int) : 'a nativeptr =
            NativePtr.ofNativeInt(start + (nativeint i))

        member m.GetBaseAddress (i : int) : int -> 'a =
            NativePtr.get (m.AddressOf(i))

        member m.Item with get(i : int) : 'a = m.GetBaseAddress 0 i

        member m.Close() =
           UnmapViewOfFile(start) |> ignore;
           CloseHandle(hMap) |> ignore

        interface IDisposable with
            member m.Dispose() = m.Close()
//warning FS0009: Uses of this construct may result in the generation of unverifiable .NET IL code. This warning can be disabled using '--nowarn:9' or '#nowarn "9"'.
//warning FS0009: Uses of this construct may result in the generation of unverifiable .NET IL code. This warning can be disabled using '--nowarn:9' or '#nowarn "9"'.
//module MMap = begin
//  type HANDLE = nativeint
//  type ADDR = nativeint
//  val CloseHandle : handler:HANDLE -> bool
//  val CreateFile :
//    lpFileName:string * dwDesiredAccess:int * dwShareMode:int *
//    lpSecurityAttributes:HANDLE * dwCreationDisposition:int *
//    dwFlagsAndAttributes:int * hTemplateFile:HANDLE -> HANDLE
//  val CreateFileMapping :
//    hFile:HANDLE * lpAttributes:HANDLE * flProtect:int * dwMaximumSizeLow:int *
//    dwMaximumSizeHigh:int * lpName:string -> HANDLE
//  val MapViewOfFile :
//    hFileMappingObject:HANDLE * dwDesiredAccess:int * dwFileOffsetHigh:int *
//    dwFileOffsetLow:int * dwNumBytesToMap:int -> ADDR
//  val OpenFileMapping :
//    dwDesiredAccess:int * bInheritHandle:bool * lpName:string -> HANDLE
//  val UnmapViewOfFile : lpBaseAddress:ADDR -> bool
//  val INVALID_HANDLE : System.IntPtr = -1n
//  val MAP_READ : int = 4
//  val GENERIC_READ : int = -2147483648
//  val NULL_HANDLE : nativeint = 0n
//  val FILE_SHARE_NONE : int = 0
//  val FILE_SHARE_READ : int = 1
//  val FILE_SHARE_WRITE : int = 2
//  val FILE_SHARE_READ_WRITE : int = 3
//  val CREATE_ALWAYS : int = 2
//  val OPEN_EXISTING : int = 3
//  val OPEN_ALWAYS : int = 4
//  val READONLY : int = 2
//  type MemMap<'a when 'a : unmanaged> =
//    class
//      interface System.IDisposable
//      new : fileName:string -> MemMap<'a>
//      member AddressOf : i:int -> nativeptr<'a>
//      member Close : unit -> unit
//      member GetBaseAddress : i:int -> (int -> 'a)
//      member Item : i:int -> 'a with get
//    end
//end

System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__
let mm = new MMap.MemMap<byte>("somefile.txt")
//val mm : MMap.MemMap<byte>

printf "%A\n" (mm.[0])
//97uy

mm.Close()
