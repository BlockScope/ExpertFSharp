let generateTicket =
    let count = ref 0
    (fun () -> incr count; !count)
//val generateTicket : (unit -> int)

type IPeekPoke =
    abstract member Peek: unit -> int
    abstract member Poke: int -> unit
//type IPeekPoke =
//  interface
//    abstract member Peek : unit -> int
//    abstract member Poke : int -> unit
//  end

let makeCounter initialState =
    let state = ref initialState
    {new IPeekPoke with
        member x.Poke n = state := !state + n
        member x.Peek() = !state}
//val makeCounter : initialState:int -> IPeekPoke

type TicketGenerator() =
    // Note: let bindings in a type definition are implicitly private to the object
    // being constructed. Members are implicitly public.
    let mutable count = 0

    member x.Next() =
        count <- count + 1;
        count

    member x.Reset () =
        count <- 0
//type TicketGenerator =
//  class
//    new : unit -> TicketGenerator
//    member Next : unit -> int
//    member Reset : unit -> unit
//  end

type IStatistic<'T, 'U> =
    abstract Record : 'T -> unit
    abstract Value : 'U
//type IStatistic<'T,'U> =
//  interface
//    abstract member Record : 'T -> unit
//    abstract member Value : 'U
//  end

let makeAverager(toFloat : 'T -> float) =
    let count = ref 0
    let total = ref 0.0
    {new IStatistic<'T, float> with
        member stat.Record(x) = incr count; total := !total + toFloat x
        member stat.Value = (!total / float !count)}
//val makeAverager : toFloat:('T -> float) -> IStatistic<'T,float>

open System

module public VisitorCredentials =

   /// The internal table of permitted visitors and the
   /// days they are allowed to visit.
   let private  visitorTable =
       dict [("Anna", set [DayOfWeek.Tuesday; DayOfWeek.Wednesday]);
             ("Carolyn", set [DayOfWeek.Friday]) ]

   /// This is the function to check if a person is a permitted visitor.
   /// Note: this is public and can be used by external code
   let public checkVisitor(person) =
       visitorTable.ContainsKey(person) &&
       visitorTable.[person].Contains(DateTime.Today.DayOfWeek)

   /// This is the function to return all known permitted visitors.
   /// Note: this is internal and can only be used by code in this assembly.
   let internal allKnownVisitors() =
       visitorTable.Keys
//module VisitorCredentials = begin
//  val private visitorTable :
//    Collections.Generic.IDictionary<string,Set<DayOfWeek>>
//  val checkVisitor : person:string -> bool
//  val internal allKnownVisitors :
//    unit -> Collections.Generic.ICollection<string>
//end

module public GlobalClock =

    type TickTock = Tick | Tock

    let mutable private clock = Tick

    let private tick = new Event<TickTock>()

    let internal oneTick() =
        (clock <- match clock with Tick -> Tock | Tock -> Tick);
        tick.Trigger (clock)

    let tickEvent = tick.Publish
//module GlobalClock = begin
//  type TickTock =
//    | Tick
//    | Tock
//  val mutable private clock : TickTock = Tick
//  val private tick : Event<TickTock>
//  val internal oneTick : unit -> unit
//  val tickEvent : IEvent<TickTock> = <published event>
//end

module internal TickTockDriver =

    open System.Threading

    let timer = new Timer(callback = (fun _ -> GlobalClock.oneTick()),
                          state = null, dueTime = 0, period = 100)
//module internal TickTockDriver = begin
//  val timer : Threading.Timer
//end

module TickTockListener =
   do GlobalClock.tickEvent.Add(function
       | GlobalClock.Tick -> printf "tick!"
       | GlobalClock.Tock -> System.Windows.Forms.MessageBox.Show "tock!" |> ignore)
//Throws up a never ending series of tocking message boxes!

open System.Collections.Generic

type public SparseVector () =

    let elems = new SortedDictionary<int, float>()

    member internal vec.Add (k, v) = elems.Add(k, v)

    member public vec.Count = elems.Keys.Count
    member vec.Item
        with public get i =
            if elems.ContainsKey(i) then elems.[i]
            else 0.0
        and internal set i v =
            elems.[i] <- v

type Vector2D =
    {DX : float; DY : float}

module Vector2DOps =
    let length v = sqrt (v.DX * v.DX + v.DY * v.DY)
    let scale k v = {DX = k * v.DX; DY = k * v.DY}
    let shiftX x v = {v with DX = v.DX + x}
    let shiftY y v = {v with DY = v.DY + y}
    let shiftXY (x, y) v = {DX = v.DX + x; DY = v.DY + y}
    let zero = {DX = 0.0; DY = 0.0}
    let constX dx = {DX = dx; DY = 0.0}
    let constY dy = {DX = 0.0; DY = dy}

//SEE: 1.fs
//SEE: 2.fs
//SEE: clock.fs

//C:\Users\dsyme\Desktop>fsc -i -a clock.fs
//Microsoft (R) F# 3.0 Compiler build 11.0.50522.1
//Copyright (c) Microsoft Corporation. All Rights Reserved.
//module Clock
//type TickTock =
//  | Tick
//  | Tock
//val ticker : x:TickTock -> TickTock

//SEE: vector.fsi and vector.fs
type Vector2D =
    {DX : float; DY : float}

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector2D =
     let length v = sqrt(v.DX * v.DX + v.DY * v.DY)

[<RequireQualifiedAccess>]
module Vector2D =
     let length v = sqrt(v.DX * v.DX + v.DY * v.DY)
     let zero = {DX = 0.0; DY = 0.0}

open Vector2D 
//error FS0892: This declaration opens the module 'FSI_0003.Vector2D', which is marked as 'RequireQualifiedAccess'. Adjust your code to use qualified references to the elements of the module instead, e.g. 'List.map' instead of 'map'. This change will ensure that your code is robust as new constructs are added to libraries.

//SEE: WidgetWheels.fs
//SEE: AutoOpen.fs

#load "vector.fs"
#load "vector.fsi" "vector.fs"
//#load "charting.fsi" "charting.fs" "charting.fsx"
//#load "matrix.fsi" "matrix.fs" "vector.fsi" "vector.fs"

C:\fsharp> fsc dolphins.fs
C:\fsharp> dir dolphins.exe
//18/06/2012  02:30 p.m.             4,608 dolphin.exe

C:\fsharp> dolphins.exe
//Known Dolphins: ["Delphinus capensis"; "Delphinus delphis"]

C:\test> fsc -g -a whales.fs
C:\test> dir whales.dll
//...
//05/04/2010  19:18             6,656 whales.dll

C:\fsharp> fsc -g -r whales.dll -o watcher.exe whaleWatcher.fs
C:\fsharp> dir watcher.exe
//...
//18/06/2012  02:48 p.m.             5,120 watcher.exe

C:\fsharp> watcher.exe 1
//You spotted ("Blue, Southern Ocean", Blue)!