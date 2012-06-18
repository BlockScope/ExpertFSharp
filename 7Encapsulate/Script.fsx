let generateTicket =
    let count = ref 0
    (fun () -> incr count; !count)

type IPeekPoke =
    abstract member Peek: unit -> int
    abstract member Poke: int -> unit

let makeCounter initialState =
    let state = ref initialState
    { new IPeekPoke with
        member x.Poke n = state := !state + n
        member x.Peek() = !state  }

//val makeCounter : int -> IPeekPoke

type TicketGenerator() =
    // Note: let bindings in a type definition are implicitly private to the object
    // being constructed. Members are implicitly public.
    let mutable count = 0

    member x.Next() =
        count <- count + 1;
        count

    member x.Reset () =
        count <- 0

type IStatistic<'T,'U> =
    abstract Record : 'T -> unit
    abstract Value : 'U

let makeAverager(toFloat: 'T -> float) =
    let count = ref 0
    let total = ref 0.0
    { new IStatistic<'a,float> with
          member stat.Record(x) = incr count; total := !total + toFloat x
          member stat.Value = (!total / float !count) }

type IStatistic<'T,'U> =
    abstract Record : 'T -> unit
    abstract Value: 'U
//val makeAverager : ('T -> float) -> IStatistic<'T,float>
open System

module public VisitorCredentials =

   /// The internal table of permitted visitors and the
   /// days they are allowed to visit.
   let private  visitorTable =
       dict [ ("Anna",    set [DayOfWeek.Tuesday; DayOfWeek.Wednesday]);
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

module public GlobalClock =

    type TickTock = Tick | Tock

    let private mutable clock = Tick

    let private tick = new Event<TickTock>()

    let internal oneTick() =
        (clock <- match clock with Tick -> Tock | Tock -> Tick);
        tick.Trigger (clock)

    let tickEvent = tick.Publish

module internal TickTockDriver =

    open System.Threading

    let timer = new Timer(callback=(fun _ -> GlobalClock.oneTick()),
                          state=null,dueTime=0,period=100)

module TickTockListener =
   do GlobalClock.tickEvent.Add(function
       | GlobalClock.Tick -> printf "tick!"
       | GlobalClock.Tock -> System.Windows.Forms.MessageBox.Show "tock!" |> ignore)

open System.Collections.Generic

type public SparseVector () =

    let elems = new SortedDictionary<int,float>()

    member internal vec.Add (k,v) = elems.Add(k,v)

    member public vec.Count = elems.Keys.Count
    member vec.Item
        with public get i =
            if elems.ContainsKey(i) then elems.[i]
            else 0.0
        and internal set i v =
            elems.[i] <- v

type Vector2D =
    { DX: float; DY: float }

module Vector2DOps =
    let length v = sqrt (v.DX * v.DX + v.DY * v.DY)
    let scale k v = { DX=k*v.DX; DY=k*v.DY }
    let shiftX x v = { v with DX=v.DX+x }
    let shiftY y v = { v with DY=v.DY+y }
    let shiftXY (x,y) v = { DX=v.DX+x; DY=v.DY+y }
    let zero = { DX=0.0; DY=0.0 }
    let constX dx = { DX=dx; DY=0.0 }
    let constY dy = { DX=0.0; DY=dy }

namespace Acme.Widgets

    type Wheel     = Square | Round | Triangle

    type Widget    = { id: int; wheels: Wheel list; size: string }

namespace Acme.Widgets

    type Lever = PlasticLever | WoodenLever

namespace Acme.Suppliers

    type LeverSupplier    = { name: string; leverKind: Acme.Widgets.Lever }

type TickTock = Tick | Tock

let ticker x =
    match x with
    | Tick -> Tock
    | Tock -> Tick

module Clock =
    type TickTock = Tick | Tock
    val ticker : TickTock -> TickTock

// The contents of vector.fsi
namespace Acme.Collections
    type SparseVector =
        new: unit -> SparseVector
        member Add: int * float -> unit
        member Item : int -> float with get


// The contents of vector.fs
namespace Acme.Collections
    open System.Collections.Generic
    type SparseVector() =
        let elems = new SortedDictionary<int,float>()
        member vec.Add(k,v) = elems.Add(k,v)
        member vec.Item
            with get i =
                if elems.ContainsKey(i) then elems.[i]
                else 0.0
            and  set i v =
                elems.[i] <- v

type Vector2D =
    { DX: float; DY: float }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Vector2D =
     let length v = sqrt(v.DX * v.DX + v.DY * v.DY)

[<RequireQualifiedAccess(true)>]
module Vector2D =
     let length v = sqrt(v.DX * v.DX + v.DY * v.DY)
     let zero = { DX=0.0; DY=0.0 }

open Vector2D 
//  Error: This declaration opens the module 'Vector2D', which is 
//  marked as 'RequireQualifiedAccess'. Adjust your code to use qualified 
//  references to the elements of the module instead, e.g. 'List.map' 
//  instead of 'map'. This change will ensure that your code is robust 
//  as new constructs are added to libraries.

module Acme.Widgets.WidgetWheels

let wheelCornerCount = 
    dict [ (Wheel.Square,   4)
           (Wheel.Triangle, 3)
           (Wheel.Round,    0) ]

namespace Acme.Compenents

[<AutoOpen>]
module private Utilities =
    let swap (x,y) = (y,x)

swap (3,4)

[<AutoOpen>]
module NumberTheoryExtensions =
    let private isPrime i =
        let lim = int (sqrt (float i))
        let rec check j =
           j > lim || (i % j <> 0 && check (j+1))
        check 2

    type System.Int32 with
        member i.IsPrime = isPrime i
 
[<assembly: AutoOpen("Acme.NumberTheory")>]
#load "vector.fs"
#load "vector.fsi" "vector.fs"
#load "charting.fsi" "charting.fs" "charting.fsx"
#load "matrix.fsi" "matrix.fs" "vector.fsi" "vector.fs"

//Listing 7-9. File dolphins.fs
let longBeaked = "Delphinus capensis"
let shortBeaked = "Delphinus delphis"
let dolphins = [ longBeaked; shortBeaked ]
printfn "Known Dolphins:  %A" dolphins

C:\fsharp> fsc dolphins.fs

C:\fsharp> dir dolphins.exe
//...
//05/04/2010  19:21             3,124 dolphins.exe

C:\fsharp> dolphins.exe

//Known Dolphins: ["Delphinus capensis"; "Delphinus delphis"]

module Whales.Fictional

/// The three kinds of whales we cover in this release
type WhaleKind =
    | Blue
    | Killer
    | GreatWhale

/// The main whale
let moby = "Moby Dick, Pacific", GreatWhale

/// The backup whale
let bluey = "Blue, Southern Ocean", Blue

/// This whale is for experimental use only
let orca = "Orca, Pacific", Killer

/// The collected whales
let whales = [| moby; bluey; orca |]

C:\test> fsc -g -a whales.fs
C:\test> dir whales.dll
//...
//05/04/2010  19:18             6,656 whales.dll

open Whales
open System

let idx = Int32.Parse(Environment.GetCommandLineArgs().[1])
let spotted = Fictional.whales.[idx]

printfn "You spotted %A!" spotted

C:\fsharp> fsc -g -r whales.dll -o watcher.exe whaleWatcher.fs
C:\fsharp> dir watcher.exe

//...
//05/04/2010  19:25             3,584 watcher.exe

C:\fsharp> watcher.exe 1

//You spotted ("Blue, Southern Ocean", Blue)!