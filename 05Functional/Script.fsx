type index = int
type flags = int64
type results = string * System.TimeSpan * int * int

type Person =
    {Name : string
     DateOfBirth : System.DateTime}

> {Name = "Bill"; DateOfBirth = new System.DateTime(1962, 09, 02)};;
//val it : Person =
//  {Name = "Bill";
//   DateOfBirth = 2/09/1962 12:00:00 a.m. {Date = 2/09/1962 12:00:00 a.m.;
//                                          Day = 2;
//                                          DayOfWeek = Sunday;
//                                          DayOfYear = 245;
//                                          Hour = 0;
//                                          Kind = Unspecified;
//                                          Millisecond = 0;
//                                          Minute = 0;
//                                          Month = 9;
//                                          Second = 0;
//                                          Ticks = 619042176000000000L;
//                                          TimeOfDay = 00:00:00;
//                                          Year = 1962;};}

> ({Name = "Anna"; DateOfBirth = new System.DateTime(1968,07,23)} : Person);;
//val it : Person =
//  {Name = "Anna";
//   DateOfBirth = 23/07/1968 12:00:00 a.m. {Date = 23/07/1968 12:00:00 a.m.;
//                                           Day = 23;
//                                           DayOfWeek = Tuesday;
//                                           DayOfYear = 205;
//                                           Hour = 0;
//                                           Kind = Unspecified;
//                                           Millisecond = 0;
//                                           Minute = 0;
//                                           Month = 7;
//                                           Second = 0;
//                                           Ticks = 620900640000000000L;
//                                           TimeOfDay = 00:00:00;
//                                           Year = 1968;};}

type PageStats =
    {Site : string;
     Time : System.TimeSpan;
     Length : int;
     NumWords : int;
     NumHRefs : int}

open System
open System.IO
open System.Net

//Using the time, http and getWords functions from Chapter 3.
let time f =
    let start = DateTime.Now
    let res = f()
    let finish = DateTime.Now
    (res, finish - start)

let http (url : string) =
    let req = System.Net.WebRequest.Create(url)
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    resp.Close()
    html

let delimiters = [|' '; '\n'; '\t'; '<'; '>'; '='|]
let getWords (s:string) = s.Split delimiters

let stats site =
    let url = "http://" + site
    let html, t = time (fun () -> http url)
    let words = html |> getWords
    let hrefs = words |> Array.filter (fun s -> s = "href")
    {Site = site; Time = t; Length = html.Length;
     NumWords = words.Length; NumHRefs = hrefs.Length}

//val stats  : string -> PageStats

> stats "www.live.com";;
//val it : PageStats = { Site="www.live.com"; Time=0.872623628;
//                       Length=7728, NumWords=1156; NumHRefs=10; }

type Person =
    {Name : string
     DateOfBirth : System.DateTime}

type Company =
    {Name : string
     Address : string}

type Dot = {X : int; Y : int}
type Point = {X : float; Y : float}

> let coords1 (p : Point) = (p.X, p.Y);;
//val coords1 : p:Point -> float * float

> let coords2 (d : Dot) = (d.X, d.Y);;
//val coords2 : d:Dot -> int * int

> let dist p = sqrt (p.X * p.X + p.Y * p.Y);; // use of X and Y implies type "Point"
//val dist : p:Point -> float

type Point3D = {X : float; Y : float; Z : float}
let p1 = {X = 3.0; Y = 4.0; Z = 5.0}
let p2 = {p1 with Y = 0.0; Z = 0.0}

val p1 : Point3D = {X = 3.0;
                    Y = 4.0;
                    Z = 5.0;}

val p2 : Point3D = {X = 3.0;
                    Y = 0.0;
                    Z = 0.0;}

let p2 = {X = 3.0; Y = 0.0; Z = 0.0}

type Route = int
type Make = string
type Model = string
type Transport =
    | Car of Make * Model
    | Bicycle
    | Bus of Route

> let ian = Car("BMW","360");;
//val ian : Transport = Car ("BMW","360")

> let don = [Bicycle; Bus 8];;
//val don  : Transport list = [Bicycle; Bus 8]

> let peter = [Car ("Ford","Fiesta"); Bicycle];;
//val peter  : Transport list = [ Car ("Ford","Fiesta"); Bicycle ];;

let averageSpeed (tr : Transport) =
    match tr with
    | Car _ -> 35
    | Bicycle -> 16
    | Bus _ -> 24

type Proposition =
    | True
    | And of Proposition * Proposition
    | Or of Proposition * Proposition
    | Not of Proposition

let rec eval (p : Proposition) =
    match p with
    | True -> true
    | And(p1,p2) -> eval p1 && eval p2
    | Or (p1,p2) -> eval p1 || eval p2
    | Not(p1) -> not (eval p1)

type 'T option =
    | None
    | Some of 'T

type 'T list =
    | ([])
    | (::) of 'T * 'T list

type Tree<'T> =
    | Tree of 'T * Tree<'T> * Tree<'T>
    | Tip of 'T

let rec sizeOfTree tree =
    match tree with
    | Tree(_, l, r) -> 1 + sizeOfTree l + sizeOfTree r
    | Tip _ -> 1
    
//val sizeOfTree : Tree<'T> -> int

> let smallTree = Tree ("1", Tree ("2", Tip "a", Tip "b"), Tip "c");;
//val smallTree : Tree<string> = Tree ("1",Tree ("2",Tip "a",Tip "b"),Tip "c")

> sizeOfTree smallTree;;
//val it : int = 5

type Point3D = Vector3D of float * float * float

let origin = Vector3D(0., 0., 0.)
let unitX = Vector3D(1., 0., 0.)
let unitY = Vector3D(0., 1., 0.)
let unitZ = Vector3D(0., 0., 1.)

let length (Vector3D(dx, dy, dz)) = sqrt(dx * dx + dy * dy + dz * dz)

type Node =
    {Name : string;
     Links : Link list}
and Link =
    | Dangling
    | Link of Node

type Set<'T> = ...

type StringMap<'T> = Microsoft.FSharp.Collections.Map<string, 'T>

type Projections<'T, 'U> = ('T -> 'U) * ('U -> 'T)
List.map;;
//val it : (('a -> 'b) -> 'a list -> 'b list) = <fun:clo@203>
//val map : ('T -> 'U) -> 'T list -> 'U list

let fetch url = (url, http url);;
//val fetch : url:string -> string * string
//val map<'T,'U> : ('T -> 'U) -> 'T list -> 'U list

let rec map (f : 'T -> 'U) (l : 'T list) =
    match l with
    | h :: t -> f h :: map f t
    | [] -> []

let rec map<'T, 'U> (f : 'T -> 'U) (l : 'T list) =
    match l with
    | h :: t -> f h :: map f t
    | [] -> []

let getFirst (a, b, c) = a
//val getFirst : a:'a * b:'b * c:'c -> 'a

let mapPair f g (x, y) = (f x, g y)
//val mapPair : f:('a -> 'b) -> g:('c -> 'd) -> x:'a * y:'c -> 'b * 'd

compare
(=)
(<)
(<=)
(>)
(>=)
(min)
(max)
val compare : 'T -> 'T -> int when 'T : comparison
val (=) : 'T -> 'T -> bool when 'T : equality
val (<) : 'T -> 'T -> bool when 'T : comparison
val (<=) : 'T -> 'T  -> bool when 'T : comparison
val (>) : 'T  -> 'T  -> bool when 'T : comparison
val (>=) : 'T  -> 'T  -> bool when 'T : comparison
val (min) : 'T  -> 'T  -> 'T  when 'T : comparison
val (max) : 'T  -> 'T  -> 'T  when 'T : comparison

> ("abc", "def") < ("abc", "xyz");;
//val it : bool = true

> compare (10, 30) (10, 20);;
//val it : int = 1

> compare [10; 30] [10; 20];;
//val it : int = 1

> compare [|10; 30|] [|10; 20|];;
//val it : int = 1

> compare [|10; 20|] [|10; 30|];;
//val it : int = -1

hash;;
//val hash : 'T -> int when 'T : equality

> hash 100;;
//val it : int = 100

> hash "abc";;
//val it : int = 536991770

> hash (100, "abc");;
//val it : int = 536990974

> sprintf "result = %A" ([1], [true]);;
//val it : string = "result = ([1], [true])"

box;;
unbox;;
val box : 'T -> obj
val unbox : obj -> 'T

> box 1;;
//val it : obj = 1

> box "abc";;
//val it : obj = "abc"

> let stringObj = box "abc";;
//val stringObj: obj = "abc"

> (unbox<string> stringObj);;
//val it : string = "abc"

> (unbox stringObj : string);;
//val it : string = "abc"

> (unbox stringObj : int);;
//System.InvalidCastException: Specified cast is not valid.
//   at <StartupCode$FSI_0046>.$FSI_0046.main@()
//Stopped due to error

//val writeValue  : System.IO.Stream -> 'T -> unit
//val readValue   : System.IO.Stream -> 'T

open System.IO
open System.Runtime.Serialization.Formatters.Binary

let writeValue outputStream (x : 'T) =
    let formatter = new BinaryFormatter()
    formatter.Serialize(outputStream, box x)

let readValue inputStream =
    let formatter = new BinaryFormatter()
    let res = formatter.Deserialize(inputStream)
    unbox res

open System.IO
let addresses = Map.ofList ["Jeff", "123 Main Street, Redmond, WA 98052";
                            "Fred", "987 Pine Road, Phila., PA 19116";
                            "Mary", "PO Box 112233, Palo Alto, CA 94301"]


let fsOut = new FileStream("Data.dat", FileMode.Create)
writeValue fsOut addresses
fsOut.Close()
let fsIn = new FileStream("Data.dat", FileMode.Open)
let res : Map<string, string> = readValue fsIn
fsIn.Close()

> res;;

val it : Map<string,string> =
  map
    [("Fred", "987 Pine Road, Phila., PA 19116");
     ("Jeff", "123 Main Street, Redmond, WA 98052");
     ("Mary", "PO Box 112233, Palo Alto, CA 94301")]

let rec hcf a b =
    if a = 0 then b
    elif a < b then hcf a (b - a)
    else hcf (a - b) b
//val hcf : a:int -> b:int -> int

> hcf 18 12;;
//val it : int = 6

> hcf 33 24;;
//val it : int = 3

let hcfGeneric (zero, sub, lessThan) =
    let rec hcf a b =
        if a = zero then b
        elif lessThan a b then hcf a (sub b a)
        else hcf (sub a b) b
    hcf

//val hcfGeneric :
//  zero:'a * sub:('a -> 'a -> 'a) * lessThan:('a -> 'a -> bool) ->
//    ('a -> 'a -> 'a) when 'a : equality

let hcfInt = hcfGeneric (0, (-), (<))
let hcfInt64  = hcfGeneric (0L, (-), (<))
let hcfBigInt = hcfGeneric (0I, (-), (<))

> hcfInt 18 12;;
//val it : int = 6

> hcfBigInt 1810287116162232383039576I 1239028178293092830480239032I;;
//val it : bigint = 33224I

type Numeric<'T> =
    {Zero : 'T;
     Subtract : ('T -> 'T -> 'T);
     LessThan : ('T -> 'T -> bool);}

let intOps = {Zero = 0; Subtract = (-); LessThan = (<)}
let bigintOps = {Zero = 0I; Subtract = (-); LessThan = (<)}
let int64Ops = {Zero = 0L; Subtract = (-); LessThan = (<)}

let hcfGeneric (ops : Numeric<'T>) =
    let rec hcf a b =
        if a = ops.Zero then b
        elif ops.LessThan a b then hcf a (ops.Subtract b a)
        else hcf (ops.Subtract a b) b
    hcf

let hcfInt = hcfGeneric intOps
let hcfBigInt = hcfGeneric bigintOps

//val hcfGeneric : ops:Numeric<'T> -> ('T -> 'T -> 'T) when 'T : equality
//val hcfInt : (int -> int -> int)
//val hcfBigInt :
//  (System.Numerics.BigInteger -> System.Numerics.BigInteger ->
//     System.Numerics.BigInteger)

> hcfInt 18 12;;
//val it : int = 6

> hcfBigInt 1810287116162232383039576I 1239028178293092830480239032I;;
//val it : bigint = 33224I

type INumeric<'T> =
    abstract Zero : 'T
    abstract Subtract : 'T * 'T -> 'T
    abstract LessThan : 'T * 'T -> bool

let intOps =
    {new INumeric<int> with
        member ops.Zero = 0
        member ops.Subtract(x, y) = x - y
        member ops.LessThan(x, y) = x < y}
//val intOps : INumeric<int>

let hcfGeneric (ops : INumeric<'T>) =
    let rec hcf a b =
        if a = ops.Zero then b
        elif ops.LessThan(a, b) then hcf a (ops.Subtract(b, a))
        else hcf (ops.Subtract(a, b)) b
    hcf

let convertToFloat x = float x

float 3.0 + float 1 + float 3L

let inline convertToFloatAndAdd x y = float x + float y

let inline hcf a b =
    let rec loop a b = 
        if a = LanguagePrimitives.GenericZero<_> then b
        elif a < b then loop a (b - a)
        else loop (a - b) b
    loop a b

//val inline hcf :
//  a: ^a -> b: ^a ->  ^a
//    when  ^a : (static member get_Zero : ->  ^a) and  ^a : comparison and
//          ^a : (static member ( - ) :  ^a *  ^a ->  ^a)

> hcf 18 12;;
//val it : int = 6

> hcf 18I 12I;;
//val it : bigint = 6I

let inline hcf a b =
    hcfGeneric 
        {new INumeric<'T> with
            member ops.Zero = LanguagePrimitives.GenericZero<'T>
            member ops.Subtract(x, y) = x - y
            member ops.LessThan(x, y) = x < y}
        a b
//val inline hcf :
//  a: ^T -> b: ^T ->  ^T
//    when  ^T : (static member get_Zero : ->  ^T) and
//          ^T : (static member ( - ) :  ^T *  ^T ->  ^T) and  ^T : comparison

> let xobj = (1 :> obj);;
//val xobj : obj = 1

> let sobj = ("abc" :> obj);;
//val sobj : obj = "abc"

> let boxedObject = box "abc";;
//val boxedObject : obj

> let downcastString = (boxedObject :?> string);;
//val downcastString : string = "abc"

> let xobj = box 1;;
//val xobj : obj = 1

> let x = (xobj :?> string);;
//error: InvalidCastException raised at or near stdin:(2,0)

let checkObject (x : obj) =
    match x with
    | :? string -> printfn "The object is a string"
    | :? int -> printfn "The object is an integer"
    | _ -> printfn "The input is something else"
//val checkObject : x:obj -> unit

> checkObject (box "abc");;
//val it : unit = ()

let reportObject (x : obj) =
    match x with
    | :? string as s -> printfn "The input is the string '%s'" s
    | :? int as d -> printfn "The input is the integer '%d'" d
    | _ -> printfn "the input is something else"
//val reportObject : x:obj -> unit

>  reportObject (box 17);;
//val it : unit = ()

> open System.Windows.Forms;;

> let setTextOfControl (c : Control) (s : string) = c.Text <- s;;
//val setTextOfControl: Control -> string -> unit

> let form = new Form();;
//val form : Form = System.Windows.Forms.Form, Text: 

> let textBox = new TextBox();;
//val textBox : TextBox = System.Windows.Forms.TextBox, Text: 

> setTextOfControl form "Form Text";;

> setTextOfControl textBox "Text Box Text";;

open System
open System.IO

let textReader  =
    if DateTime.Today.DayOfWeek = DayOfWeek.Monday
    then Console.In
    else File.OpenText("input.txt")

error FS0001: This expression was expected to have type
    TextReader    
but here has type
    StreamReader
    
//StreamReader is a subtype of TextReader, so the code can be corrected by throwing away the information that the returned type is a StreamReader:

open System
open System.IO

let textReader =
    if DateTime.Today.DayOfWeek = DayOfWeek.Monday
    then Console.In
    else (File.OpenText("input.txt") :> TextReader)

let getTextReader () : TextReader = (File.OpenText("input.txt") :> TextReader)

> open System.Windows.Forms;;

> let setTextOfControl (c : 'T when 'T :> Control) (s : string) = c.Text <- s;;
//val setTextOfControl: #Control -> string -> unit

Seq.concat;;
Seq.concat [[1;2;3]; [4;5;6]]
Seq.concat [[|1; 2; 3|]; [|4; 5; 6|]]

> let getLengths inp = inp |> Seq.map (fun y -> y.Length);;
//error FS0072: Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.

let getLengths inp =
    inp |> Seq.map (fun (y : string) -> y.Length)

let printSecondElements (inp : seq<'T * int>) =
    inp
    |> Seq.iter (fun (x, y) -> printfn "y = %d" x)
//> ... enter the code above ...
//warning FS0064: This construct causes code to be less generic than indicated by the type annotations. The type variable 'T has been constrained to be type 'int'.

type PingPong = Ping | Pong

let printSecondElements (inp : seq<PingPong * int>) =
    inp
    |> Seq.iter (fun (x, y) -> printfn "y = %d" x)

> ... enter the code above ...

error FS0001: The type 'PingPong' is not compatible with any of the types byte,int16,int32,int64,sbyte,uint16,uint32,uint64,nativeint,unativeint, arising from the use of a printf-style format string

> let empties = Array.create 100 [];;
//error FS0030: Value restriction. The value 'empties' has been inferred to have generic type
//    val empties : '_a list []    
//Either define 'empties' as a simple data term, make it a function with explicit arguments or, if you do not intend for it to be generic, add a type annotation.

let emptyList = []
let initialLists = ([], [2])
let listOfEmptyLists = [[]; []]
let makeArray () = Array.create 100 []
//val emptyList : 'a list
//val initialLists : 'a list * int list
//val listOfEmptyLists : 'a list list
//val makeArray : unit -> 'a list []

let empties = Array.create 100 []
//error FS0030: Value restriction. The value 'empties' has been inferred to have generic type
//    val empties : '_a list []    
//Either define 'empties' as a simple data term, make it a function with explicit arguments or, if you do not intend for it to be generic, add a type annotation.

let empties : int list [] = Array.create 100 []
//val empties : int list [] =
//  [|[]; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; [];
//    []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; [];
//    []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; [];
//    []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; [];
//    []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; []; [];
//    []; []; []; []; []|]

let mapFirst = List.map fst
//error FS0030: Value restriction. The value 'mapFirst' has been inferred to have generic type
//    val mapFirst : (('_a * '_b) list -> '_a list)    
//Either make the arguments to 'mapFirst' explicit or, if you do not intend for it to be generic, add a type annotation.

let mapFirst inp = List.map fst inp
//val mapFirst : inp:('a * 'b) list -> 'a list

let mapFirst inp = inp |> List.map (fun (x, y) -> x)
//val mapFirst : inp:('a * 'b) list -> 'a list

let printFstElements = List.map fst >> List.iter (printf "res = %d")
//error FS0030: Value restriction. The value 'printFstElements' has been inferred to have generic type
//    val printFstElements : ((int * '_a) list -> unit)    
//Either make the arguments to 'printFstElements' explicit or, if you do not intend for it to be generic, add a type annotation.

let printFstElements inp = inp |> List.map fst |> List.iter (printf "res = %d")
//val printFstElements : inp:(int * 'a) list -> unit

let empties = Array.create 100 []

let empties () = Array.create 100 []
let intEmpties : int list [] = empties()
let stringEmpties : string list [] = empties()

let emptyLists = Seq.init 100 (fun _ -> [])
//error FS0030: Value restriction. The value 'emptyLists' has been inferred to have generic type
//    val emptyLists : seq<'_a list>    
//Either define 'emptyLists' as a simple data term, make it a function with explicit arguments or, if you do not intend for it to be generic, add a type annotation.

let emptyLists<'T> : seq<'T list> = Seq.init 100 (fun _ -> [])
//val emptyLists<'T> : seq<'T list>

> Seq.length emptyLists;;
//val it : int = 100

> emptyLists<int>;;
//val it : seq<int list> = seq [[]; []; []; []; ...]

> emptyLists<string>;;
//val it : seq<string list> = seq [[]; []; []; []; ...]

let twice x = (x + x)
//val twice : int -> int

let twiceFloat (x : float) = x + x
//val twiceFloat : x:float -> float

let threeTimes x = (x + x + x)
let sixTimesInt64 (x : int64) = threeTimes x + threeTimes x
//val threeTimes : x:int64 -> int64
//val sixTimesInt64 : x:int64 -> int64
