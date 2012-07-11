/// A table of people in our startup
let people = 
    [ ("Amber", 27, "Design")
      ("Wendy", 35, "Events")
      ("Antonio", 40, "Sales")
      ("Petra", 31, "Design")
      ("Carlos", 34, "Marketing") ]

/// Extract information from the table of people 
let namesOfPeopleStartingWithA =
    people 
      |> Seq.map (fun (name, age, dept) -> name)
      |> Seq.filter (fun name -> name.StartsWith "A")
      |> Seq.toList

/// Extract the names of designers from the table of people 
let namesOfDesigners =
    people 
      |> Seq.filter  (fun (_, _, dept) -> dept = "Design")
      |> Seq.map (fun (name, _, _) -> name)
      |> Seq.toList

//val people : (string * int * string) list =
//  [("Amber", 27, "Design"); ("Wendy", 35, "Events"); ("Antonio", 40, "Sales");
//   ("Petra", 31, "Design"); ("Carlos", 34, "Marketing")]
//
//val namesOfPeopleStartingWithA : string list = ["Amber"; "Antonio"]
//
//val namesOfDesigners : string list = ["Amber"; "Petra"]

/// A random-number generator
let rand = System.Random()

/// An infinite sequence of numbers
let randomNumbers = seq { while true do yield rand.Next(100000) }

/// The first 10 random numbers, sorted
let firstTenRandomNumbers =
    randomNumbers 
      |> Seq.truncate 10
      |> Seq.sort                         // sort ascending
      |> Seq.toList

/// The first 3000 even random numbers and sort them
let firstThreeThousandEvenNumbersWithSquares =
    randomNumbers 
      |> Seq.filter (fun i -> i % 2 = 0)  // "where"
      |> Seq.truncate 3000
      |> Seq.sort                         // sort ascending
      |> Seq.map (fun i -> i, i*i)        // "select"
      |> Seq.toList
// random – results will vary!
//val randomNumbers : seq<int> 
//
//val firstTenRandomNumbers : int list =
//  [9444; 14443; 15015; 20448; 31038; 46145; 69447; 85050; 85509; 92181]
//
//val firstThreeThousandEvenNumbersWithSquares : (int * int) list =
//  [(56, 3136); (62, 3844); (66, 4356); (68, 4624); (70, 4900); (86, 7396);
//   (144, 20736); (238, 56644); (248, 61504); (250, 62500); ... ] 

/// The first 10 random numbers, sorted by last digit
let firstTenRandomNumbersSortedByLastDigit = 
    randomNumbers 
      |> Seq.truncate 10 
      |> Seq.sortBy (fun x -> x % 10) 
      |> Seq.toList

//val firstTenRandomNumbersSortedByLastDigit : int list =
//  [51220; 56640; 88543; 97424; 90744; 11784; 23316; 1368; 71878; 89719]

module Seq = 
    val choose : chooser:('T -> 'U option) -> source:seq<'T> -> seq<'U>
    val collect : mapping:('T -> #seq<'U>) -> source:seq<'T> -> seq<'U>
    val map     : mapping:('T -> 'U)       -> source:seq<'T> -> seq<'U>

// Take the first 10 numbers and build a triangle 1, 1, 2, 1, 2, 3, 1, 2, 3, 4, …
let triangleNumbers = 
    [ 1 .. 10 ] 
      |> Seq.collect (fun i -> [ 1 .. i ] ) 
      |> Seq.toList 
//val triangleNumbers : int list =
//  [1; 1; 2; 1; 2; 3; 1; 2; 3; 4; 1; 2; 3; 4; 5; 1; 2; 3; 4; 5; 6; 1; 2; 3; 4;
//   5; 6; 7; 1; 2; 3; 4; 5; 6; 7; 8; 1; 2; 3; 4; 5; 6; 7; 8; 9; 1; 2; 3; 4; 5;
//   6; 7; 8; 9; 10]

let gameBoard = 
    [ for i in 0 .. 7 do 
          for j in 0 .. 7 do 
              yield (i,j,rand.Next(10)) ] 

let evenPositions = 
   gameBoard
     |> Seq.choose (fun (i,j,v) -> if v % 2 = 0 then Some (i,j) else None) 
     |> Seq.toList 

// random – results will vary!
//val gameBoard : (int * int * int) list =
//  [(0, 0, 9); (0, 1, 2); (0, 2, 8); (0, 3, 2); ...]
//
//val evenPositions : (int * int) list =
//  [(0, 1); (0, 2); (0, 3); ... ]

module Seq = 
    val find         : predicate:('T -> bool)    -> source:seq<'T> -> 'T
    val findIndex    : predicate:('T -> bool)    -> source:seq<'T> -> int
    val pick         : chooser:('T -> bool)      -> source:seq<'T> -> 'T
    val tryFind      : predicate:('T -> bool)    -> source:seq<'T> -> 'T option
    val tryFindIndex : predicate:('T -> bool)    -> source:seq<'T> -> int option
    val tryPick      : chooser:('T -> 'U option) -> source:seq<'T> -> 'U option

let firstElementScoringZero = 
   gameBoard |> Seq.tryFind (fun (i,j,v) -> v = 0)

let firstPositionScoringZero = 
   gameBoard |> Seq.tryPick (fun (i,j,v) -> if v = 0 then Some(i,j) else None)
// random – results will vary!
//val firstElementScoringZero : (int * int * int) option = Some (1, 5, 0) 
//
//val firstPositionScoringZero : (int * int) option = Some (1, 5) 

let positionsGroupedByGameValue = 
   gameBoard 
       |> Seq.groupBy (fun (i,j,v) -> v) 
       |> Seq.sortBy (fun (k,v) -> k)
       |> Seq.toList 
//val positionsGroupedByGameValue : (int * seq<int * int * int>) list =
//  [(0, <seq>); (1, <seq>); (2, <seq>); (3, <seq>); (4, <seq>); (5, <seq>);
//   (6, <seq>); (7, <seq>); (8, <seq>); (9, <seq>)] 

let positionsIndexedByGameValue = 
   gameBoard 
       |> Seq.groupBy (fun (i,j,v) -> v) 
       |> Seq.sortBy (fun (k,v) -> k)
       |> Seq.map (fun (k,v) -> (k, Seq.toList v))
       |> dict

let worstPositions = positionsIndexedByGameValue.[0]
let bestPositions = positionsIndexedByGameValue.[9]
// random – results will vary!
//val positionsIndexedByGameValue :
//  System.Collections.Generic.IDictionary<int,(int * int * int) list>
//val worstPositions : (int * int * int) list =
//  [|(0, 6, 0); (2, 1, 0); (2, 3, 0); (6, 0, 0); (7, 0, 0)|]
//val bestPositions : (int * int * int) list =
//  [|(0, 3, 9); (0, 7, 9); (5, 3, 9); (6, 4, 9); (6, 7, 9)|]

> List.fold (fun acc x -> acc + x) 0 [4; 5; 6];;
//val it : int = 15

> Seq.fold (fun acc x -> acc + x) 0.0 [4.0; 5.0; 6.0];;
//val it : float = 15.0

> List.foldBack (fun x acc -> min x acc) [4; 5; 6; 3; 5] System.Int32.MaxValue;;
//val it : int = 3

> List.fold (+) 0 [4; 5; 6];;
//val it : int = 15

> Seq.fold (+) 0.0 [4.0; 5.0; 6.0];;
//val it : float = 15.0

> List.foldBack min [4; 5; 6; 3; 5] System.Int32.MaxValue;;
//val it : int = 3

> List.foldBack (fst >> min) [(3, "three"); (5, "five")] System.Int32.MaxValue;;
//val it : int = 3

let triangleNumbers = 
    [ for i in 1 .. 10 do 
         for j in 1 .. i do
             yield (i,j) ] 

let evenPositions = 
    [ for (i,j,v) in gameBoard do 
          if v % 2 = 0 then 
              yield (i,j) ] 

let rec flatten scene =
    seq { match scene with
          | Composite scenes -> for x in scenes do yield! flatten x 
          | Ellipse _ | Rect _ -> yield scene }
//val flatten : Scene -> seq<Scene>

let rec flattenAux scene acc =
    match scene with
    | Composite(scenes) -> List.foldBack flattenAux scenes acc
    | Ellipse _
    | Rect _ -> scene :: acc

let flatten2 scene = flattenAux scene [] |> Seq.ofList

let flatten3 scene =
    let acc = new ResizeArray<_>()
    let rec flattenAux s =
        match s with
        | Composite(scenes) -> scenes |> List.iter flattenAux
        | Ellipse _ | Rect _ -> acc.Add s
    flattenAux scene;
    Seq.readonly acc
//val flatten2 : Scene -> seq<Scene>
//val flatten3 : Scene -> seq<Scene>

let rec rectanglesOnly scene =
    match scene with
    | Composite scenes -> Composite (scenes |> List.map rectanglesOnly)
    | Ellipse rect | Rect rect -> Rect rect

let rec mapRects f scene =
    match scene with
    | Composite scenes -> Composite (scenes |> List.map (mapRects f))
    | Ellipse rect -> Ellipse (f rect)
    | Rect rect -> Rect (f rect)

//val rectanglesOnly : Scene -> Scene
//val mapRects: (RectangleF -> RectangleF) -> Scene -> Scene

let adjustAspectRatio scene =
    scene |> mapRects (fun r -> RectangleF.Inflate(r, 1.1f, 1.0f/1.1f)) 

<Composite>
     <File file='spots.xml'/>
     <File file='dots.xml'/>
</Composite>

type Scene =
    | Ellipse   of RectangleF
    | Rect      of RectangleF
    | Composite of Scene list
    | Delay     of Lazy<Scene>

let rec extractScene (node: XmlNode) =
    let attribs = node.Attributes
    let childNodes = node.ChildNodes
    match node.Name with
    | "Circle"  ->
        ...
    | "File"   ->
        let file = attribs.GetNamedItem("file").Value
        let scene = lazy (let d = XmlDocument()
                          d.Load(file)
                          extractScene(d :> XmlNode))
        Scene.Delay scene

let rec getScene scene =
    match scene with
    | Delay d -> getScene (d.Force())
    | _ -> scene

let rec flattenAux scene acc =
    match getScene(scene) with
    | Composite   scenes -> List.foldBack flattenAux scenes acc
    | Ellipse _ | Rect _ -> scene :: acc
    | Delay _ -> failwith "this lazy value should have been eliminated by getScene"

let flatten2 scene = flattenAux scene []

type SceneVeryLazy =
    | Ellipse   of Lazy<RectangleF>
    | Rect      of Lazy<RectangleF>
    | Composite of seq<SceneVeryLazy>
    | LoadFile  of string

type SceneWithCachedBoundingBox =
    | Ellipse of RectangleF
    | Rect    of RectangleF
    | CompositeRepr  of SceneWithCachedBoundingBox list * RectangleF option ref

type SceneWithCachedBoundingBox =
    | Ellipse of RectangleF
    | Rect    of RectangleF
    | CompositeRepr   of SceneWithCachedBoundingBox list * RectangleF option ref

    member x.BoundingBox =
        match x with
        | Ellipse rect | Rect rect -> rect
        | CompositeRepr (scenes,cache) ->
            match !cache with
            | Some v -> v
            | None ->
                let bbox =
                    scenes
                    |> List.map (fun s -> s.BoundingBox)
                    |> List.reduce (fun r1 r2 -> RectangleF.Union(r1,r2))
                cache := Some bbox
                bbox

    /// Create a Composite node with an initially empty cache
    static member Composite(scenes)  = CompositeRepr(scenes,ref None)

type Prop =
    | And of Prop * Prop
    | Or  of Prop * Prop
    | Not of Prop
    | Var of string
    | True

type Prop =
    | Prop of int

and internal PropRepr =
    | AndRepr of Prop * Prop
    | OrRepr  of Prop * Prop
    | NotRepr of Prop
    | VarRepr of string
    | TrueRepr

open System.Collections.Generic

module PropOps =

    let internal uniqStamp = ref 0
    type internal PropTable() =
        let fwdTable = new Dictionary<PropRepr,Prop>(HashIdentity.Structural)
        let bwdTable = new Dictionary<int,PropRepr>(HashIdentity.Structural)
        member t.ToUnique repr =
            if fwdTable.ContainsKey repr then fwdTable.[repr]
            else let stamp = incr uniqStamp; !uniqStamp
                 let prop = Prop stamp
                 fwdTable.Add (repr, prop)
                 bwdTable.Add (stamp, repr)
                 prop
        member t.FromUnique (Prop stamp) =
            bwdTable.[stamp]

    let internal table = PropTable ()

    // Public construction functions
    let And (p1,p2) = table.ToUnique (AndRepr (p1, p2))
    let Not p       = table.ToUnique (NotRepr p)
    let Or (p1,p2)  = table.ToUnique (OrRepr (p1, p2))
    let Var p       = table.ToUnique (VarRepr p)
    let True        = table.ToUnique TrueRepr
    let False       = Not True

    // Deconstruction function
    let internal getRepr p = table.FromUnique p

> open PropOps;;
> True;;
//val it : Prop = Prop 1

> And (Var "x",Var "y");;
//val it : Prop = Prop 5

> getRepr it;;
//val it : PropRepr = AndRepr(Prop 3, Prop 4)

> And(Var "x",Var "y");;
//val it : Prop = Prop 5

[<Struct>]
type Complex(r: float, i: float) =
    static member Polar(mag,phase) = Complex(mag * cos phase, mag * sin phase)
    member x.Magnitude = sqrt(r*r + i*i)
    member x.Phase = atan2 i r
    member x.RealPart = r
    member x.ImaginaryPart = i

let (|Rect|) (x:Complex) = (x.RealPart, x.ImaginaryPart)

let (|Polar|) (x:Complex) = (x.Magnitude, x.Phase)

let addViaRect a b =
    match a, b with
    | Rect (ar, ai), Rect (br, bi) -> Complex (ar+br, ai+bi)

let mulViaRect a b =
    match a, b with
    | Rect (ar, ai), Rect (br, bi) -> Complex (ar*br - ai*bi, ai*br + bi*ar)

let mulViaPolar a b =
    match a, b with
    | Polar (m, p), Polar (n, q) -> Complex.Polar (m*n, p+q)

> let c = Complex (3.0, 4.0);;
//val c : complex

> c;;
//val it : complex = 3.0r+4.0i

>  match c with
   | Rect (x, y) -> printfn "x = %g, y = %g" x y;;
//x = 3, y = 4

>  match c with
   | Polar (x, y) -> printfn "x = %g, y = %g" x y;;
//x = 5.0, y = 0.927295

> addViaRect c c;;
//val it : complex = 6.0r+8.0i

> mulViaRect c c;;
//val it : complex = -7.0r+24.0i

> mulViaPolar c c;;
//val it : complex = -7.0r+24.0i

let mulViaRect a b =
    match a, b with
    | Rect (ar, ai), Rect (br, bi) ->
        Complex (ar*br - ai*bi, ai*br + bi*ar)
//val (|Rect|) : complex -> float * float
//val (|Polar|) : complex -> float * float

let add2 (Rect (ar, ai)) (Rect (br, bi))   = Complex (ar+br, ai+bi)
let mul2 (Polar (r1, th1)) (Polar (r2, th2)) = Complex (r1*r2, th1+th2)

type System.Type with
    member IsGenericType : bool
    member GetGenericTypeDefinition : unit -> Type
    member GetGenericArguments : unit -> Type[]
    member HasElementType : bool
    member GetElementType : unit -> Type
    member IsByRef : bool
    member IsPointer : bool
    member IsGenericParameter : bool
    member GenericParameterPosition : int

let (|Named|Array|Ptr|Param|) (typ : System.Type) =
    if typ.IsGenericType
    then Named(typ.GetGenericTypeDefinition(),typ.GetGenericArguments())
    elif typ.IsGenericParameter then Param(typ.GenericParameterPosition)
    elif not typ.HasElementType then Named(typ, [| |])
    elif typ.IsArray then Array(typ.GetElementType(),typ.GetArrayRank())
    elif typ.IsByRef then Ptr(true,typ.GetElementType())
    elif typ.IsPointer then Ptr(false,typ.GetElementType())
    else failwith "MSDN says this can't happen"

open System

let rec formatType typ =
    match typ with
    | Named (con, [| |]) -> sprintf "%s" con.Name
    | Named (con, args) -> sprintf "%s<%s>" con.Name (formatTypes args)
    | Array (arg, rank) -> sprintf "Array(%d,%s)" rank (formatType arg)
    | Ptr(true,arg) -> sprintf "%s&" (formatType arg)
    | Ptr(false,arg) -> sprintf "%s*" (formatType arg)
    | Param(pos) -> sprintf "!%d" pos

and formatTypes typs =
    String.Join(",", Array.map formatType typs)

let rec freeVarsAcc typ acc =
    match typ with
    | Array (arg, rank) -> freeVarsAcc arg acc
    | Ptr (_,arg) -> freeVarsAcc arg acc
    | Param _ -> (typ :: acc)
    | Named (con, args) -> Array.foldBack freeVarsAcc args acc
let freeVars typ = freeVarsAcc typ []

let (|MulThree|_|) inp = if inp % 3 = 0 then Some(inp/3) else None
let (|MulSeven|_|) inp = if inp % 7 = 0 then Some(inp/7) else None

let (|MulN|_|) n inp = if inp % n = 0 then Some(inp/n) else None

type Prop = Prop of int
and internal PropRepr =
    | AndRepr of Prop * Prop
    | OrRepr  of Prop * Prop
    | NotRepr of Prop
    | VarRepr of string
    | TrueRepr

module PropOps =
    ...
    let (|And|Or|Not|Var|True|) prop =
        match table.FromUnique prop with
        | AndRepr (x, y) -> And (x, y)
        | OrRepr (x, y) -> Or (x, y)
        | NotRepr x -> Not x
        | VarRepr v -> Var v
        | TrueRepr -> True

open PropOps

let rec showProp prec prop =
    let parenIfPrec lim s = if prec < lim then "(" + s + ")" else s
    match prop with
    | Or (p1, p2)  -> parenIfPrec 4 (showProp 4 p1 + " || " + showProp 4 p2)
    | And (p1, p2) -> parenIfPrec 3 (showProp 3 p1 + " && " + showProp 3 p2)
    | Not p        -> parenIfPrec 2 ("not " + showProp 1 p)
    | Var v        -> v
    | True         -> "T"

let rec nnf sign prop =
    match prop with
    | And (p1, p2) -> if sign then And (nnf sign p1, nnf sign p2)
                      else Or (nnf sign p1, nnf sign p2)
    | Or (p1, p2)  -> if sign then Or (nnf sign p1, nnf sign p2)
                      else And (nnf sign p1, nnf sign p2)
    | Not p        -> nnf (not sign) p
    | Var _ | True -> if sign then prop else Not prop

let NNF prop = nnf true prop

> let t1 = Not(And(Not(Var("x")),Not(Var("y"))));;
//val t1 : Prop

> fsi.AddPrinter(showProp);;
> t1;;
//val it : Prop = not (not x && not y)

> let t2 = Or(Not(Not(Var("x"))),Var("y"));;
//val t2 : Prop

> t2;;
//val it : Prop = not (not x) || y

> (t1 = t2);;
//val it : bool = false

> NNF t1;;
//val it : Prop = x || y

> NNF t2;;
//val it : Prop = x || y

> NNF t1 = NNF t2;;
//val it : bool = true

//val compare : 'T -> 'T -> int when 'T : comparison
//val (=)   : 'T -> 'T -> bool when 'T : equality
//val (<)   : 'T -> 'T -> bool when 'T : comparison
//val (<=)  : 'T -> 'T -> bool when 'T : comparison
//val (>)   : 'T -> 'T -> bool when 'T : comparison
//val (>=)  : 'T -> 'T -> bool when 'T : comparison
//val min : 'T -> 'T -> 'T when 'T : comparison
//val max : 'T -> 'T -> 'T when 'T : comparison
//val hash  : 'T -> int when 'T : equality

type Set<'T when 'T : comparison> = ...
type Map<'Key, 'Value when 'Key : comparison> = ...

let form1 = new System.Windows.Forms.Form()
let form2 = new System.Windows.Forms.Form()
form1 = form1    // true
form1 = form2    // false

let form1 = new System.Windows.Forms.Form()
let form2 = new System.Windows.Forms.Form()
form1 <= form2
    // Error: The type 'Form' does not support the 'comparison' constraint. 
    // For example, it does not support the 'System.IComparable' interface	

> let form1 = new System.Windows.Forms.Form();;
> let form2 = new System.Windows.Forms.Form();;

> (form1, form2) = (form1, form2);;
//val it : bool =  true

(form1, form2) = (form2, form1) // false

> (form1, "Data for Form1") <= (form2, " Data for Form2")
// Error: The type 'System.Windows.Forms.Form' does not support the 'comparison' constraint. 

[<StructuralEquality;StructuralComparison>] 
type MiniIntegerContainer = MiniIntegerContainer of int

[<StructuralEquality;StructuralComparison>] 
type MyData = MyData of int * string * string * System.Windows.Forms.Form

//   error FS1177: The struct, record or union type 'MyData' has the 
//   'StructuralComparison' attribute but the component type
//   'System.Windows.Forms.Form' does not satisfy the 'comparison' constraint

/// A type abbreviation indicating we're using integers for unique stamps 
/// on objects 
type stamp = int

/// A structural type containing a function that can't be compared for equality   
[<CustomEquality; CustomComparison>]
type MyThing = 
    { Stamp: stamp;
      Behaviour: (int -> int) }  

    override x.Equals(yobj) = 
        match yobj with
        | :? MyThing as y -> (x.Stamp = y.Stamp)
        | _ -> false

    override x.GetHashCode() = hash x.Stamp
    interface System.IComparable with
      member x.CompareTo yobj = 
          match yobj with
          | :? MyThing as y -> compare x.Stamp y.Stamp
          | _ -> invalidArg "yobj" "cannot compare values of different types"

namespace System

    type IComparable =
          abstract CompareTo : obj -> int

val hash : 'T -> int when 'T : equality
val (=) : 'T -> 'T -> bool when 'T : equality
val compare : 'T -> 'T -> int when 'T : comparison

let inline equalsOn f x (yobj:obj) = 
    match yobj with
    | :? 'T as y -> (f x = f y)
    | _ -> false

let inline hashOn f x =  hash (f x)

let inline compareOn f x (yobj: obj) = 
    match yobj with
    | :? 'T as y -> compare (f x) (f y)
    | _ -> invalidArg "yobj" "cannot compare values of different types"

type stamp = int

[<CustomEquality; CustomComparison>]
type MyUnionType = 
    | MyUnionType of stamp * (int -> int)  

    static member Stamp (MyUnionType (s,_)) = s

    override x.Equals y = equalsOn MyUnionType.Stamp x y
    override x.GetHashCode() = hashOn MyUnionType.Stamp x
    interface System.IComparable with
      member x.CompareTo y = compareOn MyUnionType.Stamp x y

[<ReferenceEquality>] 
type MyFormWrapper = MyFormWrapper of System.Windows.Forms.Form * (int -> int)

[<NoEquality; NoComparison>]
type MyProjections = 
    | MyProjections of (int * string) * (string -> int)  

type Graph<'Node when 'Node : equality>() = ...

type MiniContainer<'T> = MiniContainer of 'T

type MiniContainer<[<EqualityConditionalOn; ComparisonConditionalOn >]'T>(x : 'T) =
    member x.Value = x
    override x.Equals(yobj) = 
        match yobj with
        | :? MiniContainer<'T> as y -> Unchecked.equals x.Value y.Value
        | _ -> false

    override x.GetHashCode() = Unchecked.hash x.Value

    interface System.IComparable with
      member x.CompareTo yobj = 
          match yobj with
          | :? MiniContainer<'T> as y -> Unchecked.compare x.Value y.Value
          | _ -> invalidArg "yobj" "cannot compare values of different types"

let rec deepRecursion n =
    if n = 1000000 then () else
    if n % 100 = 0 then
        printfn "--> deepRecursion, n = %d" n
    deepRecursion (n+1)
    printfn "<-- deepRecursion, n = %d" n

> deepRecursion 0;;
//--> deepRecursion, n = 0
//...
//--> deepRecursion, n = 79100
//--> deepRecursion, n = 79200
//--> deepRecursion, n = 79300
//Process is terminated due to StackOverflowException
//Session termination detected. Press Enter to restart.

let rec tailCallRecursion n : unit =
    if n = 1000000 then () else
    if n % 100 = 0 then
        printfn "--> tailCallRecursion, n = %d" n
    tailCallRecursion (n+1)

> tailCallRecursion 0;;
...
//--> tailCallRecursion, n = 999600
//--> tailCallRecursion, n = 999700
//--> tailCallRecursion, n = 999800
//--> tailCallRecursion, n = 999900

let rec last l =
    match l with
    | [] -> invalidArg "l" "the input list should not be empty"
    | [h] -> h
    | h::t -> last t

let rec replicateNotTailRecursiveA n x =
    if n <= 0 then []
    else x :: replicateNotTailRecursiveA (n-1) x

let rec replicateNotTailRecursiveB n x =
    if n <= 0 then []
    else
        let recursiveResult = replicateNotTailRecursiveB (n-1) x
        x :: recursiveResult

let rec replicateAux n x acc =
    if n <= 0 then acc
    else replicateAux (n-1) x (x::acc)

let replicate n x = replicateAux n x []

let replicate n x =
    let rec loop i acc =
        if i >= n then acc
        else loop (i+1) (x::acc)
    loop 0 []

let rec mapNotTailRecursive f inputList =
    match inputList with
    | [] -> []
    | h::t -> (f h) :: mapNotTailRecursive f t

let rec mapIncorrectAcc f inputList acc =
    match inputList with
    | [] -> acc            // whoops! Forgot to reverse the accumulator here!
    | h::t -> mapIncorrectAcc f t (f h :: acc)

let mapIncorrect f inputList = mapIncorrectAcc f inputList []

> mapIncorrect (fun x -> x * x) [1;2;3;4];;
//val it : int list = [ 16; 9; 4; 1]

let rec mapAcc f inputList acc =
    match inputList with
    | [] -> List.rev acc
    | h::t -> mapAcc f t (f h :: acc)

let map f inputList = mapAcc f inputList []
> map (fun x -> x * x) [1;2;3;4];;
//val it : int list = [ 1; 4; 9; 16]

type Chain =
    | ChainNode of int * string * Chain
    | ChainEnd of string

    member chain.LengthNotTailRecursive =
        match chain with
        | ChainNode(_,_,subChain) -> 1 + subChain.LengthNotTailRecursive
        | ChainEnd _ -> 0

type Chain =
    | ChainNode of int * string * Chain
    | ChainEnd of string

    // The implementation of this property is tail recursive.
    member chain.Length =
        let rec loop c acc =
            match c with
            | ChainNode(_,_,subChain) -> loop subChain (acc+1)
            | ChainEnd _ -> acc
        loop chain 0

type Tree =
    | Node of string * Tree * Tree
    | Tip of string

let rec sizeNotTailRecursive tree =
    match tree with
    | Tip _ -> 1
    | Node(_,treeLeft,treeRight) ->
        sizeNotTailRecursive treeLeft + sizeNotTailRecursive treeRight

let rec mkBigUnbalancedTree n tree =
    if n = 0 then tree
    else Node("node",Tip("tip"),mkBigUnbalancedTree (n-1) tree)

let tree1 = Tip("tip")
let tree2 = mkBigUnbalancedTree 10000 tree1
let tree3 = mkBigUnbalancedTree 10000 tree2
let tree4 = mkBigUnbalancedTree 10000 tree3
let tree5 = mkBigUnbalancedTree 10000 tree4
let tree6 = mkBigUnbalancedTree 10000 tree5

let rec sizeAcc acc tree =
    match tree with
    | Tip _ -> 1+acc
    | Node(_,treeLeft,treeRight) ->
        let acc = sizeAcc acc treeLeft
        sizeAcc acc treeRight

let size tree = sizeAcc 0 tree

let rec sizeCont tree cont =
    match tree with
    | Tip _ -> cont 1
    | Node(_,treeLeft,treeRight) ->
        sizeCont treeLeft (fun leftSize ->
          sizeCont treeRight (fun rightSize ->
            cont (leftSize + rightSize)))

let size tree = sizeCont tree (fun x -> x)

//val sizeCont : Tree -> (int -> 'a) -> 'a
//val size : Tree -> int

sizeCont treeLeft (fun leftSize ->
  sizeCont treeRight (fun rightSize ->
    cont (leftSize + rightSize)))

> size tree6;;
//val it : int = 50001

let rec sizeContAcc acc tree cont =
    match tree with
    | Tip _ -> cont (1+acc)
    | Node (_, treeLeft, treeRight) ->
        sizeContAcc acc treeLeft (fun accLeftSize ->
        sizeContAcc accLeftSize treeRight cont)

let size tree = sizeContAcc 0 tree (fun x -> x)

type Expr =
    | Add  of Expr * Expr
    | Bind of string * Expr * Expr
    | Var  of string
    | Num  of int

type Env = Map<string,int>

let rec eval (env: Env) expr =
    match expr with
    | Add (e1,e2)         -> eval env e1 + eval env e2
    | Bind (var,rhs,body) -> eval (env.Add(var, eval env rhs)) body
    | Var var             -> env.[var]
    | Num n               -> n

let rec evalCont (env: Env) expr cont =
    match expr with
    | Add (e1,e2)         ->
        evalCont env e1 (fun v1 ->
        evalCont env e2 (fun v2 ->
        cont (v1+v2)))
    | Bind (var,rhs,body) ->
        evalCont env rhs (fun v1 ->
        evalCont (env.Add(var,v1)) body cont)
    | Num n             ->
        cont n
    | Var var           ->
        cont (env.[var])

let eval env expr = evalCont env expr (fun x -> x)