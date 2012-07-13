/// A table of people in our startup
let people = 
    [("Amber", 27, "Design")
     ("Wendy", 35, "Events")
     ("Antonio", 40, "Sales")
     ("Petra", 31, "Design")
     ("Carlos", 34, "Marketing")]

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

Seq.choose;;
Seq.collect;;
Seq.map;;
//val it : (('a -> 'b option) -> seq<'a> -> seq<'b>) = <fun:clo@72>
//val it : (('a -> #seq<'c>) -> seq<'a> -> seq<'c>) = <fun:clo@73-1>
//val it : (('a -> 'b) -> seq<'a> -> seq<'b>) = <fun:clo@74-2>
module Seq = 
    val choose : chooser : ('T -> 'U option) -> source : seq<'T> -> seq<'U>
    val collect : mapping : ('T -> #seq<'U>) -> source : seq<'T> -> seq<'U>
    val map : mapping : ('T -> 'U) -> source : seq<'T> -> seq<'U>

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
    val find : predicate : ('T -> bool) -> source : seq<'T> -> 'T
    val findIndex : predicate : ('T -> bool) -> source : seq<'T> -> int
    val pick : chooser : ('T -> bool) -> source : seq<'T> -> 'T
    val tryFind : predicate : ('T -> bool) -> source : seq<'T> -> 'T option
    val tryFindIndex : predicate : ('T -> bool) -> source : seq<'T> -> int option
    val tryPick : chooser : ('T -> 'U option) -> source : seq<'T> -> 'U option

let firstElementScoringZero = 
   gameBoard |> Seq.tryFind (fun (i, j, v) -> v = 0)

let firstPositionScoringZero = 
   gameBoard |> Seq.tryPick (fun (i, j, v) -> if v = 0 then Some(i, j) else None)
// random – results will vary!
//val firstElementScoringZero : (int * int * int) option = Some (1, 5, 0) 
//
//val firstPositionScoringZero : (int * int) option = Some (1, 5) 

let positionsGroupedByGameValue = 
   gameBoard 
       |> Seq.groupBy (fun (i, j, v) -> v) 
       |> Seq.sortBy (fun (k, v) -> k)
       |> Seq.toList 
//val positionsGroupedByGameValue : (int * seq<int * int * int>) list =
//  [(0, <seq>); (1, <seq>); (2, <seq>); (3, <seq>); (4, <seq>); (5, <seq>);
//   (6, <seq>); (7, <seq>); (8, <seq>); (9, <seq>)] 

let positionsIndexedByGameValue = 
   gameBoard 
       |> Seq.groupBy (fun (i, j, v) -> v) 
       |> Seq.sortBy (fun (k, v) -> k)
       |> Seq.map (fun (k, v) -> (k, Seq.toList v))
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
    [for i in 1 .. 10 do 
         for j in 1 .. i do
             yield (i, j)] 

let evenPositions = 
    [for (i, j, v) in gameBoard do 
         if v % 2 = 0 then 
             yield (i, j)] 
//val triangleNumbers : (int * int) list =
//  [(1, 1); (2, 1); (2, 2); (3, 1); (3, 2); (3, 3); (4, 1); (4, 2); (4, 3);
//   (4, 4); (5, 1); (5, 2); (5, 3); (5, 4); (5, 5); (6, 1); (6, 2); (6, 3);
//   (6, 4); (6, 5); (6, 6); (7, 1); (7, 2); (7, 3); (7, 4); (7, 5); (7, 6);
//   (7, 7); (8, 1); (8, 2); (8, 3); (8, 4); (8, 5); (8, 6); (8, 7); (8, 8);
//   (9, 1); (9, 2); (9, 3); (9, 4); (9, 5); (9, 6); (9, 7); (9, 8); (9, 9);
//   (10, 1); (10, 2); (10, 3); (10, 4); (10, 5); (10, 6); (10, 7); (10, 8);
//   (10, 9); (10, 10)]
//val evenPositions : (int * int) list =
//  [(0, 0); (0, 2); (0, 4); (0, 7); (1, 2); (1, 3); (1, 5); (1, 7); (2, 4);
//   (2, 5); (2, 6); (2, 7); (3, 2); (3, 5); (3, 6); (4, 4); (4, 6); (4, 7);
//   (5, 4); (5, 6); (5, 7); (6, 1); (6, 4); (6, 5); (6, 7); (7, 0); (7, 2);
//   (7, 3); (7, 4); (7, 5); (7, 6)]

// From chapter 8 on text, the definition of Scene.
open System.Drawing
type Scene =
    | Ellipse of RectangleF
    | Rect of RectangleF
    | Composite of Scene list
//type Scene =
//  | Ellipse of RectangleF
//  | Rect of RectangleF
//  | Composite of Scene list

let rec flatten scene =
    seq {match scene with
         | Composite scenes -> for x in scenes do yield! flatten x 
         | Ellipse _ | Rect _ -> yield scene}
//val flatten : scene:Scene -> seq<Scene>

let rec flattenAux scene acc =
    match scene with
    | Composite(scenes) -> List.foldBack flattenAux scenes acc
    | Ellipse _
    | Rect _ -> scene :: acc

let flatten2 scene = flattenAux scene [] |> Seq.ofList
//val flattenAux : scene:Scene -> acc:Scene list -> Scene list
//val flatten2 : scene:Scene -> seq<Scene>
let flatten3 scene =
    let acc = new ResizeArray<_>()
    let rec flattenAux s =
        match s with
        | Composite(scenes) -> scenes |> List.iter flattenAux
        | Ellipse _ | Rect _ -> acc.Add s
    flattenAux scene;
    Seq.readonly acc
//val flatten2 : scene:Scene -> seq<Scene>
//val flatten3 : scene:Scene -> seq<Scene>

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
    scene |> mapRects (fun r -> RectangleF.Inflate(r, 1.1f, 1.0f / 1.1f)) 

<Composite>
     <File file='spots.xml'/>
     <File file='dots.xml'/>
</Composite>

// This Scene type and extractScene function are from chapter 8.
type Scene =
    | Ellipse of RectangleF
    | Rect of RectangleF
    | Composite of Scene list
    | Delay of Lazy<Scene>

    /// A derived constructor
    static member Circle(center : PointF, radius) =
        Ellipse(RectangleF(center.X - radius, center.Y - radius,
                           radius * 2.0f, radius * 2.0f))

    /// A derived constructor
    static member Square(left, top, side) =
        Rect(RectangleF(left, top, side, side))
//type Scene =
//  | Ellipse of RectangleF
//  | Rect of RectangleF
//  | Composite of Scene list
//  | Delay of Lazy<Scene>
//  with
//    static member Circle : center:PointF * radius:float32 -> Scene
//    static member Square : left:float32 * top:float32 * side:float32 -> Scene
//  end

open System.Xml

/// Extract a number from an XML attribute collection
let extractFloat32 attrName (attribs : XmlAttributeCollection) =
    float32 (attribs.GetNamedItem(attrName).Value)

/// Extract a Point from an XML attribute collection
let extractPointF (attribs : XmlAttributeCollection) =
    PointF(extractFloat32 "x" attribs, extractFloat32 "y" attribs)

/// Extract a Rectangle from an XML attribute collection
let extractRectangleF (attribs : XmlAttributeCollection) =
    RectangleF(extractFloat32 "left" attribs, extractFloat32 "top" attribs,
               extractFloat32 "width" attribs, extractFloat32 "height" attribs)
//val extractFloat32 :
//  attrName:string -> attribs:XmlAttributeCollection -> float32
//val extractPointF : attribs:XmlAttributeCollection -> PointF
//val extractRectangleF : attribs:XmlAttributeCollection -> RectangleF
               
let rec extractScene (node : XmlNode) =
    let attribs = node.Attributes
    let childNodes = node.ChildNodes
    match node.Name with
    | "Circle"  ->
        Scene.Circle(extractPointF(attribs), extractFloat32 "radius" attribs)
    | "Ellipse"  ->
        Scene.Ellipse(extractRectangleF(attribs))
    | "Rectangle"  ->
        Scene.Rect(extractRectangleF(attribs))
    | "Square"  ->
        Scene.Square(extractFloat32 "left" attribs, extractFloat32 "top" attribs,
                     extractFloat32 "side" attribs)
    | "Composite"   ->
        Scene.Composite [for child in childNodes -> extractScene(child)]
    | "File" ->
        let file = attribs.GetNamedItem("file").Value
        let scene = lazy (let d = XmlDocument()
                          d.Load(file)
                          extractScene(d :> XmlNode))
        Scene.Delay scene
    | _ -> failwithf "unable to convert XML '%s'" node.OuterXml 
//val extractScene : node:XmlNode -> Scene

let rec extractScene (node : XmlNode) =
    let attribs = node.Attributes
    let childNodes = node.ChildNodes
    match node.Name with
    | "Circle" ->
        ...
    | "File" ->
        let file = attribs.GetNamedItem("file").Value
        let scene = lazy (let d = XmlDocument()
                          d.Load(file)
                          extractScene(d :> XmlNode))
        Scene.Delay scene

let rec getScene scene =
    match scene with
    | Delay d -> getScene (d.Force())
    | _ -> scene
//val getScene : scene:Scene -> Scene

let rec flattenAux scene acc =
    match getScene(scene) with
    | Composite scenes -> List.foldBack flattenAux scenes acc
    | Ellipse _ | Rect _ -> scene :: acc
    | Delay _ -> failwith "this lazy value should have been eliminated by getScene"
//val flattenAux : scene:Scene -> acc:Scene list -> Scene list

let flatten2 scene = flattenAux scene []
//val flatten2 : scene:Scene -> Scene list

type SceneVeryLazy =
    | Ellipse of Lazy<RectangleF>
    | Rect of Lazy<RectangleF>
    | Composite of seq<SceneVeryLazy>
    | LoadFile of string
//type SceneVeryLazy =
//  | Ellipse of Lazy<RectangleF>
//  | Rect of Lazy<RectangleF>
//  | Composite of seq<SceneVeryLazy>
//  | LoadFile of string

type SceneWithCachedBoundingBox =
    | Ellipse of RectangleF
    | Rect of RectangleF
    | CompositeRepr of SceneWithCachedBoundingBox list * RectangleF option ref
//type SceneWithCachedBoundingBox =
//  | Ellipse of RectangleF
//  | Rect of RectangleF
//  | CompositeRepr of SceneWithCachedBoundingBox list * RectangleF option ref

type SceneWithCachedBoundingBox =
    | Ellipse of RectangleF
    | Rect of RectangleF
    | CompositeRepr of SceneWithCachedBoundingBox list * RectangleF option ref

    member x.BoundingBox =
        match x with
        | Ellipse rect | Rect rect -> rect
        | CompositeRepr (scenes, cache) ->
            match !cache with
            | Some v -> v
            | None ->
                let bbox =
                    scenes
                    |> List.map (fun s -> s.BoundingBox)
                    |> List.reduce (fun r1 r2 -> RectangleF.Union(r1, r2))
                cache := Some bbox
                bbox

    /// Create a Composite node with an initially empty cache
    static member Composite(scenes)  = CompositeRepr(scenes, ref None)
//type SceneWithCachedBoundingBox =
//  | Ellipse of RectangleF
//  | Rect of RectangleF
//  | CompositeRepr of SceneWithCachedBoundingBox list * RectangleF option ref
//  with
//    member BoundingBox : RectangleF
//    static member
//      Composite : scenes:SceneWithCachedBoundingBox list ->
//                    SceneWithCachedBoundingBox
//  end

type Prop =
    | And of Prop * Prop
    | Or of Prop * Prop
    | Not of Prop
    | Var of string
    | True
//type Prop =
//  | And of Prop * Prop
//  | Or of Prop * Prop
//  | Not of Prop
//  | Var of string
//  | True

type Prop =
    | Prop of int

and internal PropRepr =
    | AndRepr of Prop * Prop
    | OrRepr of Prop * Prop
    | NotRepr of Prop
    | VarRepr of string
    | TrueRepr
//type Prop = | Prop of int
//and internal PropRepr =
//  | AndRepr of Prop * Prop
//  | OrRepr of Prop * Prop
//  | NotRepr of Prop
//  | VarRepr of string
//  | TrueRepr

open System.Collections.Generic

module PropOps =

    let internal uniqStamp = ref 0
    type internal PropTable() =
        let fwdTable = new Dictionary<PropRepr, Prop>(HashIdentity.Structural)
        let bwdTable = new Dictionary<int, PropRepr>(HashIdentity.Structural)
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
    let And (p1, p2) = table.ToUnique (AndRepr (p1, p2))
    let Not p = table.ToUnique (NotRepr p)
    let Or (p1, p2)  = table.ToUnique (OrRepr (p1, p2))
    let Var p = table.ToUnique (VarRepr p)
    let True = table.ToUnique TrueRepr
    let False = Not True

    // Deconstruction function
    let internal getRepr p = table.FromUnique p
//module PropOps = begin
//  val internal uniqStamp : int ref = {contents = 2;}
//  type internal PropTable =
//    class
//      new : unit -> PropTable
//      member FromUnique : Prop -> PropRepr
//      member ToUnique : repr:PropRepr -> Prop
//    end
//  val internal table : PropTable
//  val And : p1:Prop * p2:Prop -> Prop
//  val Not : p:Prop -> Prop
//  val Or : p1:Prop * p2:Prop -> Prop
//  val Var : p:string -> Prop
//  val True : Prop = Prop 1
//  val False : Prop = Prop 2
//  val internal getRepr : p:Prop -> PropRepr
//end

> open PropOps;;
> True;;
//val it : Prop = Prop 1

> And (Var "x", Var "y");;
//val it : Prop = Prop 5

> getRepr it;;
//error FS0410: The type 'PropRepr' is less accessible than the value, member or type 'val it : PropRepr' it is used in

> And(Var "x", Var "y");;
//val it : Prop = Prop 5

[<Struct>]
type Complex(r : float, i : float) =
    static member Polar(mag, phase) = Complex(mag * cos phase, mag * sin phase)
    member x.Magnitude = sqrt(r * r + i * i)
    member x.Phase = atan2 i r
    member x.RealPart = r
    member x.ImaginaryPart = i

let (|Rect|) (x : Complex) = (x.RealPart, x.ImaginaryPart)

let (|Polar|) (x : Complex) = (x.Magnitude, x.Phase)

let addViaRect a b =
    match a, b with
    | Rect (ar, ai), Rect (br, bi) -> Complex (ar + br, ai + bi)

let mulViaRect a b =
    match a, b with
    | Rect (ar, ai), Rect (br, bi) -> Complex (ar * br - ai * bi, ai * br + bi * ar)

let mulViaPolar a b =
    match a, b with
    | Polar (m, p), Polar (n, q) -> Complex.Polar (m * n, p + q)
//type Complex =
//  struct
//    new : r:float * i:float -> Complex
//    member ImaginaryPart : float
//    member Magnitude : float
//    member Phase : float
//    member RealPart : float
//    static member Polar : mag:float * phase:float -> Complex
//  end
//val ( |Rect| ) : x:Complex -> float * float
//val ( |Polar| ) : x:Complex -> float * float
//val addViaRect : a:Complex -> b:Complex -> Complex
//val mulViaRect : a:Complex -> b:Complex -> Complex
//val mulViaPolar : a:Complex -> b:Complex -> Complex

// This is the printer for complex from chapter 10. I'll adapt it now ...
fsi.AddPrinter (fun (c : System.Numerics.Complex) -> sprintf "%fr + %fi" c.Real c.Imaginary)
fsi.AddPrinter (fun (c : Complex) -> sprintf "%gr + %gi" c.RealPart c.ImaginaryPart)
> let c = Complex (3.0, 4.0);;
//val c : Complex = 3r + 4i

> c;;
//val it : Complex = 3r + 4i

>  match c with
   | Rect (x, y) -> printfn "x = %g, y = %g" x y;;
//x = 3, y = 4

>  match c with
   | Polar (x, y) -> printfn "x = %g, y = %g" x y;;
//x = 5, y = 0.927295

> addViaRect c c;;
//val it : Complex = 6r + 8i

> mulViaRect c c;;
//val it : Complex = -7r + 24i

> mulViaPolar c c;;
//val it : Complex = -7r + 24i

let mulViaRect a b =
    match a, b with
    | Rect (ar, ai), Rect (br, bi) ->
        Complex (ar * br - ai * bi, ai * br + bi * ar)
//val (|Rect|) : complex -> float * float
//val (|Polar|) : complex -> float * float

let add2 (Rect (ar, ai)) (Rect (br, bi)) = Complex (ar + br, ai + bi)
let mul2 (Polar (r1, th1)) (Polar (r2, th2)) = Complex (r1 * r2, th1 + th2)

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
    then Named(typ.GetGenericTypeDefinition(), typ.GetGenericArguments())
    elif typ.IsGenericParameter then Param(typ.GenericParameterPosition)
    elif not typ.HasElementType then Named(typ, [||])
    elif typ.IsArray then Array(typ.GetElementType(), typ.GetArrayRank())
    elif typ.IsByRef then Ptr(true, typ.GetElementType())
    elif typ.IsPointer then Ptr(false, typ.GetElementType())
    else failwith "MSDN says this can't happen"
//val ( |Named|Array|Ptr|Param| ) :
//  typ:System.Type ->
//    Choice<(System.Type * System.Type []),(System.Type * int),
//           (bool * System.Type),int>

open System

let rec formatType typ =
    match typ with
    | Named (con, [||]) -> sprintf "%s" con.Name
    | Named (con, args) -> sprintf "%s<%s>" con.Name (formatTypes args)
    | Array (arg, rank) -> sprintf "Array(%d,%s)" rank (formatType arg)
    | Ptr(true, arg) -> sprintf "%s&" (formatType arg)
    | Ptr(false, arg) -> sprintf "%s*" (formatType arg)
    | Param(pos) -> sprintf "!%d" pos

and formatTypes typs =
    String.Join(",", Array.map formatType typs)
//val formatType : typ:Type -> string
//val formatTypes : typs:Type [] -> string

let rec freeVarsAcc typ acc =
    match typ with
    | Array (arg, rank) -> freeVarsAcc arg acc
    | Ptr (_, arg) -> freeVarsAcc arg acc
    | Param _ -> (typ :: acc)
    | Named (con, args) -> Array.foldBack freeVarsAcc args acc

let freeVars typ = freeVarsAcc typ []
//val freeVarsAcc : typ:Type -> acc:Type list -> Type list
//val freeVars : typ:Type -> Type list

let (|MulThree|_|) inp = if inp % 3 = 0 then Some(inp / 3) else None
let (|MulSeven|_|) inp = if inp % 7 = 0 then Some(inp / 7) else None
//val ( |MulThree|_| ) : inp:int -> int option
//val ( |MulSeven|_| ) : inp:int -> int option

let (|MulN|_|) n inp = if inp % n = 0 then Some(inp / n) else None
//val ( |MulN|_| ) : n:int -> inp:int -> int option

type Prop = Prop of int
and internal PropRepr =
    | AndRepr of Prop * Prop
    | OrRepr of Prop * Prop
    | NotRepr of Prop
    | VarRepr of string
    | TrueRepr
//type Prop = | Prop of int
//and internal PropRepr =
//  | AndRepr of Prop * Prop
//  | OrRepr of Prop * Prop
//  | NotRepr of Prop
//  | VarRepr of string
//  | TrueRepr

open System.Collections.Generic

module PropOps =

    let internal uniqStamp = ref 0
    type internal PropTable() =
        let fwdTable = new Dictionary<PropRepr, Prop>(HashIdentity.Structural)
        let bwdTable = new Dictionary<int, PropRepr>(HashIdentity.Structural)
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
    let And (p1, p2) = table.ToUnique (AndRepr (p1, p2))
    let Not p = table.ToUnique (NotRepr p)
    let Or (p1, p2)  = table.ToUnique (OrRepr (p1, p2))
    let Var p = table.ToUnique (VarRepr p)
    let True = table.ToUnique TrueRepr
    let False = Not True

    // Deconstruction function
    let internal getRepr p = table.FromUnique p
    
    let (|And|Or|Not|Var|True|) prop =
        match table.FromUnique prop with
        | AndRepr (x, y) -> And (x, y)
        | OrRepr (x, y) -> Or (x, y)
        | NotRepr x -> Not x
        | VarRepr v -> Var v
        | TrueRepr -> True
//module PropOps = begin
//  val internal uniqStamp : int ref = {contents = 2;}
//  type internal PropTable =
//    class
//      new : unit -> PropTable
//      member FromUnique : Prop -> PropRepr
//      member ToUnique : repr:PropRepr -> Prop
//    end
//  val internal table : PropTable
//  val And : p1:Prop * p2:Prop -> Prop
//  val Not : p:Prop -> Prop
//  val Or : p1:Prop * p2:Prop -> Prop
//  val Var : p:string -> Prop
//  val True : Prop = Prop 1
//  val False : Prop = Prop 2
//  val internal getRepr : p:Prop -> PropRepr
//  val ( |And|Or|Not|Var|True| ) :
//    prop:Prop -> Choice<(Prop * Prop),(Prop * Prop),Prop,string,unit>
//end

module PropOps =
//    ...
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
    | Or (p1, p2) -> parenIfPrec 4 (showProp 4 p1 + " || " + showProp 4 p2)
    | And (p1, p2) -> parenIfPrec 3 (showProp 3 p1 + " && " + showProp 3 p2)
    | Not p -> parenIfPrec 2 ("not " + showProp 1 p)
    | Var v -> v
    | True -> "T"
//val showProp : prec:int -> prop:Prop -> string

let rec nnf sign prop =
    match prop with
    | And (p1, p2) ->
        if sign then And (nnf sign p1, nnf sign p2)
        else Or (nnf sign p1, nnf sign p2)
    | Or (p1, p2) ->
        if sign then Or (nnf sign p1, nnf sign p2)
        else And (nnf sign p1, nnf sign p2)
    | Not p ->
        nnf (not sign) p
    | Var _ | True ->
        if sign then prop else Not prop

let NNF prop = nnf true prop
//val nnf : sign:bool -> prop:Prop -> Prop
//val NNF : prop:Prop -> Prop

> let t1 = Not(And(Not(Var("x")), Not(Var("y"))));;
//val t1 : Prop

> fsi.AddPrinter(showProp 5);;
> t1;;
//val it : Prop = not (not x && not y)

> let t2 = Or(Not(Not(Var("x"))), Var("y"));;
val t2 : Prop = not (not x) || y

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

val compare : 'T -> 'T -> int when 'T : comparison
val (=) : 'T -> 'T -> bool when 'T : equality
val (<) : 'T -> 'T -> bool when 'T : comparison
val (<=) : 'T -> 'T -> bool when 'T : comparison
val (>) : 'T -> 'T -> bool when 'T : comparison
val (>=) : 'T -> 'T -> bool when 'T : comparison
val min : 'T -> 'T -> 'T when 'T : comparison
val max : 'T -> 'T -> 'T when 'T : comparison
val hash : 'T -> int when 'T : equality
//val it : ('a -> 'a -> int) when 'a : comparison = <fun:it@768-16>
//val it : ('a -> 'a -> bool) when 'a : equality = <fun:it@769-17>
//val it : ('a -> 'a -> bool) when 'a : comparison = <fun:it@770-18>
//val it : ('a -> 'a -> bool) when 'a : comparison = <fun:it@771-19>
//val it : ('a -> 'a -> bool) when 'a : comparison = <fun:it@772-20>
//val it : ('a -> 'a -> bool) when 'a : comparison = <fun:it@773-21>
//val it : ('a -> 'a -> 'a) when 'a : comparison = <fun:it@774-22>
//val it : ('a -> 'a -> 'a) when 'a : comparison = <fun:it@775-23>
//val it : ('a -> int) when 'a : equality = <fun:it@776-24>

type Set<'T when 'T : comparison> = ...
type Map<'Key, 'Value when 'Key : comparison> = ...

let form1 = new System.Windows.Forms.Form()
let form2 = new System.Windows.Forms.Form()
form1 = form1    // true
form1 = form2    // false

let form1 = new System.Windows.Forms.Form()
let form2 = new System.Windows.Forms.Form()
form1 <= form2
//error FS0001: The type 'Windows.Forms.Form' does not support the 'comparison' constraint. For example, it does not support the 'System.IComparable' interface

> let form1 = new System.Windows.Forms.Form();;
> let form2 = new System.Windows.Forms.Form();;

> (form1, form2) = (form1, form2);;
//val it : bool =  true

(form1, form2) = (form2, form1) // false

> (form1, "Data for Form1") <= (form2, " Data for Form2")
//error FS0001: The type 'Windows.Forms.Form' does not support the 'comparison' constraint. For example, it does not support the 'System.IComparable' interface 

[<StructuralEquality; StructuralComparison>] 
type MiniIntegerContainer = MiniIntegerContainer of int

[<StructuralEquality; StructuralComparison>] 
type MyData = MyData of int * string * string * System.Windows.Forms.Form

//   error FS1177: The struct, record or union type 'MyData' has the 
//   'StructuralComparison' attribute but the component type
//   'System.Windows.Forms.Form' does not satisfy the 'comparison' constraint

/// A type abbreviation indicating we're using integers for unique stamps 
/// on objects 
type stamp = int
//type stamp = int

/// A structural type containing a function that can't be compared for equality   
[<CustomEquality; CustomComparison>]
type MyThing = 
    {Stamp : stamp;
     Behaviour : (int -> int)}  

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
//type MyThing =
//  {Stamp: stamp;
//   Behaviour: int -> int;}
//  with
//    interface System.IComparable
//    override Equals : yobj:obj -> bool
//    override GetHashCode : unit -> int
//  end

namespace System

    type IComparable =
          abstract CompareTo : obj -> int

val hash : 'T -> int when 'T : equality
val (=) : 'T -> 'T -> bool when 'T : equality
val compare : 'T -> 'T -> int when 'T : comparison
//val it : ('a -> int) when 'a : equality = <fun:it@857>
//val it : ('a -> 'a -> bool) when 'a : equality = <fun:it@858-1>
//val it : ('a -> 'a -> int) when 'a : comparison = <fun:it@859-2>

let inline equalsOn f x (yobj : obj) = 
    match yobj with
    | :? 'T as y -> (f x = f y)
    | _ -> false

let inline hashOn f x =  hash (f x)

let inline compareOn f x (yobj : obj) = 
    match yobj with
    | :? 'T as y -> compare (f x) (f y)
    | _ -> invalidArg "yobj" "cannot compare values of different types"
//val inline equalsOn :
//  f:('T -> 'a) -> x:'T -> yobj:obj -> bool when 'a : equality
//val inline hashOn : f:('a -> 'b) -> x:'a -> int when 'b : equality
//val inline compareOn :
//  f:('T -> 'a) -> x:'T -> yobj:obj -> int when 'a : comparison

type stamp = int

[<CustomEquality; CustomComparison>]
type MyUnionType = 
    | MyUnionType of stamp * (int -> int)  

    static member Stamp (MyUnionType (s, _)) = s

    override x.Equals y = equalsOn MyUnionType.Stamp x y
    override x.GetHashCode() = hashOn MyUnionType.Stamp x
    interface System.IComparable with
        member x.CompareTo y = compareOn MyUnionType.Stamp x y
//type MyUnionType =
//  | MyUnionType of stamp * (int -> int)
//  with
//    interface System.IComparable
//    override Equals : y:obj -> bool
//    override GetHashCode : unit -> int
//    static member Stamp : MyUnionType -> stamp
//  end

[<ReferenceEquality>] 
type MyFormWrapper = MyFormWrapper of System.Windows.Forms.Form * (int -> int)
//type MyFormWrapper =
//  | MyFormWrapper of System.Windows.Forms.Form * (int -> int)

[<NoEquality; NoComparison>]
type MyProjections = 
    | MyProjections of (int * string) * (string -> int)  
//type MyProjections = | MyProjections of (int * string) * (string -> int)

type Graph<'Node when 'Node : equality>() = ...

type MiniContainer<'T> = MiniContainer of 'T
//type MiniContainer<'T> = | MiniContainer of 'T

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
//type MiniContainer<'T> =
//  class
//    interface System.IComparable
//    new : x:'T -> MiniContainer<'T>
//    override Equals : yobj:obj -> bool
//    override GetHashCode : unit -> int
//    member Value : MiniContainer<'T>
//  end

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
    tailCallRecursion (n + 1)
//val tailCallRecursion : n:int -> unit

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
//val last : l:'a list -> 'a

let rec replicateNotTailRecursiveA n x =
    if n <= 0 then []
    else x :: replicateNotTailRecursiveA (n - 1) x
//val replicateNotTailRecursiveA : n:int -> x:'a -> 'a list

let rec replicateNotTailRecursiveB n x =
    if n <= 0 then []
    else
        let recursiveResult = replicateNotTailRecursiveB (n - 1) x
        x :: recursiveResult
//val replicateNotTailRecursiveB : n:int -> x:'a -> 'a list

let rec replicateAux n x acc =
    if n <= 0 then acc
    else replicateAux (n - 1) x (x :: acc)

let replicate n x = replicateAux n x []
//val replicateAux : n:int -> x:'a -> acc:'a list -> 'a list
//val replicate : n:int -> x:'a -> 'a list

let replicate n x =
    let rec loop i acc =
        if i >= n then acc
        else loop (i + 1) (x :: acc)
    loop 0 []
//val replicate : n:int -> x:'a -> 'a list

let rec mapNotTailRecursive f inputList =
    match inputList with
    | [] -> []
    | h :: t -> (f h) :: mapNotTailRecursive f t
//val mapNotTailRecursive : f:('a -> 'b) -> inputList:'a list -> 'b list

let rec mapIncorrectAcc f inputList acc =
    match inputList with
    | [] -> acc            // whoops! Forgot to reverse the accumulator here!
    | h :: t -> mapIncorrectAcc f t (f h :: acc)

let mapIncorrect f inputList = mapIncorrectAcc f inputList []
//val mapIncorrectAcc :
//  f:('a -> 'b) -> inputList:'a list -> acc:'b list -> 'b list
//val mapIncorrect : f:('a -> 'b) -> inputList:'a list -> 'b list

> mapIncorrect (fun x -> x * x) [1; 2; 3; 4];;
//val it : int list = [16; 9; 4; 1]

let rec mapAcc f inputList acc =
    match inputList with
    | [] -> List.rev acc
    | h :: t -> mapAcc f t (f h :: acc)

let map f inputList = mapAcc f inputList []
//val mapAcc : f:('a -> 'b) -> inputList:'a list -> acc:'b list -> 'b list
//val map : f:('a -> 'b) -> inputList:'a list -> 'b list

> map (fun x -> x * x) [1; 2; 3; 4];;
//val it : int list = [1; 4; 9; 16]

type Chain =
    | ChainNode of int * string * Chain
    | ChainEnd of string

    member chain.LengthNotTailRecursive =
        match chain with
        | ChainNode(_, _, subChain) -> 1 + subChain.LengthNotTailRecursive
        | ChainEnd _ -> 0

type Chain =
    | ChainNode of int * string * Chain
    | ChainEnd of string

    // The implementation of this property is tail recursive.
    member chain.Length =
        let rec loop c acc =
            match c with
            | ChainNode(_, _, subChain) -> loop subChain (acc + 1)
            | ChainEnd _ -> acc
        loop chain 0
//type Chain =
//  | ChainNode of int * string * Chain
//  | ChainEnd of string
//  with
//    member Length : int
//  end

type Tree =
    | Node of string * Tree * Tree
    | Tip of string

let rec sizeNotTailRecursive tree =
    match tree with
    | Tip _ -> 1
    | Node(_, treeLeft, treeRight) ->
        sizeNotTailRecursive treeLeft + sizeNotTailRecursive treeRight
//type Tree =
//  | Node of string * Tree * Tree
//  | Tip of string
//val sizeNotTailRecursive : tree:Tree -> int

let rec mkBigUnbalancedTree n tree =
    if n = 0 then tree
    else Node("node", Tip("tip"), mkBigUnbalancedTree (n - 1) tree)

let tree1 = Tip("tip")
let tree2 = mkBigUnbalancedTree 10000 tree1
let tree3 = mkBigUnbalancedTree 10000 tree2
let tree4 = mkBigUnbalancedTree 10000 tree3
let tree5 = mkBigUnbalancedTree 10000 tree4
let tree6 = mkBigUnbalancedTree 10000 tree5

let rec sizeAcc acc tree =
    match tree with
    | Tip _ -> 1 + acc
    | Node(_, treeLeft, treeRight) ->
        let acc = sizeAcc acc treeLeft
        sizeAcc acc treeRight

let size tree = sizeAcc 0 tree
//val sizeAcc : acc:int -> tree:Tree -> int
//val size : tree:Tree -> int

let rec sizeCont tree cont =
    match tree with
    | Tip _ -> cont 1
    | Node(_, treeLeft, treeRight) ->
        sizeCont treeLeft (fun leftSize ->
          sizeCont treeRight (fun rightSize ->
            cont (leftSize + rightSize)))

let size tree = sizeCont tree (fun x -> x)
//val sizeCont : tree:Tree -> cont:(int -> 'a) -> 'a
//val size : tree:Tree -> int

// This is a snippet from above and is not meant to be standalone
//sizeCont treeLeft (fun leftSize ->
//  sizeCont treeRight (fun rightSize ->
//    cont (leftSize + rightSize)))

> size tree6;;
//val it : int = 50001

let rec sizeContAcc acc tree cont =
    match tree with
    | Tip _ -> cont (1 + acc)
    | Node (_, treeLeft, treeRight) ->
        sizeContAcc acc treeLeft (fun accLeftSize ->
        sizeContAcc accLeftSize treeRight cont)

let size tree = sizeContAcc 0 tree (fun x -> x)
//val sizeContAcc : acc:int -> tree:Tree -> cont:(int -> 'a) -> 'a
//val size : tree:Tree -> int

type Expr =
    | Add of Expr * Expr
    | Bind of string * Expr * Expr
    | Var of string
    | Num of int
//type Expr =
//  | Add of Expr * Expr
//  | Bind of string * Expr * Expr
//  | Var of string
//  | Num of int

type Env = Map<string, int>

let rec eval (env : Env) expr =
    match expr with
    | Add (e1, e2) -> eval env e1 + eval env e2
    | Bind (var, rhs, body) -> eval (env.Add(var, eval env rhs)) body
    | Var var -> env.[var]
    | Num n -> n
//type Env = Map<string,int>
//val eval : env:Env -> expr:Expr -> int

let rec evalCont (env : Env) expr cont =
    match expr with
    | Add (e1, e2) ->
        evalCont env e1 (fun v1 ->
        evalCont env e2 (fun v2 ->
        cont (v1 + v2)))
    | Bind (var, rhs, body) ->
        evalCont env rhs (fun v1 ->
        evalCont (env.Add(var, v1)) body cont)
    | Num n ->
        cont n
    | Var var ->
        cont (env.[var])

let eval env expr = evalCont env expr (fun x -> x)
//val evalCont : env:Env -> expr:Expr -> cont:(int -> 'a) -> 'a
//val eval : env:Env -> expr:Expr -> int
