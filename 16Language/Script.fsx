> seq { for i in 0 .. 3 -> (i,i*i) };;
//val it : seq<int * int> = seq [ (0,0); (1,1); (2,4); (3,9) ]

        let rec loop n =
            async { printfn "n = %d, waiting..." n
                    let! msg = inbox.Receive()
                    return! loop (n+msg) }

bind : M<'T> -> ('T ->  M<'U>) -> M<'U>
return : 'T -> M<'T>

type Attempt<'T> = (unit -> 'T option)

let succeed x = (fun () -> Some(x)) : Attempt<'T>
let fail      = (fun () -> None) : Attempt<'T>
let runAttempt (a:Attempt<'T>) = a()

//val succeed : 'T -> Attempt<'T>
//val fail : Attempt<'T>
//val runAttempt : Attempt<'T> -> 'T option

type AttemptBuilder =
    member Bind : Attempt<'T> * ('T -> Attempt<'U>) -> Attempt<'U>
    member Delay : (unit -> Attempt<'T>) -> Attempt<'T>
    member Return : 'T -> Attempt<'T>
    member ReturnFrom : Attempt<'T> -> Attempt<'T>

let attempt = new AttemptBuilder()
//val attempt : AttemptBuilder

> let alwaysOne = attempt { return 1 };;
//val alwaysOne: Attempt<int>

> let alwaysPair = attempt { return (1,"two") };;
//val alwaysPair: Attempt<int * string>

> runAttempt alwaysOne;;
//val it : int option = Some 1

> runAttempt alwaysPair;;
//val it : (int * string) option = Some(1,"two")

> let failIfBig n = attempt { if n > 1000 then return! fail else return n };;
//val failIfBig: int -> Attempt<int>

> runAttempt (failIfBig 999);;
//val it : int option = Some 999

> runAttempt (failIfBig 1001);;
//val it : int option = None

> let failIfEitherBig (inp1,inp2) =
        attempt { let! n1 = failIfBig inp1
                  let! n2 = failIfBig inp2
                  return (n1,n2) };;
//val failIfEitherBig: int * int -> Attempt<int * int>

> runAttempt (failIfEitherBig (999,998));;
//val it : (int * int) option = Some(999,998)

> runAttempt (failIfEitherBig (1003,998));;
//val it : (int * int) option = None

> runAttempt (failIfEitherBig (999,1001));;
//val it : (int * int) option = None

  let sumIfBothSmall (inp1,inp2) =
        attempt { let! n1 = failIfBig inp1
                  let! n2 = failIfBig inp2
                  let sum = n1 + n2
                  return sum }

let succeed x = (fun () -> Some(x))
let fail      = (fun () -> None)
let runAttempt (a:Attempt<'T>) = a()
let bind p rest = match runAttempt p with None -> fail | Some r -> (rest r)
let delay f = (fun () -> runAttempt (f ()))
let combine p1 p2 = (fun () -> match p1() with None -> p2() | res -> res)

type AttemptBuilder() =

    /// Used to de-sugar uses of 'let!' inside computation expressions.
    member b.Bind(p,rest) = bind p rest

    /// Delays the construction of an attempt until just before it is executed
    member b.Delay(f) = delay f

    /// Used to de-sugar uses of 'return' inside computation expressions.
    member b.Return(x) = succeed x

    /// Used to de-sugar uses of 'return!' inside computation expressions.
    member b.ReturnFrom(x:Attempt<'T>) = x

    /// Used to de-sugar uses of 'c1; c2' inside computation expressions.
    member b.Combine(p1:Attempt<'T>,p2:Attempt<'T>) = combine p1 p2

    /// Used to de-sugar uses of 'if .. then ..' inside computation expressions.
    member b.Zero() = fail

let attempt = new AttemptBuilder()

type AttemptBuilder =
    new : unit -> AttemptBuilder
    member Bind   : Attempt<'T> * ('T -> Attempt<'U>) -> Attempt<'U>
    member Delay  : (unit -> Attempt<'T>) -> Attempt<'T>
    member Combine  : Attempt<'T> * Attempt<'T> -> Attempt<'T>
    member Zero : unit -> Attempt<'T> 
    member Return : 'T -> Attempt<'T>
    member ReturnFrom : Attempt<'T> -> Attempt<'T>
//val attempt : AttemptBuilder

attempt { let! n1 = failIfBig inp1
          let! n2 = failIfBig inp2
          let sum = n1 + n2
          return sum }

attempt.Bind( failIfBig inp1,(fun n1 ->
   attempt.Bind(failIfBig inp2,(fun n2 ->
      attempt.Return sum)))))

//val bindM : M<'T> -> ('T -> M<'U>) -> M<'U>
//val returnM : 'T -> M<'T>

let delayM f = bindM (returnM ()) f


type MBuilder() =
    member b.Return(x)    = returnM x
    member b.Bind(v,f)    = bindM v f
    member b.Delay(f)     = delayM f

let sumIfBothSmall (inp1,inp2) =
    attempt { let! n1 = failIfBig inp1
              printfn "Hey, n1 was small!"
              let! n2 = failIfBig inp2
              printfn "n2 was also small!"
              let sum = n1 + n2
              return sum }

> runAttempt(sumIfBothSmall (999,999));;
//Hey, n1 was small!
//n2 was also small!
//val it : int option = Some 1998

> runAttempt(sumIfBothSmall (999,1003));;
//Hey, n1 was small!
//val it : int option = None

let sumIfBothSmall (inp1,inp2) =
    attempt { let sum = ref 0
              let! n1 = failIfBig inp1
              sum := sum.Value + n1
              let! n2 = failIfBig inp2
              sum := sum.Value + n2
              return sum.Value }

let printThenSeven =
        attempt { printf "starting..."
                  return 3 + 4 }

        attempt.Delay(fun () ->
            printf "starting..."
            attempt.Return(3+4))

type Attempt<'T> = (unit -> 'T option)
let succeed x = (fun () -> Some(x))
let fail      = (fun () -> None)
let runAttempt (a:Attempt<'T>) = a()
let bind p rest = match runAttempt p with None -> fail | Some r -> (rest r)
let delay f = (fun () -> runAttempt (f ()))
let condition p guard = (fun () -> 
    match p() with 
    | Some x when guard x -> Some x 
    | _ -> None)

type AttemptBuilder() =
    member b.Return(x) = succeed x
    member b.Bind(p,rest) = bind p rest
    member b.Delay(f) = delay f

    [<CustomOperation("condition",MaintainsVariableSpaceUsingBind=true)>]
    member x.Condition(p,[<ProjectionParameter>] b) =  condition p b

let attempt = new AttemptBuilder()

let randomNumberInCircle = 
    attempt { let x,y = rand(),rand()
              condition (x*x+y*y < 1.0)
              return (x,y) }
                   
type Distribution<'T when 'T : comparison> =
    abstract Sample : 'T
    abstract Support : Set<'T>
    abstract Expectation: ('T -> float) -> float

let always x =
    { new Distribution<'T> with
         member d.Sample = x
         member d.Support = Set.singleton x
         member d.Expectation(H) = H(x) }

let rnd = System.Random()

let coinFlip (p:float) (d1:Distribution<'T>) (d2:Distribution<'T>) =
    if p < 0.0 || p > 1.0 then failwith "invalid probability in coinFlip"
    { new Distribution<'T> with
         member d.Sample =
             if rnd.NextDouble() < p then d1.Sample else d2.Sample
         member d.Support = Set.union d1.Support d2.Support
         member d.Expectation(H) =
             p * d1.Expectation(H) + (1.0-p) * d2.Expectation(H) }

The types of these primitives are as follows:
type Distribution<'T> =
    abstract Expectation: ('T -> float) -> float
    abstract Sample : 'T
    abstract Support : Set<'T>

//val always: 'T -> Distribution<'T>
//val coinFlip : float -> Distribution<'T> -> Distribution<'T> -> Distribution<'T>

let bind (dist:Distribution<'T>) (k: 'T -> Distribution<'U>) =
    { new Distribution<'U> with
         member d.Sample = 
             (k dist.Sample).Sample
         member d.Support =
             Set.unionMany (dist.Support |> Set.map (fun d -> (k d).Support))
         member d.Expectation H = 
             dist.Expectation(fun x -> (k x).Expectation H) }

type DistributionBuilder() =
    member x.Delay f = bind (always ()) f
    member x.Bind (d, f) = bind d f
    member x.Return v = always v
    member x.ReturnFrom vs = vs

let dist = new DistributionBuilder()

//val bind: Distribution<'T> -> ('T -> Distribution<'U>) -> Distribution<'U>
//val dist: DistributionBuilder

let weightedCases (inp: ('T * float) list) =
    let rec coinFlips w l =
        match l with
        | []          -> failwith "no coinFlips"
        | [(d,_)]     -> always d
        | (d,p)::rest -> coinFlip (p/(1.0-w)) (always d) (coinFlips (w+p) rest)
    coinFlips 0.0 inp

let countedCases inp =
    let total = Seq.sumBy (fun (_,v) -> v) inp
    weightedCases (inp |> List.map (fun (x,v) -> (x, float v / float total)))

//val weightedCases : ('T * float) list -> Distribution<'T>
//val countedCases : ('T * int) list -> Distribution<'T>

type Outcome = Even | Odd | Zero
let roulette = countedCases [ Even,18; Odd,18; Zero,1]

> roulette.Sample;;
//val it:  Outcome = Even

> roulette.Sample;;
//val it:  Outcome = Odd

> roulette.Expectation (function Even -> 10.0 | Odd -> 0.0 | Zero -> 0.0);;
//val it:  float = 4.864864865

type Light =
    | Red
    | Green
    | Yellow

let trafficLightD = weightedCases [ Red,0.50; Yellow,0.10; Green, 0.40 ]

type Action = Stop | Drive

let cautiousDriver light =
    dist { match light with
           | Red -> return Stop
           | Yellow -> return! weightedCases [ Stop, 0.9; Drive, 0.1 ]
           | Green -> return Drive }

let aggressiveDriver light =
    dist { match light with
           | Red    -> return! weightedCases [ Stop, 0.9; Drive, 0.1 ]
           | Yellow -> return! weightedCases [ Stop, 0.1; Drive, 0.9 ]
           | Green  -> return Drive }

let otherLight light =
    match light with
    | Red -> Green
    | Yellow -> Red
    | Green -> Red

type CrashResult = Crash | NoCrash

// Where the suffix D means distribution
let crash (driverOneD, driverTwoD, lightD) =
    dist { // Sample from the traffic light
           let! light = lightD

           // Sample the first driver's behavior given the traffic light
           let! driverOne = driverOneD light

           // Sample the second driver's behavior given the traffic light
           let! driverTwo = driverTwoD (otherLight light)

           // Work out the probability of a crash
           match driverOne, driverTwo with
             | Drive,Drive -> return! weightedCases [ Crash, 0.9; NoCrash, 0.1 ]
             | _ -> return NoCrash }

> let model = crash (cautiousDriver, aggressiveDriver, trafficLightD);;
//val model : Distribution<CrashResult>

> model.Sample;;
//val it : CrashResult = NoCrash
...
> model.Sample;;
//val it : CrashResult = Crash

> model.Expectation (function Crash -> 1.0 | NoCrash -> 0.0);;
//val it : float = 0.0369

let linesOfFile(fileName) =
    seq { use textReader = System.IO.File.OpenText(fileName)
          while not textReader.EndOfStream do
              yield textReader.ReadLine() }

let rnd = System.Random()

let rec randomWalk k =
    seq { yield k
          yield! randomWalk (k + rnd.NextDouble() - 0.5) }
> randomWalk 10.0;;
//val it: seq<float> = seq [10.0; 10.23817784; 9.956430122; 10.18110362; ...]

> randomWalk 10.0;;
//val it : seq<float> = seq [10.0; 10.19761089; 10.26774703; 9.888072922; ...]

> let intType = typeof<int>;;
//val intType : System.Type

> intType.FullName;;
//val it : string = "System.Int32"

> intType.AssemblyQualifiedName;;
//val it : string = "System.Int32, mscorlib, Version=2.0.0.0, Culture=neutral,
//PublicKeyToken=b77a5c561934e089"

> let intListType = typeof<int list>;;
//val intListType : System.Type

> intListType.FullName;;
//val it : string = "Microsoft.FSharp.Collections.List`1[[System.Int32, mscorlib,
//Version=2.0.0.0, Culture=neutral, PublicKeyToken=b77a5c561934e089]]"

open System
open System.IO
open System.Globalization
open Microsoft.FSharp.Reflection

/// An attribute to be added to fields of a schema record type to indicate the
/// column used in the data format for the schema.
type ColumnAttribute(col:int) =
    inherit Attribute()
    member x.Column = col

/// SchemaReader builds an object that automatically transforms lines of text
/// files in comma-separated form into instances of the given type 'Schema.
/// 'Schema must be an F# record type where each field is attributed with a
/// ColumnAttribute attribute, indicating which column of the data the record
/// field is drawn from. This simple version of the reader understands
/// integer, string and DateTime values in the CSV format.
type SchemaReader<'Schema>() =

    // Grab the object for the type that describes the schema
    let schemaType = typeof<'Schema>

    // Grab the fields from that type
    let fields = FSharpType.GetRecordFields(schemaType) 

    // For each field find the ColumnAttribute and compute a function
    // to build a value for the field
    let schema =
        fields |> Array.mapi (fun fldIdx fld ->
            let fieldInfo = schemaType.GetProperty(fld.Name)
            let fieldConverter =
                match fld.PropertyType with
                |  ty when ty = typeof<string>   -> (fun (s:string) -> box s)
                |  ty when ty = typeof<int>      -> (System.Int32.Parse >> box)
                |  ty when ty = typeof<DateTime> -> 
                     (fun s -> box (DateTime.Parse(s,CultureInfo.InvariantCulture)))
                |  ty -> failwithf "Unknown primitive type %A" ty

            let attrib =
                match fieldInfo.GetCustomAttributes(typeof<ColumnAttribute>,
                                                    false) with
                | [| (:? ColumnAttribute as attrib) |] ->   attrib
                | _ -> failwithf "No column attribute found on field %s" fld.Name
            (fldIdx, fld.Name, attrib.Column, fieldConverter))

    // Compute the permutation defined by the ColumnAttribute indexes
    let columnToFldIdxPermutation c =
      schema |> Array.pick (fun (fldIdx,_,colIdx,_) -> 
          if colIdx = c then Some fldIdx else None)

    // Drop the parts of the schema we don't need
    let schema =
      schema |> Array.map (fun (_,fldName,_,fldConv) -> (fldName,fldConv))

    // Compute a function to build instances of the schema type. This uses an
    // F# library function.
    let objectBuilder = FSharpValue.PreComputeRecordConstructor(schemaType)

    // OK, now we're ready to implement a line reader
    member reader.ReadLine(textReader: TextReader) =
        let line = textReader.ReadLine()
        let words = line.Split([|','|]) |> Array.map(fun s -> s.Trim())
        if words.Length <> schema.Length then
            failwith "unexpected number of columns in line %s" line
        let words = words |> Array.permute columnToFldIdxPermutation

        let convertColumn colText (fieldName, fieldConverter) =
           try fieldConverter colText
           with e ->
               failwithf "error converting '%s' to field '%s'" colText fieldName

        let obj = objectBuilder (Array.map2 convertColumn words schema)

        // OK, now we know we've dynamically built an object of the right type
        unbox<'Schema>(obj)

    /// This reads an entire file
    member reader.ReadFile(file) =
        seq { use textReader = File.OpenText(file)
              while not textReader.EndOfStream do
                  yield reader.ReadLine(textReader) }

//type SchemaReader<'Schema> =
//    new : unit -> SchemaReader<'Schema>
//    member ReadFile : string -> seq<'Schema>
//    member ReadLine : System.IO.TextReader -> 'Schema

Steve, 12 March 2010, Cheddar
Sally, 18 Feb 2010, Brie
...

type CheeseClub =
    { [<Column(0)>] Name            : string
      [<Column(2)>] FavouriteCheese : string
      [<Column(1)>] LastAttendance  : System.DateTime }

> let reader = new SchemaReader<CheeseClub>();;
//val reader : SchemaReader<CheeseClub>

> fsi.AddPrinter(fun (c:System.DateTime) -> c.ToString());;

> System.IO.File.WriteAllLines("data.txt", [| "Steve, 12 March 2010, Cheddar";
                                              "Sally, 18 Feb 2010, Brie"; |]);;

> reader.ReadFile("data.txt");;
//val it : seq<CheeseClub>
// = seq
//    [{Name = "Steve";
//      FavouriteCheese = "Cheddar";
//      LastAttendance = 12/03/2010 00:00:00;};
//     {Name = "Sally";
//      FavouriteCheese = "Brie";
//      LastAttendance = 18/02/2010 00:00:00;}]

open System.Reflection

let (?) (obj:obj) (nm:string) : 'T = 
    obj.GetType().InvokeMember(nm, BindingFlags.GetProperty, null, obj, [| |]) 
    |> unbox<'T>

let (?<-) (obj:obj) (nm:string) (v:obj) : unit = 
    obj.GetType().InvokeMember(nm, BindingFlags.SetProperty, null, obj, [| v |]) 
    |> ignore

type Record1 = { Length : int; mutable Values : int list }

let obj1 = box [1;2;3]
let obj2 = box { Length = 4; Values = [3;4;5;7] }

let n1 : int = obj1?Length 
let n2 : int = obj2?Length
let valuesOld : int list = obj2?Values

obj2?Values <- [7;8;9]

let valuesNew : int list = obj2?Values

> open Microsoft.FSharp.Quotations;;

> let oneExpr = <@ 1 @>;;
//val oneExpr : Expr<int>

> oneExpr;;
//val it : Expr<int> = <@ (Int32 1) @>

> let plusExpr = <@ 1 + 1 @>;;
//val plusExpr : Expr<int>

> plusExpr;;
//val it : Expr<int>
//  = <@ Microsoft.FSharp.Core.Operators.op_Addition (Int32 1) (Int32 1) @>

open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns
open Microsoft.FSharp.Quotations.DerivedPatterns

type Error = Err of float

let rec errorEstimateAux (e:Expr) (env : Map<Var,_>) =
    match e with
    | SpecificCall <@@ (+) @@> (tyargs,_,[xt;yt]) ->
        let x,Err(xerr) = errorEstimateAux xt env
        let y,Err(yerr) = errorEstimateAux yt env
        (x+y,Err(xerr+yerr))

    | SpecificCall <@@ (-) @@> (tyargs,_,[xt;yt]) ->
        let x,Err(xerr) = errorEstimateAux xt env
        let y,Err(yerr) = errorEstimateAux yt env
        (x-y,Err(xerr+yerr))

    | SpecificCall <@@ ( * ) @@> (tyargs,_,[xt;yt]) ->
        let x,Err(xerr) = errorEstimateAux xt env
        let y,Err(yerr) = errorEstimateAux yt env
        (x*y,Err(xerr*abs(y)+yerr*abs(x)+xerr*yerr))

    | SpecificCall <@@ abs @@> (tyargs,_,[xt]) ->
        let x,Err(xerr) = errorEstimateAux xt env
        (abs(x),Err(xerr))

    | Let(var,vet, bodyt) ->
        let varv,verr = errorEstimateAux vet env
        errorEstimateAux bodyt (env.Add(var,(varv,verr)))

    | Call(None,MethodWithReflectedDefinition(Lambda(v,body)),[arg]) ->
        errorEstimateAux  (Expr.Let(v,arg,body)) env

    | Var(x) -> env.[x]

    | Double(n) -> (n,Err(0.0))

    | _ -> failwithf "unrecognized term: %A" e

let rec errorEstimateRaw (t : Expr) =
    match t with
    | Lambda(x,t) ->
        (fun xv -> errorEstimateAux t (Map.ofSeq [(x,xv)]))
    | PropertyGet(None,PropertyGetterWithReflectedDefinition(body),[]) ->
        errorEstimateRaw body
    | _ -> failwithf "unrecognized term: %A - expected a lambda" t

let errorEstimate (t : Expr<float -> float>) = errorEstimateRaw t
//val errorEstimateAux : Expr -> Map<ExprVarName,(float * Error)> -> float * Error
//val errorEstimateRaw : Expr -> (float * Error -> float * Error)
//val errorEstimate : Expr<(float -> float)> -> (float * Error -> float * Error)

> let err x = Err x;;
//val err : float -> Error

> fsi.AddPrinter (fun (x:float,Err v) -> sprintf "%g±%g" x v);;
//val it : unit = ()

> errorEstimate <@ fun x -> x+2.0*x+3.0*x*x @> (1.0,err 0.1);;
//val it : float * Error = 6±0.61

> errorEstimate <@ fun x -> let y = x + x in y*y + 2.0 @> (1.0,err 0.1);;
//val it : float * Error = 6±0.84

[<ReflectedDefinition>]
let poly x = x+2.0*x+3.0/(x*x)

You can retrieve definitions like this using the MethodWithReflectedDefinition and PropertyGetterWithReflectedDefinition active patterns, as shown in Listing 9-11. You can now use this function in a regular <@ ... @> quotation and thus analyze it for errors:
> errorEstimate <@ poly @> (3.0, err 0.1);;
//val it : float * Error = 9.33333±0.582149

> errorEstimate <@ poly @> (30271.3, err 0.0001);;
//val it : float * Error = 90813.9±3.02723
