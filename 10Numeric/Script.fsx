#load "FSharpChart.fsx"
open MSDN.FSharp.Charting

let rnd = System.Random()
let rand() = rnd.NextDouble()

let randomPoints = [for i in 0 .. 1000 -> 10.0 * rand(), 10.0 * rand()]

randomPoints |> FSharpChart.Point

let randomTrend1 = [for i in 0.0 .. 0.1 .. 10.0 -> i, sin i + rand()]
let randomTrend2 = [for i in 0.0 .. 0.1 .. 10.0 -> i, sin i + rand()]

type Chart = FSharpChart
Chart.Combine [Chart.Line randomTrend1; Chart.Point randomTrend2]

FSharpChart.Line(randomPoints) |> FSharpChart.WithTitle "Expected Trend"

randomPoints
    |> FSharpChart.Line
    |> FSharpChart.WithTitle "Expected Trend"

true, false // System.Boolean
0uy, 19uy, 0xFFuy //System.Byte
0y, 19y, 0xFFy //System.SByte
0s, 19s, 0x0800s //System.Int16
0us, 19us, 0x0800us //System.UInt16
0, 19, 0x0800, 0b0001 //System.Int32
0u, 19u, 0x0800u //System.UInt32
0L, 19L, 0x0800L //System.Int64
0UL, 19UL, 0x0800UL //System.UInt64
0n, 19n, 0x0800n //System.IntPtr
0un, 19un, 0x0800un //System.UIntPtr
0.0f, 19.7f, 1.3e4f //System.Single
0.0, 19.7, 1.3e4 //System.Double
0M, 19M, 19.03M //System.Decimal
0I, 19I //System.Numerics.BigInteger
System.Numerics.Complex(2.0, 3.0) // System.Numerics.Complex

> 2147483647 + 1;;
//val it : int = -2147483648

sbyte (-17) //-17y
byte 255 //255uy
int16 0 //0s
uint16 65535 //65535us
int 17.8 //17
uint32 12 //12u
int64 (-100.4) //-100L
uint64 1 //1UL
decimal 65.3 //65.3M
float32 65 //65.0f
float 65 //65.0

abs (-10.0f) //10.0f
cos 0.0 //1.0
cosh 1.0 //1.543080635
acos 1.0 //0.0
ceil 1.001 //2.0
truncate 8.9 //8.0
exp 1.0 //2.718281828
2.0 ** 4.0 //16.0

sprintf "%X" (0x65 &&& 0x0F) //0x05
sprintf "%X" (0x65 ||| 0x18) //0x7D
sprintf "%X" (0x65 ^^^ 0x0F) //0x6A
sprintf "%X" (~~~0x65) //0xFFFFFF9a
sprintf "%X" (0x01 <<< 3) //0x08
sprintf "%X" (0x65 >>> 3) //0x0C

let encode (n : int32) =
    if (n >= 0 && n <= 0x7F) then [n]
    elif (n >= 0x80 && n <= 0x3FFF) then
        [(0x80 ||| (n >>> 8)) &&& 0xFF;
         (n &&& 0xFF)]
    else  [0xC0;
           ((n >>> 24) &&& 0xFF);
           ((n >>> 16) &&& 0xFF);
           ((n >>> 8) &&& 0xFF);
           (n &&& 0xFF)]
//val encode : n:int32 -> int32 list

> encode 32;;
//val it : int32 list = [32]

> encode 320;;
//val it : int32 list = [129; 64]

> encode 32000;;
//val it : int32 list = [192; 0; 0; 125; 0]

let rnd = new System.Random()
let rand() = rnd.NextDouble()
let data = [for i in 1 .. 1000 -> rand() * rand()]

let averageOfData = data |> Seq.average
let sumOfData = data |> Seq.sum
let maxOfData = data |> Seq.max
let minOfData = data |> Seq.min

// Random numbers, results may differ!
//val averageOfData : float = 0.2365092084
//val sumOfData : float = 236.5092084
//val maxOfData : float = 0.9457838635
//val minOfData : float = 6.153537535e-05

type RandomPoint = {X : float; Y : float; Z : float}

let random3Dpoints = 
    [for i in 1 .. 1000 -> {X = rand(); Y = rand(); Z = rand()}]

let averageX = random3Dpoints |> Seq.averageBy (fun p -> p.X)
let averageY = random3Dpoints |> Seq.averageBy (fun p -> p.Y)
let averageZ = random3Dpoints |> Seq.averageBy (fun p -> p.Z)

// Random numbers, results may differ!
//val averageX : float = 0.4910144491
//val averageY : float = 0.5001688922
//val averageZ : float = 0.5170302648

let maxY = random3Dpoints |> Seq.maxBy (fun p -> p.Y)

// Random numbers, results may differ!
//val maxY : RandomPoint = {X = 0.9829979292;
//                          Y = 0.9997189497;
//                          Z = 0.4552816481;}

// See http://en.wikipedia.org/wiki/Euclidean_distance, for a discussion of norm and distance.
let norm (p : RandomPoint) = sqrt (p.X * p.X + p.Y * p.Y + p.Z * p.Z)
let closest = random3Dpoints |> Seq.minBy (fun p -> norm p)

// Random numbers, results may differ!
//val norm : p:RandomPoint -> float
//val closest : RandomPoint = {X = 0.05287901873;
//                             Y = 0.0570056001;
//                             Z = 0.1018355787;}

let histogram = 
    random3Dpoints 
    |> Seq.countBy (fun p -> int (norm p * 10.0 / sqrt 3.0) ) 
    |> Seq.sortBy fst 
    |> Seq.toList

// Random numbers, results may differ!
//val closest : (int * int) list =
//  [(0, 1); (1, 15); (2, 52); (3, 97); (4, 173); (5, 233); (6, 256); (7, 116);
//   (8, 52); (9, 5)]

/// Compute the variance of an array of inputs
let variance (values : float[]) = 
    let sqr x = x * x
    let avg = values |> Array.average
    let sigma2 = values |> Array.averageBy (fun x -> sqr (x - avg))
    sigma2

let standardDeviation values =
    sqrt (variance values)

//val variance : values:float [] -> float
//val standardDeviation : values:float [] -> float

let sampleTimes = [|for x in 0 .. 1000 -> 50.0 + 10.0 * rand()|]
//val sampleTimes : float [] =
//  [|52.59674612; 50.09170097; 53.42933899; 59.10801735; 53.18131675; ...|]

let exampleDeviation = standardDeviation sampleTimes
let exampleVariance = variance sampleTimes
//val exampleDeviation : float = 2.870294413
//val exampleVariance : float = 8.238590017

module Seq = 
    /// Compute the variance of the given statistic from from the input data
    let varianceBy (f : 'T -> float) values = 
        let sqr x = x * x
        let xs = values |> Seq.map f |> Seq.toArray
        let avg = xs |> Array.average
        let res = xs |> Array.averageBy (fun x -> sqr (x - avg))
        res

    /// Compute the standard deviation of the given statistic drawn from the input data
    let standardDeviationBy f values =
        sqrt (varianceBy f values)
//module Seq = begin
//  val varianceBy : f:('T -> float) -> values:seq<'T> -> float
//  val standardDeviationBy : f:('a -> float) -> values:seq<'a> -> float
//end

let inline variance values = 
    let sqr x = x * x
    let avg = values |> Array.average
    let sigma2 = values |> Array.averageBy (fun x -> sqr (x - avg))
    sigma2 

let inline standardDeviation values =
    sqrt (variance values)

//val inline variance :
//  values: ^a [] ->  ^c
//    when  ^a : (static member ( + ) :  ^a *  ^a ->  ^a) and
//          ^a : (static member DivideByInt :  ^a * int ->  ^a) and
//          ^a : (static member get_Zero : ->  ^a) and
//          ^a : (static member ( - ) :  ^a *  ^a ->  ^b) and
//          ^b : (static member ( * ) :  ^b *  ^b ->  ^c) and
//          ^c : (static member ( + ) :  ^c *  ^c ->  ^c) and
//          ^c : (static member DivideByInt :  ^c * int ->  ^c) and
//          ^c : (static member get_Zero : ->  ^c)
//val inline standardDeviation :
//  values: ^a [] ->  ^d
//    when  ^a : (static member ( + ) :  ^a *  ^a ->  ^a) and
//          ^a : (static member DivideByInt :  ^a * int ->  ^a) and
//          ^a : (static member get_Zero : ->  ^a) and
//          ^a : (static member ( - ) :  ^a *  ^a ->  ^b) and
//          ^b : (static member ( * ) :  ^b *  ^b ->  ^c) and
//          ^c : (static member ( + ) :  ^c *  ^c ->  ^c) and
//          ^c : (static member DivideByInt :  ^c * int ->  ^c) and
//          ^c : (static member get_Zero : ->  ^c) and
//          ^c : (static member Sqrt :  ^c ->  ^d)

type Input<'T> = {Data : 'T; Features : float[]}
type Centroid = float[]

module Array = 
    /// Like Seq.groupBy, but returns arrays 
    let classifyBy f (xs : _[]) = 
         xs |> Seq.groupBy f |> Seq.map (fun (k, v) -> (k, Seq.toArray v)) |> Seq.toArray

module Seq = 
    /// Return x, f(x), f(f(x)), f(f(f(x))), ...
    let iterate f x = x |> Seq.unfold (fun x -> Some (x, f x))

/// Compute the norm distance between an input and a centroid
let distance (xs : Input<_>) (ys : Centroid) =
    (xs.Features,ys) 
        ||> Array.map2 (fun x y -> (x - y) * (x - y))
        |> Array.sum

/// Find the average of set of inputs. First compute xs1 + ... + xsN, pointwise, 
/// then divide each element of the sum by the number of inputs.
let computeCentroidOfGroup (_, group : Input<_>[]) =
    let e0 = group.[0].Features
    [|for i in 0 .. e0.Length - 1 -> group |> Array.averageBy (fun e -> e.Features.[i])|]

/// Group all the inputs by the nearest centroid
let classifyIntoGroups inputs centroids = 
    inputs |> Array.classifyBy (fun v -> centroids |> Array.minBy (distance v))

/// Repeatedly classify the inputs, starting with the initial centroids
let rec computeCentroids inputs centroids = seq {
    let classification = classifyIntoGroups inputs centroids
    yield classification
    let newCentroids = Array.map computeCentroidOfGroup classification
    yield! computeCentroids inputs newCentroids}

/// Extract the features and repeatedly classify the inputs, starting with the 
/// initial centroids
let kmeans inputs featureExtractor initialCentroids = 
    let inputs = 
        inputs 
        |> Seq.map (fun i -> {Data = i; Features = featureExtractor i}) 
        |> Seq.toArray
    let initialCentroids = initialCentroids |> Seq.toArray
    computeCentroids inputs initialCentroids
//type Input<'T> =
//  {Data: 'T;
//   Features: float [];}
//type Centroid = float []
//module Array = begin
//  val classifyBy :
//    f:('a -> 'b) -> xs:'a [] -> ('b * 'a []) [] when 'b : equality
//end
//module Seq = begin
//  val iterate : f:('a -> 'a) -> x:'a -> seq<'a>
//end
//val distance : xs:Input<'a> -> ys:Centroid -> float
//val computeCentroidOfGroup : 'a * group:Input<'b> [] -> float []
//val classifyIntoGroups :
//  inputs:Input<'a> [] -> centroids:Centroid [] -> (Centroid * Input<'a> []) []
//val computeCentroids :
//  inputs:Input<'a> [] ->
//    centroids:Centroid [] -> seq<(Centroid * Input<'a> []) []>
//val kmeans :
//  inputs:seq<'a> ->
//    featureExtractor:('a -> float []) ->
//      initialCentroids:seq<Centroid> -> seq<(Centroid * Input<'a> []) []>

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

type Observation = {Time : float<s>; Location : float<m>}

let rnd = System.Random()
let rand() = rnd.NextDouble() 
let randZ() = rnd.NextDouble() - 0.5

/// Create a point near the given point
let near p = {Time= p.Time + randZ() * 20.0<s>; 
              Location = p.Location + randZ() * 5.0<m>}

let data = 
    [for i in 1 .. 1000 -> near {Time= 100.0<s>; Location = 60.0<m>}
     for i in 1 .. 1000 -> near {Time= 120.0<s>; Location = 80.0<m>}
     for i in 1 .. 1000 -> near {Time= 180.0<s>; Location = 30.0<m>}
     for i in 1 .. 1000 -> near {Time= 70.0<s>; Location = 40.0<m>}]

let maxTime = data |> Seq.maxBy (fun p -> p.Time) |> fun p -> p.Time
let maxLoc = data |> Seq.maxBy (fun p -> p.Location) |> fun p -> p.Location

let initialCentroids = [for i in 0 .. 9 -> [|rand(); rand()|]]
let featureExtractor (p : Observation) = [|p.Time / maxTime; p.Location / maxLoc|]

kmeans data featureExtractor initialCentroids

kmeans data featureExtractor initialCentroids
   |> Seq.map (Array.map (fun (c, _) -> c.[0] * maxTime, c.[1] * maxLoc))
   |> Seq.nth 100 

//> val it : (float<s> * float<m>) [] =
//  [|(95.21297664, 59.92134092); (105.128182, 60.03017317);
//    (120.0522592, 79.99954457); (179.7447501, 29.96446713);
//    (65.97080136, 38.75120135); (75.63604991, 40.05351476);
//    (64.8228706, 41.26265782)|]

#r @".\packages\MathNet.Numerics.2.1.2\lib\Net40\MathNet.Numerics.dll"
#r @".\packages\MathNet.Numerics.FSharp.2.1.2\lib\Net40\MathNet.Numerics.FSharp.dll"
#r @".\packages\zlib.net.1.0.4.0\lib\zlib.net.dll"

open MathNet.Numerics.Statistics 

let data = [for i in 0.0 .. 0.01 .. 10.0 -> sin i] 

let exampleVariance = data |> Statistics.Variance 
let exampleMean = data |> Statistics.Mean 
let exampleMin = data |> Statistics.Minimum
let exampleMax = data |> Statistics.Maximum

//val exampleVariance : float = 0.443637494
//val exampleMean : float = 0.1834501596
//val exampleMin : float = -0.9999971464
//val exampleMax : float = 0.9999996829

open MathNet.Numerics.Distributions 
open System.Collections.Generic

let exampleBellCurve = Normal(100.0, 10.0)
//val exampleBellCurve : MathNet.Numerics.Distributions.Normal =
//  Normal(Mean = 100, StdDev = 10)

> exampleBellCurve.Samples();;
//val it : IEnumerable<float> =
//  seq [102.4361311; 88.10527203; 100.1478871; 88.2344663; ...]

let histogram n data = 
    let h = Histogram(data, n)
    [|for i in 0 .. h.BucketCount - 1 -> 
          (sprintf "%.0f-%.0f" h.[i].LowerBound h.[i].UpperBound, h.[i].Count)|]
//val histogram : n:int -> data:IEnumerable<float> -> (string * float) []

exampleBellCurve.Samples() 
    |> Seq.truncate 1000 
    |> histogram 10 
    |> Chart.Column

// Reset the F# interfactive session here.
#r @".\packages\MathNet.Numerics.2.1.2\lib\Net40\MathNet.Numerics.dll"
#r @".\packages\MathNet.Numerics.FSharp.2.1.2\lib\Net40\MathNet.Numerics.FSharp.dll"
#r @".\packages\zlib.net.1.0.4.0\lib\zlib.net.dll"
open MathNet.Numerics.FSharp
open MathNet.Numerics.LinearAlgebra.Generic

let vector1 = vector [1.0; 2.4; 3.0]
let vector2 = vector [7.0; 2.1; 5.4]
//val vector1 : Vector<float>
//val vector2 : Vector<float>

vector1 + vector2;;
//val it : Vector<float> = seq [8.0; 4.5; 8.4]

let matrix1 = matrix [[1.0; 2.0]; [1.0; 3.0]];;
let matrix2 = matrix [[1.0; -2.0]; [0.5; 3.0]];;
let ``matrix1*2`` = matrix1 * matrix2;;
//val matrix1 : Matrix<float> = 1,2
//1,3
//val matrix2 : Matrix<float> = 1,-2
//0.5,3
//val ( matrix1*2 ) : Matrix<float> = 2,4
//2.5,7

open MathNet.Numerics.LinearAlgebra.Double 

fsi.AddPrintTransformer (fun (x : DenseVector) -> 
     box [|for i in 0 .. x.Count - 1 -> x.[i]|])

fsi.AddPrintTransformer (fun (x : DenseMatrix) -> 
     box (array2D [for i in 0 .. x.RowCount - 1 -> 
                       [for j in 0 .. x.ColumnCount - 1 -> x.[i, j]]]))

//val vector1 : Vector<float> = [|1.0; 2.4; 3.0|]
//val vector2 : Vector<float> = [|7.0; 2.1; 5.4|]
//val it : Vector<float> = seq [8.0; 4.5; 8.4]
//
//val matrix1 : Matrix<float> = [[1.0; 2.0]
//                               [1.0; 3.0]]
//val matrix2 : Matrix<float> = [[1.0; -2.0]
//                               [0.5; 3.0]]
//val ( matrix1*2 ) : Matrix<float> = [[2.0; 4.0]
//                                     [2.5; 7.0]]

// Reset the F# interfactive session here.
#r @".\packages\MathNet.Numerics.2.1.2\lib\Net40\MathNet.Numerics.dll"
#r @".\packages\MathNet.Numerics.FSharp.2.1.2\lib\Net40\MathNet.Numerics.FSharp.dll"
#r @".\packages\zlib.net.1.0.4.0\lib\zlib.net.dll"
open MathNet.Numerics.FSharp
open System

// From earlier in this file. I repeat it here so I can rebind after
// resetting the F# interactive session.
let rnd = System.Random()
let rand() = rnd.NextDouble()

let largeMatrix = matrix [for i in 1 .. 100 -> [for j in 1 .. 100 -> rand()]]

let inverse = largeMatrix.Inverse()
let check = largeMatrix * largeMatrix.Inverse()
//val check : Matrix<float> =
//  [[1.0; 2.775557562e-16; 4.371503159e-15; 1.887379142e-15; 1.887379142e-15;
//    -1.054711873e-15; -2.664535259e-15; 6.689093723e-15; 4.440892099e-16;
//    -5.551115123e-16; -2.664535259e-15; -5.551115123e-16; 2.664535259e-15;
//    -2.220446049e-15; 1.110223025e-16; -1.776356839e-15; -2.886579864e-15; 
//    ...

// Open this namespace so we can use the Evd extension method.
open MathNet.Numerics.LinearAlgebra.Double
open MathNet.Numerics.LinearAlgebra.Generic

let evd = largeMatrix.Evd()
let eigenValues = evd.EigenValues()
//val evd : Factorization.Evd
//val eigenValues : Vector<Numerics.Complex>
//val it : Vector<Numerics.Complex> =
//  seq
//    [(49.9175253943554, 0) {Imaginary = 0.0;
//                            Magnitude = 49.91752539;
//                            Phase = 0.0;
//                            Real = 49.91752539;};
//     (2.80519107014208, 0) {Imaginary = 0.0;
//                            Magnitude = 2.80519107;
//                            Phase = 0.0;
//                            Real = 2.80519107;};
//     (1.6469180515329, 2.22222217981812) {Imaginary = 2.22222218;
//                                          Magnitude = 2.765973696;
//                                          Phase = 0.9330071135;
//                                          Real = 1.646918052;};
//     (1.6469180515329, -2.22222217981812) {Imaginary = -2.22222218;
//                                           Magnitude = 2.765973696;
//                                           Phase = -0.9330071135;
//                                           Real = 1.646918052;}; ...]

fsi.AddPrinter (fun (c : System.Numerics.Complex) -> sprintf "%fr + %fi" c.Real c.Imaginary)
//val it : Vector<Numerics.Complex> =
//  seq
//    [49.917525r + 0.000000i; 2.805191r + 0.000000i; 1.646918r + 2.222222i;
//     1.646918r + -2.222222i; ...]


[<Measure>] 
type click

[<Measure>] 
type pixel

[<Measure>] 
type money
//[<Measure>]
//type click
//[<Measure>]
//type pixel
//[<Measure>]

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

let rateOfClicks = 200.0<click/s>
let durationOfExecution = 3.5<s>
let numberOfClicks = rateOfClicks * durationOfExecution 
//val rateOfClicks : float<click/Data.UnitSystems.SI.UnitSymbols.s> = 200.0
//val durationOfExecution : float<Data.UnitSystems.SI.UnitSymbols.s> = 3.5
//val numberOfClicks : float<click> = 700.0

let integrateByMidpointRule f (a, b) = (b - a) * f ((a + b) / 2.0)

let integrateByTrapezoidalRule f (a, b) = (b - a) * ((f a + f b) / 2.0)

let integrateByIterativeRule f (a, b) n = 
    (b - a) / float n * 
    ((f a + f b) / 2.0 + 
      List.sum [for k in 1 .. n - 1 -> f (a + float k * (b - a) / float n)])
//val integrateByMidpointRule : f:(float -> float) -> a:float * b:float -> float
//val integrateByTrapezoidalRule :
//  f:(float -> float) -> a:float * b:float -> float
//val integrateByIterativeRule :
//  f:(float -> float) -> a:float * b:float -> n:int -> float

let integrateByMidpointRule (f : float<'u> -> float<'v>) (a : float<'u>, b : float<'u>) = 
    (b - a) * f ( (a+b) / 2.0)

let integrateByTrapezoidalRule (f : float<'u> -> float<'v>) (a : float<'u>, b : float<'u>) = 
    (b - a) * ((f a + f b) / 2.0)

let integrateByIterativeRule (f : float<'u> -> float<'v>) (a : float<'u>, b : float<'u>) n = 
    (b - a) / float n * 
    ((f a + f b) / 2.0 + 
      List.sum [for k in 1 .. n - 1 -> f (a + float k * (b - a) / float n)])
//val integrateByMidpointRule :
//  f:(float<'u> -> float<'v>) -> a:float<'u> * b:float<'u> -> float<'u 'v>
//val integrateByTrapezoidalRule :
//  f:(float<'u> -> float<'v>) -> a:float<'u> * b:float<'u> -> float<'u 'v>
//val integrateByIterativeRule :
//  f:(float<'u> -> float<'v>) ->
//    a:float<'u> * b:float<'u> -> n:int -> float<'u 'v>

let velocityFunction (t : float<s>) = 100.0<m/s> + t * -9.81<m/s^2>

let distance1 = integrateByMidpointRule velocityFunction (0.0<s>, 10.0<s>)
let distance2 = integrateByTrapezoidalRule velocityFunction (0.0<s>, 10.0<s>)
let distance3 = integrateByIterativeRule velocityFunction (0.0<s>, 10.0<s>) 10
//val velocityFunction : t:float<s> -> float<m/s>
//val distance1 : float<m> = 509.5
//val distance2 : float<m> = 509.5
//val distance3 : float<m> = 509.5

let variance (values : seq<float<_>>) = 
    let sqr x = x * x
    let xs = values |> Seq.toArray
    let avg = xs |> Array.average
    let variance = xs |> Array.averageBy (fun x -> sqr (x - avg))
    variance

let standardDeviation values =
    sqrt (variance values)
//val variance : values:seq<float<'u>> -> float<'u ^ 2>
//val standardDeviation : values:seq<float<'u>> -> float<'u>

let sampleTimes = [for x in 0 .. 1000 -> 50.0<s> + 10.0<s> * rand()]

let exampleDeviation = standardDeviation sampleTimes
let exampleVariance = variance sampleTimes
//val exampleDeviation : float<s> = 2.865331753
//val exampleVariance : float<s ^ 2> = 8.210126054

type Vector2D<[<Measure>] 'u> = {DX : float<'u>; DY : float<'u>}

/// Two-dimensional vectors
type Vector2D<[<Measure>] 'u>(dx : float<'u>, dy : float<'u>) =

    /// Get the X component of the vector
    member v.DX = dx    
    
    /// Get the Y component of the vector
    member v.DY = dy    
    
    /// Get the length of the vector
    member v.Length = sqrt(dx * dx + dy * dy)
    
    /// Get a vector scaled by the given factor
    member v.Scale k = Vector2D(k * dx, k * dy)
    
    /// Return a vector shifted by the given delta in the X coordinate
    member v.ShiftX x = Vector2D(dx + x, dy)

    /// Return a vector shifted by the given delta in the Y coordinate
    member v.ShiftY y = Vector2D(dx, dy + y)

    /// Get the zero vector
    static member Zero = Vector2D<'u>(0.0<_>, 0.0<_>)

    /// Return a constant vector along the X axis
    static member ConstX dx = Vector2D<'u>(dx, 0.0<_>)

    /// Return a constant vector along the Y axis
    static member ConstY dy = Vector2D<'u>(0.0<_>, dy)

    /// Return the sum of two vectors
    static member (+) (v1 : Vector2D<'u>, v2 : Vector2D<'u>) = 
        Vector2D(v1.DX + v2.DX, v1.DY + v2.DY)

    /// Return the difference of two vectors
    static member (-) (v1 : Vector2D<'u>, v2 : Vector2D<'u>) = 
        Vector2D(v1.DX - v2.DX, v1.DY - v2.DY)

    /// Return the pointwise-product of two vectors
    static member (.*) (v1 : Vector2D<'u>, v2 : Vector2D<'u>) = 
        Vector2D(v1.DX * v2.DX, v1.DY * v2.DY)
//type Vector2D<'u> =
//  class
//    new : dx:float<'u> * dy:float<'u> -> Vector2D<'u>
//    member Scale : k:float<'u0> -> Vector2D<'u 'u0>
//    member ShiftX : x:float<'u> -> Vector2D<'u>
//    member ShiftY : y:float<'u> -> Vector2D<'u>
//    member DX : float<'u>
//    member DY : float<'u>
//    member Length : float<'u>
//    static member ConstX : dx:float<'u> -> Vector2D<'u>
//    static member ConstY : dy:float<'u> -> Vector2D<'u>
//    static member Zero : Vector2D<'u>
//    static member ( + ) : v1:Vector2D<'u> * v2:Vector2D<'u> -> Vector2D<'u>
//    static member
//      ( .* ) : v1:Vector2D<'u> * v2:Vector2D<'u> -> Vector2D<'u ^ 2>
//    static member ( - ) : v1:Vector2D<'u> * v2:Vector2D<'u> -> Vector2D<'u>
//  end

let three = float 3.0<kg>
let sixKg = LanguagePrimitives.FloatWithMeasure<kg> (three + three)
//val three : float = 3.0
//val sixKg : float<kg> = 6.0

type Input<'T, [<Measure>] 'u> = 
    {Data : 'T 
     Features: float<'u>[]}
//type Input<'T,'u> =
//  {Data: 'T;
//   Features: float<'u> [];}

type Centroid<[<Measure>] 'u> = float<'u>[]
//type Centroid<'u> = float<'u> []

let distance (xs : Input<_, _>) (ys : Centroid<_>) = …

let computeCentroidOfGroup (_, group : Input<_>[]) =