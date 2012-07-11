#load "FSharpChart.fsx"
open MSDN.FSharp.Charting

let rnd = System.Random()
let rand() = rnd.NextDouble()

let randomPoints = [ for i in 0 .. 1000 -> 10.0 * rand(), 10.0 * rand() ]

randomPoints |> Chart.Point

let randomTrend1 = [ for i in 0.0 ..0.1.. 10.0 -> i, sin i + rand()]
let randomTrend2 = [ for i in 0.0 ..0.1.. 10.0 -> i, sin i + rand()]

Chart.Combine [ Chart.Line randomTrend1; Chart.Point randomTrend2 ]

randomPoints 
    |> fun c -> Chart.Line (c,Title="Expected Trend") 

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

> 2147483647+1;;
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

0x65 &&& 0x0F //0x05
0x65 ||| 0x18 //0x7D
0x65 ˆˆˆ 0x0F //0x6A
~~~0x65 //0xFFFFFF9a
0x01 <<< 3 //0x08
0x65 >>> 3 //0x0C

let encode (n: int32) =
    if   (n >= 0    && n <= 0x7F)   then [ n ]
    elif (n >= 0x80 && n <= 0x3FFF) then [ (0x80 ||| (n >>> 8)) &&& 0xFF;
                                           (n &&& 0xFF) ]
    else  [ 0xC0; ((n >>> 24) &&& 0xFF);
                  ((n >>> 16) &&& 0xFF);
                  ((n >>> 8)  &&& 0xFF);
                   (n         &&& 0xFF) ]

> encode 32;;
//val it : int32 list = [32]

> encode 320;;
//val it : int32 list = [129; 64]

> encode 32000;;
//val it : int32 list = [192; 0; 0; 125; 0]

let rnd = new System.Random()
let rand() = rnd.NextDouble()
let data = [ for i in 1 .. 1000 -> rand() * rand() ]

let averageOfData = data |> Seq.average
let sumOfData = data |> Seq.sum
let maxOfData = data |> Seq.max
let minOfData = data |> Seq.min

// Random numbers, results may differ!
//val averageOfData : float = 0.2365092084
//val sumOfData : float = 236.5092084
//val maxOfData : float = 0.9457838635
//val minOfData : float = 6.153537535e-05

type RandomPoint = { X: float; Y: float; Z: float }

let random3Dpoints = 
    [ for i in 1 .. 1000 -> { X=rand();Y=rand();Z=rand() } ]

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

let norm (p: RandomPoint) = p.X * p.X + p.Y * p.Y + p.Z * p.Z
let closest = random3Dpoints |> Seq.minBy (fun p -> norm p)

// Random numbers, results may differ!
//val norm : p:RandomPoint -> float
//val closest : RandomPoint = {X = 0.05287901873;
//                             Y = 0.0570056001;
//                             Z = 0.1018355787;}

let histogram = 
    random3Dpoints 
    |> Seq.countBy (fun p -> int (distance p * 10.0 / sqrt 3.0) ) 
    |> Seq.sortBy fst 
    |> Seq.toList

// Random numbers, results may differ!
//val closest : (int * int) list =
//  [(0, 1); (1, 15); (2, 52); (3, 97); (4, 173); (5, 233); (6, 256); (7, 116);
//   (8, 52); (9, 5)]

/// Compute the variance of an array of inputs
let variance (values:float[]) = 
    let avg = values |> Array.average
    let sigma2 = values |> Array.averageBy (fun x -> sqr (x - avg))
    sigma2

let standardDeviation values =
    sqrt (variance values)

//val variance : float [] -> float
//val standardDeviation : float [] -> float

let sampleTimes = [ for x in 0 .. 1000 -> 50.0 + 10.0 * rand() ]

let exampleDeviation = standardDeviation  sampleTimes
let exampleVariance = variance sampleTimes

//val exampleDeviation : float = 2.865331753
//val exampleVariance : float = 8.210126054

module Seq = 
    /// Compute the variance of the given statistic from from the input data
    let varianceBy (f: 'T -> float) values = 
        let xs = values |> Seq.map f |> Seq.toArray
        let avg = xs |> Array.average
        let res = xs |> Array.averageBy (fun x -> sqr (x - avg))
        res

    /// Compute the standard deviation of the given statistic drawn from the input data
    let standardDeviationBy f values =
        sqrt (varianceBy f values)

let inline variance values = 
    let avg = values |> Array.average
    let sigma2 = values |> Array.averageBy (fun x -> sqr (x - avg))
    sigma2 

let inline standardDeviation values =
    sqrt (variance values)

//val inline variance :
//  values: ^a [] ->  ^c
//    when  ^a : (static member ( + ) :  ^a *  ^a ->  ^a) and
//          ^a : (static member DivideByInt :  ^a * int ->  ^a) and
//          ^a : (static member Zero : ^a) and
//          ^a : (static member ( - ) :  ^a *  ^a ->  ^b) and
//          ^b : (static member ( * ) :  ^b *  ^b ->  ^c) and
//          ^c : (static member ( + ) :  ^c *  ^c ->  ^c) and
//          ^c : (static member DivideByInt :  ^c * int ->  ^c) and
//          ^c : (static member get_Zero : ->  ^c)
//val inline standardDeviation :
//  values: ^a [] ->  ^d
//    when  ^a : (static member ( + ) :  ^a *  ^a ->  ^a) and
//          ^a : (static member DivideByInt :  ^a * int ->  ^a) and
//          ^a : (static member Zero : ^a) and
//          ^a : (static member ( - ) :  ^a *  ^a ->  ^b) and
//          ^b : (static member ( * ) :  ^b *  ^b ->  ^c) and
//          ^c : (static member ( + ) :  ^c *  ^c ->  ^c) and
//          ^c : (static member DivideByInt :  ^c * int ->  ^c) and
//          ^c : (static member get_Zero : ->  ^c) and
//          ^c : (static member Sqrt :  ^c ->  ^d)

module Samples.KMeans

type internal Input<'T> = 
   { Data : 'T 
     Features: float[] }

type Centroid = float[]

module Array = 
    /// Like Seq.groupBy, but returns arrays 
    let classifyBy f (xs:_[]) = 
         xs |> Seq.groupBy f |> Seq.map (fun (k,v) -> (k,Seq.toArray v)) |> Seq.toArray

module Seq = 
    /// Return x, f(x), f(f(x)), f(f(f(x))), ...
    let iterate f x = x |> Seq.unfold (fun x -> Some (x, f x))

/// Compute the norm distance between an input and a centroid
let distance (xs:Input<_>) (ys:Centroid) =
    (xs.Features,ys) 
        ||> Array.map2 (fun x y -> (x - y) * (x - y))
        |> Array.sum

/// Find the average of set of inputs. First compute xs1 + ... + xsN, pointwise, 
/// then divide each element of the sum by the number of inputs.
let computeCentroidOfGroup (_,group:Input<_>[]) =
    let e0 = group.[0].Features
    [| for i in 0 .. e0.Length - 1 -> group |> Array.averageBy (fun e -> e.Features.[i]) |]

/// Group all the inputs by the nearest centroid
let classifyIntoGroups inputs centroids = 
    inputs |> Array.classifyBy (fun v -> centroids |> Array.minBy (distance v))

/// Repeatedly classify the inputs, starting with the initial centroids
let rec computeCentroids inputs centroids = 
    seq { let classification = classifyIntoGroups inputs centroids
          yield classification
          let newCentroids = Array.map computeCentroidOfGroup classification
          yield! computeCentroids inputs newCentroids }

/// Extract the features and repeatedly classify the inputs, starting with the 
/// initial centroids
let kmeans inputs featureExtractor initialCentroids = 
    let inputs = 
        inputs 
        |> Seq.map (fun i -> { Data = i; Features = featureExtractor i }) 
        |> Seq.toArray
    let initialCentroids = initialCentroids |> Seq.toArray
    computeCentroids inputs initialCentroids

open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

type Observation = { Time: float<s>; Location: float<m> }

let rnd = System.Random()
let rand() = rnd.NextDouble() 
let randZ() = rnd.NextDouble() - 0.5

/// Create a point near the given point
let near p = { Time= p.Time + randZ() * 20.0<s>; 
               Location = p.Location + randZ() * 5.0<m> }

let data = 
     [ for i in 1 .. 1000 -> near { Time= 100.0<s>; Location = 60.0<m> }
       for i in 1 .. 1000 -> near { Time= 120.0<s>; Location = 80.0<m> }
       for i in 1 .. 1000 -> near { Time= 180.0<s>; Location = 30.0<m> }
       for i in 1 .. 1000 -> near { Time= 70.0<s>; Location = 40.0<m> } ]

let maxTime = data |> Seq.maxBy (fun p -> p.Time) |> fun p -> p.Time
let maxLoc = data |> Seq.maxBy (fun p -> p.Location) |> fun p -> p.Location

let initialCentroids = [ for i in 0 .. 9 -> [| rand(); rand() |] ]
let featureExtractor (p:Observation) = [| p.Time / maxTime; p.Location / maxLoc |]

kmeans data featureExtractor initialCentroids
   |> Seq.map (Array.map (fun (c,_) -> c.[0] * maxTime, c.[1] * maxLoc))
   |> Seq.nth 100 

//> val it : (float<s> * float<m>) [] =
//  [|(95.21297664, 59.92134092); (105.128182, 60.03017317);
//    (120.0522592, 79.99954457); (179.7447501, 29.96446713);
//    (65.97080136, 38.75120135); (75.63604991, 40.05351476);
//    (64.8228706, 41.26265782)|]

#r @"c:\proj\Mat\packages\MathNet.Numerics.2.1.2\lib\Net40\MathNet.Numerics.dll"
#r @"c:\proj\Mat\packages\MathNet.Numerics.FSharp.2.1.2\lib\Net40\MathNet.Numerics.FSharp.dll"
#r @"c:\proj\Mat\packages\zlib.net.1.0.4.0\lib\zlib.net.dll"

open MathNet.Numerics.Statistics 

let data = [ for i in 0.0 .. 0.01 .. 10.0 -> sin i ] 

let exampleVariance = data |> Statistics.Variance 
let exampleMean = data |> Statistics.Mean 
let exampleMin = data |> Statistics.Minimum
let exampleMax = data |> Statistics.Maximum

//val exampleVariance : float = 0.443637494
//val exampleMean : float = 0.1834501596
//val exampleMin : float = -0.9999971464
//val exampleMax : float = 0.9999996829

open MathNet.Numerics.Distributions 

let exampleBellCurve = Normal(100.0, 10.0)

> exampleBellCurve.Samples();;
//val it : IEnumerable<float> =
//  seq [102.4361311; 88.10527203; 100.1478871; 88.2344663; ...]

let histogram n data = 
    let h = Histogram(data, n)
    [| for i in 0 .. h.BucketCount - 1 -> 
         (sprintf "%.0f-%.0f" h.[i].LowerBound h.[i].UpperBound, h.[i].Count) |]

exampleBellCurve.Samples() 
    |> Seq.truncate 1000 
    |> histogram 10 
    |> Chart.Column

let vector1 = vector [ 1.0; 2.4 ; 3.0 ] 
let vector2 = vector [ 7.0; 2.1 ; 5.4 ]

vector1 + vector2
//val vector1 : Vector<float> = 1,2.4,3
//val vector2 : Vector<float> = 7,2.1,5.4
//val it : Vector<float> = 8,4.5,8.4

vector [ 1.0; 2.0 ; 3.0 ] + vector [ 1.0; 2.0 ; 3.0 ]
//val matrix1 : Matrix<float> = 1,2
//1,3
//val matrix2 : Matrix<float> = 1,-2
//0.5,3
//val it : Matrix<float> = 2,4
//2.5,7

fsi.AddPrintTransformer (fun (x:DenseVector) -> 
     box [| for i in 0 .. x.Count-1 -> x.[i] |])

fsi.AddPrintTransformer (fun (x:DenseMatrix) -> 
     box (array2D [ for i in 0 .. x.RowCount-1 -> 
                        [ for j in 0 .. x.ColumnCount - 1 -> x.[i,j] ] ]))
//val vector1 : Vector<float> = [|1.0; 2.4; 3.0|]
//val vector2 : Vector<float> = [|7.0; 2.1; 5.4|]
//val it : Vector<float> = [|8.0; 4.5; 8.4|]
//val matrix1 : Matrix<float> = [[1.0; 2.0]
//                               [1.0; 3.0]]
//val matrix2 : Matrix<float> = [[1.0; -2.0]
//                               [0.5; 3.0]]
//val it : Matrix<float> = [[2.0; 4.0]
//                          [2.5; 7.0]]

let largeMatrix = matrix [ for i in 1 .. 100 -> [ for j in 1 .. 100 -> rand() ] ]

let inverse = largeMatrix.Inverse()
let check = largeMatrix * largeMatrix.Inverse()
//val check : Matrix<float> =
//  [[1.0; 2.775557562e-16; 4.371503159e-15; 1.887379142e-15; 1.887379142e-15;
//    -1.054711873e-15; -2.664535259e-15; 6.689093723e-15; 4.440892099e-16;
//    -5.551115123e-16; -2.664535259e-15; -5.551115123e-16; 2.664535259e-15;
//    -2.220446049e-15; 1.110223025e-16; -1.776356839e-15; -2.886579864e-15; 
//    ...

let evd = largeMatrix.Evd()
let eigenValues = evd.EigenValues()
//val eigenValues : Vector< Complex> =
//  seq
//    [(49.9915503772098, 0);
//     (-0.406120558380603, 2.92962073514449);
//    ...

fsi.AddPrinter (fun (c:System.Numerics.Complex) -> sprintf "%fr + %fi" c.Real c.Imaginary)
//val eigenValues : Vector< Complex> =
//  seq
//    [49.991550r + 0.000000i; -0.406121r + 2.929621i; -0.406121r + -2.929621i;
//     3.095556r + 0.000000i; ...]
//    ...

[<Measure>] 
type click

[<Measure>] 
type pixel

[<Measure>] 
type money

open Microsoft.FSharp.Data.UnitSystems.SI.UnitNames
open Microsoft.FSharp.Data.UnitSystems.SI.UnitSymbols

let rateOfClicks = 200.0<click/s>
let durationOfExecution = 3.5<s>
let numberOfClicks = rateOfClicks * durationOfExecution 
//val numberOfClicks: float<click> = 700.0

let integrateByMidpointRule f (a,b) = (b - a) * f ( (a+b) / 2.0)

let integrateByTrapezoidalRule f (a,b) = (b - a) * ( (f a + f b) / 2.0)

let integrateByIterativeRule f (a,b) n = 
    (b - a) / float n * 
    ( (f a + f b) / 2.0 + 
      List.sum [ for k in 1 .. n-1 -> f (a + float k * (b - a) / float n) ])

let integrateByMidpointRule (f: float<'u> -> float<'v>) (a:float<'u>, b:float<'u>) = 
    (b-a) * f ( (a+b) / 2.0)

let integrateByTrapezoidalRule (f: float<'u> -> float<'v>) (a:float<'u>, b:float<'u>) = 
    (b-a) * ( (f a + f b) / 2.0)

let integrateByIterativeRule (f: float<'u> -> float<'v>) (a:float<'u>, b:float<'u>) n = 
    (b-a) / float n * 
    ( (f a + f b) / 2.0 + 
      List.sum [ for k in 1 .. n-1 -> f (a + float k * (b - a) / float n) ])
//val integrateByMidpointRule : 
//       (float<'u> -> float<'v>) -> float<'u> * float<'u> -> float<'u 'v>
//val integrateByTrapezoidalRule : 
//       (float<'u> -> float<'v>) -> float<'u> * float<'u> -> float<'u 'v>
//val integrateByIterativeRule :
//       (float<'u> -> float<'v>) -> float<'u> * float<'u> -> int -> float<'u 'v>

let velocityFunction (t:float<s>) = 100.0<m/s> + t * -9.81<m/s^2>

let distance1 = integrateByMidpointRule    velocityFunction (0.0<s>, 10.0<s>)
let distance2 = integrateByTrapezoidalRule velocityFunction (0.0<s>, 10.0<s>)
let distance3 = integrateByIterativeRule   velocityFunction (0.0<s>, 10.0<s>) 10
//val distance1 : float<m> = 509.5
//val distance2 : float<m> = 509.5
//val distance3 : float<m> = 509.5

let variance (values: seq<float<_>>) = 
    let xs = values |> Seq.toArray
    let avg = xs |> Array.average
    let variance = xs |> Array.averageBy (fun x -> sqr (x - avg))
    variance

let standardDeviation values =
    sqrt (variance values)
//val variance : values:seq<float<'u>> -> float<'u ^ 2>
//val standardDeviation : values:seq<float<'u>> -> float<'u>

let sampleTimes = [ for x in 0 .. 1000 -> 50.0<s> + 10.0<s> * rand() ]

let exampleDeviation = standardDeviation  sampleTimes
let exampleVariance = variance sampleTimes
//val exampleDeviation : float<s> = 2.865331753
//val exampleVariance : float<s ^ 2> = 8.210126054

type Vector2D<[<Measure>] 'u> =
    { DX: float<'u>; DY: float<'u> }

/// Two-dimensional vectors
type Vector2D<[<Measure>] 'u>(dx: float<'u>, dy: float<'u>) =

    /// Get the X component of the vector
    member v.DX = dx    
    
    /// Get the Y component of the vector
    member v.DY = dy    
    
    /// Get the length of the vector
    member v.Length = sqrt(dx * dx + dy * dy)
    
    /// Get a vector scaled by the given factor
    member v.Scale k = Vector2D(k*dx, k*dy)
    
    /// Return a vector shifted by the given delta in the X coordinate
    member v.ShiftX x = Vector2D(dx+x, dy)

    /// Return a vector shifted by the given delta in the Y coordinate
    member v.ShiftY y = Vector2D(dx, dy+y)

    /// Get the zero vector
    static member Zero = Vector2D<'u>(0.0<_>, 0.0<_>)

    /// Return a constant vector along the X axis
    static member ConstX dx = Vector2D<'u>(dx, 0.0<_>)

    /// Return a constant vector along the Y axis
    static member ConstY dy = Vector2D<'u>(0.0<_>, dy)

    /// Return the sum of two vectors
    static member (+) (v1: Vector2D<'u>, v2: Vector2D<'u>) = 
        Vector2D(v1.DX + v2.DX, v1.DY + v2.DY)

    /// Return the difference of two vectors
    static member (-) (v1: Vector2D<'u>, v2: Vector2D<'u>) = 
        Vector2D(v1.DX - v2.DX, v1.DY - v2.DY)

    /// Return the pointwise-product of two vectors
    static member (.*) (v1: Vector2D<'u>, v2: Vector2D<'u>) = 
        Vector2D(v1.DX * v2.DX, v1.DY * v2.DY)

let three = float 3.0<kg>
let sixKg = LanguagePrimitives.FloatWithMeasure<kg> (three + three)
val three : float = 3.0
val sixKg : float<kg> = 6.0

type Input<'T, [<Measure>] 'u> = 
   { Data : 'T 
     Features: float<'u>[] }

type Centroid<[<Measure>] 'u> = float<'u>[]
let distance (xs:Input<_,_>) (ys:Centroid<_>) = …

let computeCentroidOfGroup (_,group:Input<_>[]) =