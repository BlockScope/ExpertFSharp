let repeatFetch url n =
    for i = 1 to n do
        let html = http url
        printf "fetched <<< %s >>>\n" html 
    printf "Done!\n"

open System

let loopUntilSaturday() =
    while (DateTime.Now.DayOfWeek <> DayOfWeek.Saturday) do
        printf "Still working!\n"

    printf "Saturday at last!\n"

> for (b,pj) in [ ("Banana 1",true); ("Banana 2",false)  ] do
      if pj then printfn "%s is in pyjamas today!" b;;
//Banana 1 is in pyjamas today!

> open System.Text.RegularExpressions;;

> for m in (Regex.Matches("All the Pretty Horses","[a-zA-Z]+"))  do
      printf "res = %s\n" m.Value;;
//res = All
//res = the
//res = Pretty
//res = Horses

type DiscreteEventCounter =
    { mutable Total: int; 
      mutable Positive: int;
      Name : string }

let recordEvent (s: DiscreteEventCounter) isPositive =
    s.Total <- s.Total+1
    if isPositive then s.Positive <- s.Positive+1

let reportStatus (s: DiscreteEventCounter) =
    printfn "We have %d %s out of %d" s.Positive s.Name s.Total

let newCounter nm =
    { Total = 0;
      Positive = 0;
      Name = nm  }

let longPageCounter = newCounter "long page(s)"

let fetch url =
    let page = http url
    recordEvent longPageCounter (page.Length > 10000)
    page

> fetch "http://www.smh.com.au" |> ignore;;

> fetch "http://www.theage.com.au"  |> ignore;;

> reportStatus longPageCounter;;

> let cell1 = ref 1;;
//val cell1 : int ref = {contents = 1;}

> cell1.Value;;
//val it : int = 1

> cell1 := 3;;

> cell1;;
//val it : int ref = {contents = 3;}

> cell1.Value;;

val ref  : 'T -> 'T ref
val (:=) : 'T ref -> 'T -> unit
val (!)  : 'T ref -> 'T

type 'T ref = 
    { mutable contents: 'T }
    member cell.Value = cell.contents

let (!) r = r.contents

let (:=) r v = r.contents <- v

let ref v = { contents = v }

> let cell2 = cell1;;
//val cell2 : int ref = {contents = 3;}

> !cell2;;
//val it : int = 3

> cell1 := 7;;

> !cell2;;
//val it : int = 7

let generateStamp =
    let count = ref 0
    (fun () -> count := !count + 1; !count)
val generateStamp: unit -> int

> generateStamp();;
//val it : int = 1

> generateStamp();;
//val it : int = 2

> let mutable cell1 = 1;;
//val cell1 : int = 1

> cell1;;
//val it : int = 1

> cell1 <- 3;;
//val it : unit = ()

> cell1;;
//val it : int = 3

let sum n m =
    let mutable res = 0
    for i = n to m do
        res <- res + i
    res

> sum 3 6;;
//val it : int = 18

> let arr = [| 1.0; 1.0; 1.0 |];;
//val arr : float[] = [| 1.0; 1.0; 1.0 |]

> arr.[1];;
//val it : float = 1.0

> arr.[1] <- 3.0;;

> arr;;
//val it : float[] = [| 1.0; 3.0; 1.0 |]


Array.append	: 'T[] -> 'T[] -> 'T[]	Returns a new array containing elements of the first array followed by elements of the second array
Array.sub	: 'T[] -> int -> int -> 'T[]	Returns a new array containing a portion of elements of the input array
Array.copy	: 'T[] -> 'T[]	Returns a copy of the input array
Array.iter	: ('T -> unit) -> 'T[] -> unit	Applies a function to all elements of the input array
Array.filter	: ('T -> bool) -> 'T[] -> 'T[]	Returns a new array containing a selection of elements of the input array
Array.length	: 'T[] -> int	Returns the length of the input array
Array.map	: ('T -> 'U) -> 'T[] -> 'U[]	Returns a new array containing the results of applying the function to each element of the input array
Array.fold	: ('T -> 'U -> 'T) -> 'T -> 'U[] -> 'T	Accumulates left to right over the input array
Array.foldBack	: ('T -> 'U -> 'U) -> 'T[] -> 'U -> 'U	Accumulates right to left over the input array

> let bigArray = Array.zeroCreate<int> 100000000;;
//val bigArray : int [] = ...

> let tooBig = Array.zeroCreate<int> 1000000000;;
//System.OutOfMemoryException: Exception of type 'System.OutOfMemoryException'
//was thrown.

> let arr = [| for i in 0 .. 5 -> (i,i*i) |];;
//val arr : (int * int) [] = [|(0, 0); (1, 1); (2, 4); (3, 9); (4, 16); (5, 25)|]

> let arr = [| for i in 0 .. 5 -> (i,i*i) |];;
//val arr : (int * int) [] = [|(0, 0); (1, 1); (2, 4); (3, 9); (4, 16); (5, 25)|]

> arr.[1..3];;
//val it : (int * int) [] = [| (1, 1); (2, 4); (3, 9); |]

> arr.[..2];;
//val it : (int * int) [] = [| (0, 0); (1, 1); (2, 4); |]

> arr.[3..];;
//val it : (int * int) [] = [| (3, 9); (4, 16); (5, 25) |]

type ResizeArray<'T> = System.Collections.Generic.List<'T>

> let names = new ResizeArray<string>();;
//val names : ResizeArray<string>

> for name in ["Claire"; "Sophie"; "Jane"] do
      names.Add(name);;

> names.Count;;
//val it : int = 3

> names.[0];;
//val it : string = "Claire"

> names.[1];;
//val it : string = "Sophie"

> names.[2];;
//val it : string = "Jane"

> let squares = new ResizeArray<int>(seq { for i in 0 .. 100 -> i*i });;
//val squares : ResizeArray<int>

> for x in squares do
      printfn "square: %d" x;;

//square: 0
//square: 1
//square: 4
//square: 9
//...

> open System.Collections.Generic;;

> let capitals = new Dictionary<string, string>(HashIdentity.Structural);;
//val capitals : Dictionary<string,string> = dict []

> capitals.["USA"] <- "Washington";;
> capitals.["Bangladesh"] <- "Dhaka";;

> capitals.ContainsKey("USA");;
//val it : bool = true

> capitals.ContainsKey("Australia");;
//val it : bool = false

> capitals.Keys;;
//val it : KeyCollection<string,string> = seq["USA"; "Bangladesh"]

> capitals.["USA"];;
//val it : string = "Washington"

> for kvp in capitals do
      printf "%s has capital %s\n" kvp.Key kvp.Value;;

open System.Collections.Generic

let lookupName nm (dict : Dictionary<string,string>) =
    let mutable res = ""
    let foundIt = dict.TryGetValue(nm, &res)
    if foundIt then res
    else failwithf "Didn’t find %s" nm

> let res = ref "";;
//val res: string ref = {contents = "";}

> capitals.TryGetValue("Australia", res);;
//val it: bool = false

> capitals.TryGetValue("USA", res);;
//val it: bool = true

> res;;
//val it: string ref = {contents = "Washington"}

> capitals.TryGetValue("Australia");;
//val it: bool * string = (false, null)

> capitals.TryGetValue("USA");;
//val it: bool * string = (true, "Washington")

> open System.Collections.Generic;;
> open Microsoft.FSharp.Collections;;

> let sparseMap = new Dictionary<(int * int), float>();;
//val sparseMap : Dictionary <(int * int),float> = dict []

> sparseMap.[(0,2)] <- 4.0;;

> sparseMap.[(1021,1847)] <- 9.0;;

> sparseMap.Keys;;
///val it : Dictionary.KeyCollection<(int * int),float>  = seq [(0,2); (1021; 1847)]

> let req = System.Net.WebRequest.Create("not a URL");;
//System.UriFormatException: Invalid URI: The format of the URI could not be
//determined.

> (raise (System.InvalidOperationException("not today thank you")) : unit);;
//System.InvalidOperationException: not today thank you

> if false then 3 else failwith "hit the wall";;
//System.Exception: hit the wall

val failwith  : string -> 'T
val raise : System.Exception -> 'T
val failwithf  : StringFormat<'T,'U> -> 'T
val invalidArg  : string -> string -> 'T

if (System.DateTime.Now > failwith "not yet decided") then
    printfn "you've run out of time!"

> try
     raise (System.InvalidOperationException ("it's just not my day"))
  with
     | :? System.InvalidOperationException -> printfn "caught!";;

open System.IO

let http (url: string) =
    try
        let req = System.Net.WebRequest.Create(url)
        let resp = req.GetResponse()
        let stream = resp.GetResponseStream()
        let reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        html
    with
        | :? System.UriFormatException -> ""
        | :? System.Net.WebException -> ""

> try
      raise (new System.InvalidOperationException ("invalid operation"))
  with
      | err -> printfn "oops, msg = '%s'" err.Message;;
oops, msg = 'invalid operation'

let httpViaTryFinally(url: string) =
    let req = System.Net.WebRequest.Create(url)
    let resp = req.GetResponse()
    try
        let stream = resp.GetResponseStream()
        let reader = new StreamReader(stream)
        let html = reader.ReadToEnd()
        html
    finally
        resp.Close()

let httpViaUseBinding(url: string) =
    let req = System.Net.WebRequest.Create(url)
    use resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    html

exception BlockedURL of string

let http2 url =
    if url = "http://www.kaos.org"
    then raise(BlockedURL(url))
    else http url

> try
     raise(BlockedURL("http://www.kaos.org"))
  with
     | BlockedURL(url) -> printf "blocked! url = '%s'\n" url;;

//blocked! url = 'http://www.kaos.org'

> open System.IO;;

> File.WriteAllLines("test.txt", [| "This is a test file.";
                                    "It is easy to read." |]);;

> open System.IO;;

> File.ReadAllLines "test.txt";;
//val it : string [] = [| "This is a test file.";  "It is easy to read." |]

> File.ReadAllText "test.txt";;
//val it : string = "This is a test file.\r\nIt is easy to read\r\n"

> seq { for line in File.ReadLines("test.txt") do
           let words = line.Split [| ' ' |]
              if words.Length > 3 && words.[2] = "easy" then
                 yield line };;

//val it : seq<string> = seq [ "It is easy to read." ]

> let outp = File.CreateText "playlist.txt";;
//val outp : StreamWriter

> outp.WriteLine "Enchanted";;

> outp.WriteLine "Put your records on";;

> outp.Close();

> let inp = File.OpenText("playlist.txt");;
//val inp : StreamReader

> inp.ReadLine();;
//val it : string = "Enchanted"

> inp.ReadLine();;
//val it : string = "Put your records on"

> inp.Close();;

> System.Console.WriteLine "Hello World";;

Hello World

> System.Console.ReadLine();;

<enter "I'm still here" here>
//val it : string = "I'm still here"

let isWord (words: string list) =
    let wordTable = Set.ofList words
    fun w -> wordTable.Contains(w)

//val isWord : string list -> (string -> bool) 

> let isCapital = isWord ["London";"Paris";"Warsaw";"Tokyo"];;
//val isCapital : (string -> bool)

> isCapital "Paris";;
//val it : bool = true

> isCapital "Manchester";;
//val it : bool = false

let isCapitalSlow inp = isWord ["London";"Paris";"Warsaw";"Tokyo"] inp

let isWordSlow2 (words: string list) (word:string) =
    List.exists (fun word2 -> word = word2) words

let isCapitalSlow2 inp = isWordSlow2 ["London";"Paris";"Warsaw";"Tokyo"] inp

let isWordSlow3 (words: string list) (w:string) =
    let wordTable = Set.Create(words)
    wordTable.Contains(w)

let isCapitalSlow3 inp = isWordSlow3 ["London";"Paris";"Warsaw";"Tokyo"] inp

let isWord (words: string list) =
    let wordTable = System.Collections.Generic.HashSet<_>(words)
    fun word -> wordTable.Contains word

open System

type NameLookupService =
    abstract Contains : string -> bool

let buildSimpleNameLookup (words: string list) =
    let wordTable = System.Collections.Generic.HashSet<_>(words)
    { new NameLookupService with
        member t.Contains w = wordTable.Contains w }

> let capitalLookup = buildSimpleNameLookup ["London";"Paris";"Warsaw";"Tokyo"];;
//val capitalLookup : NameLookupService

> capitalLookup.Contains "Paris";;
//val it : bool = true

let rec fib n = if n <= 2 then 1 else fib (n-1) + fib (n-2)

#nowarn "40" // do not warn on recursive computed objects and functions
let fibFast =
    let t = new System.Collections.Generic.Dictionary<int,int>()
    let rec fibCached n =
        if t.ContainsKey n then t.[n]
        elif n <= 2 then 1
        else let res = fibCached (n-1) + fibCached (n-2)
             t.Add (n,res)
             res
    fun n -> fibCached n

open System.Collections.Generic 

let memoize (f: 'T -> 'U) =
    let t = new Dictionary<'T,'U>(HashIdentity.Structural)
    fun n ->
        if t.ContainsKey n then t.[n]
        else let res = f n
             t.Add (n,res)
             res

let rec fibFast =
    memoize (fun n -> if n <= 2 then 1 else fibFast (n-1) + fibFast (n-2))

val memoize : ('T -> 'U) -> ('T -> 'U) when 'T : equality
val fibFast : (int -> int)

let rec fibNotFast n =
    memoize (fun n -> if n <= 2 then 1 else fibNotFast (n-1) + fibNotFast (n-2)) n

open System.Collections.Generic 

type Table<'T,'U> =
    abstract Item : 'T -> 'U with get
    abstract Discard : unit -> unit

let memoizeAndPermitDiscard f =
    let lookasideTable = new Dictionary<_,_>(HashIdentity.Structural)
    { new Table<'T,'U> with
          member t.Item
             with get(n) =
                 if lookasideTable.ContainsKey(n)
                 then lookasideTable.[n]
                 else let res = f n
                      lookasideTable.Add(n,res)
                      res

          member t.Discard() =
              lookasideTable.Clear() }


#nowarn "40" // do not warn on recursive computed objects and functions

let rec fibFast =
    memoizeAndPermitDiscard
        (fun n ->
            printfn "computing fibFast %d" n
            if n <= 2 then 1 else fibFast.[n-1] + fibFast.[n-2])

val memoizeAndPermitDiscard : ('T -> 'U) -> Table<'T, 'U> when 'T : equality
val fibFast : Table<int,int>

> fibFast.[3];;
//computing fibFast 3
//computing fibFast 2
//computing fibFast 1
//val it : int = 2

> fibFast.[5];;
//computing fibFast 5
//computing fibFast 4
//val it : int = 5

> fibFast.Discard();;

> fibFast.[5];;
//computing fibFast 5
//computing fibFast 4
//computing fibFast 3
//computing fibFast 2
//computing fibFast 1
//val it : int = 5

> let sixty = lazy (30+30);;
//val sixty : Lazy<int>

> sixty.Force();;
//val it : int = 60 

> let sixtyWithSideEffect = lazy (printfn "Hello world"; 30+30);;
//val sixtyWithSideEffect: Lazy<int>

> sixtyWithSideEffect.Force();;
//Hello world
//val it : int = 60

> sixtyWithSideEffect.Force();;
//val it : int = 60

let myWriteStringToFile () =
    use outp = File.CreateText(@"playlist.txt")
    outp.WriteLine("Enchanted")
    outp.WriteLine("Put your records on")

let myWriteStringToFile () =
    using (File.CreateText(@"playlist.txt")) (fun outp ->
        outp.WriteLine("Enchanted")
        outp.WriteLine("Put your records on"))

let using (ie : #System.IDisposable) f =
    try f(ie)
    finally ie.Dispose()

namespace System
    type IDisposable =
        abstract Dispose: unit -> unit

/// Fetch a web page
let http (url: string) =
    let req = System.Net.WebRequest.Create url
    use resp = req.GetResponse()
    use stream = resp.GetResponseStream()
    use reader = new System.IO.StreamReader(stream)
    let html = reader.ReadToEnd()
    html

open System
open System.IO

type LineChooser(fileName1, fileName2) =
    let file1 = File.OpenText(fileName1)
    let file2 = File.OpenText(fileName2)
    let rnd = new System.Random()

    let mutable disposed = false

    let cleanup() =
        if not disposed then
            disposed <- true;
            file1.Dispose();
            file2.Dispose();

    interface System.IDisposable with
        member x.Dispose() = cleanup()

    member obj.CloseAll() = cleanup()

    member obj.GetLine() =
        if not file1.EndOfStream &&
           (file2.EndOfStream  || rnd.Next() % 2 = 0) then file1.ReadLine()
        elif not file2.EndOfStream then file2.ReadLine()
        else raise (new EndOfStreamException())

> open System.IO;;

> File.WriteAllLines("test1.txt", [| "Daisy, Daisy"; "Give me your hand oh do" |]);;

> File.WriteAllLines("test2.txt", [| "I'm a little teapot"; "Short and stout" |]);;

> let chooser = new LineChooser ("test1.txt", "test2.txt");;
//val chooser : LineChooser

> chooser.GetLine();;
//val it : string = "Daisy, Daisy"

> chooser.GetLine();;
//val it : string = "I'm a little teapot"

> (chooser :> IDisposable).Dispose();;

> chooser.GetLine();;

open System

type TicketGenerator() =
    let mutable free = []
    let mutable max = 0
    member h.Alloc() =
        match free with
        | [] -> max <- max + 1; max
        | h::t -> free <- t; h
    member h.Dealloc(n:int) =
        printfn "returning ticket %d" n
        free <- n :: free

let ticketGenerator = new TicketGenerator()

type Customer() =
    let myTicket = ticketGenerator.Alloc()
    let mutable disposed = false
    let cleanup() =
         if not disposed then
             disposed <- true
             ticketGenerator.Dealloc(myTicket)
    member x.Ticket = myTicket
    interface IDisposable with
         member x.Dispose() = cleanup(); GC.SuppressFinalize(x)
    override x.Finalize() = cleanup()

> let bill = new Customer();;
//val bill : Customer

> bill.Ticket;;
//val it : int = 1

> begin
      use joe = new Customer()
      printfn "joe.Ticket = %d" joe.Ticket
  end;;

//joe.Ticket = 2
//returning ticket 2

> begin
      use jane = new Customer()
      printfn "jane.Ticket = %d" jane.Ticket
  end;;

//jane.Ticket = 2
//returning ticket 2

open System.IO

let firstTwoLines file =
    seq { use s = File.OpenText(file)
          yield s.ReadLine()
          yield s.ReadLine() }

> File.WriteAllLines("test1.txt", [| "Es kommt ein Schiff";
                                     "A ship is coming" |]);;

> let twolines() = firstTwoLines "test1.txt";;
//val twolines : unit -> seq<string>

> twolines() |> Seq.iter (printfn "line = '%s'")

//line = 'Es kommt ein Schiff'
//line = A ship is coming'

> let parents = [("Adam",None); ("Cain",Some("Adam","Eve"))];;
//val parents : (string * (string * string) option) list = ...

match System.Environment.GetEnvironmentVariable("PATH") with
| null -> printf "the environment variable PATH is not defined\n"
| res -> printf "the environment variable PATH is set to %s\n" res

let switchOnType (a:obj) =
    match a with
    | null                     -> printf "null!"
    | :? System.Exception as e -> printf "An exception: %s!" e.Message
    | :? System.Int32 as i     -> printf "An integer: %d!" i
    | :? System.DateTime as d  -> printf "A date/time: %O!" d
    | _                        -> printf "Some other kind of object\n"

let factorizeImperative n =
    let mutable primefactor1 = 1
    let mutable primefactor2 = n
    let mutable i = 2
    let mutable fin = false
    while (i < n && not fin) do
        if (n % i = 0) then
            primefactor1 <- i
            primefactor2 <- n / i
            fin <- true
        i <- i + 1

    if (primefactor1 = 1) then None
    else Some (primefactor1, primefactor2)

let factorizeRecursive n =
    let rec find i =
        if i >= n then None
        elif (n % i = 0) then Some(i,n / i)
        else find (i+1)
    find 2

open System.Collections.Generic

let divideIntoEquivalenceClasses keyf seq =

    // The dictionary to hold the equivalence classes
    let dict = new Dictionary<'key,ResizeArray<'T>>()

    // Build the groupings
    seq |> Seq.iter (fun v ->
        let key = keyf v
        let ok,prev = dict.TryGetValue(key)
        if ok then prev.Add(v)
        else let prev = new ResizeArray<'T>()
             dict.[key] <- prev
             prev.Add(v))

    // Return the sequence-of-sequences. Don't reveal the
    // internal collections: just reveal them as sequences
    dict |> Seq.map (fun group -> group.Key, Seq.readonly group.Value)

//val divideIntoEquivalenceClasses : ('T -> 'key) -> seq<'T> -> seq<'key * seq<'T>>

> divideIntoEquivalenceClasses (fun n -> n % 3) [ 0 .. 10 ];;
//val it : seq<int * seq<int>>
//= seq [(0, seq [0; 3; 6; 9]); (1, seq [1; 4; 7; 10]); (2, seq [2; 5; 8])]

open System.IO
let reader1, reader2 =
    let reader = new StreamReader(File.OpenRead("test.txt"))
    let firstReader() = reader.ReadLine()
    let secondReader() = reader.ReadLine()

    // Note: we close the stream reader here!
    // But we are returning function values which use the reader
    // This is very bad!
    reader.Close()
    firstReader, secondReader

// Note: stream reader is now closed! The next line will fail!
let firstLine = reader1()
let secondLine = reader2()
firstLine, secondLine

open System.IO

let line1, line2 =
    let reader = new StreamReader(File.OpenRead("test.txt"))
    let firstLine = reader.ReadLine()
    let secondLine = reader.ReadLine()
    reader.Close()
    firstLine, secondLine

let reader =
    seq { use reader = new StreamReader(File.OpenRead("test.txt"))
          while not reader.EndOfStream do
              yield reader.ReadLine() }
