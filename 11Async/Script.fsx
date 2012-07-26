> open System.Windows.Forms;;
> let form = new Form(Text = "Click Form", Visible = true, TopMost = true);;
//val form : Form

> form.Click.Add(fun evArgs -> printfn "Clicked!");;
> form.MouseMove.Add(fun args -> printfn "Mouse, (X, Y) = (%A, %A)" args.X args.Y);;

form.MouseMove
    |> Event.filter (fun args -> args.X > 100)
    |> Event.listen (fun args -> printfn "Mouse, (X, Y) = (%A, %A)" args.X args.Y)

Event.choose
Event.create
Event.filter
Event.scan
Event.listen
Event.map
Event.partition
// I get the following error when trying to get F# interactive to give me the types.
// error FS0335: Could not resolve the ambiguity in the use of a generic construct with a 'delegate' constraint at or near this position
//Event.choose	: ('T -> 'U option) -> IEvent<'T> -> IEvent<'U>
//Event.create	: unit -> ('T -> unit) * IEvent<'T>
//Event.filter	: ('T -> bool) -> IEvent<'T> -> IEvent<'T>
//Event.scan	: ('U -> 'T -> 'U) -> 'U -> IEvent<'T> -> IEvent<'U>
//Event.listen	: ('T -> unit) -> IEvent<'T> -> unit
//Event.map	: ('T -> 'U) -> IEvent<'T> -> IEvent<'U>
//Event.partition	: ('T -> bool) -> IEvent<'T> -> IEvent<'T> * IEvent<'T>

open System
open System.Windows.Forms

type RandomTicker(approxInterval) =
    let timer = new Timer()
    let rnd = new System.Random(99)
    let tickEvent = new Event<int> ()

    let chooseInterval() : int =
        approxInterval + approxInterval / 4 - rnd.Next(approxInterval / 2)

    do timer.Interval <- chooseInterval()

    do timer.Tick.Add(fun args ->
        let interval = chooseInterval()
        tickEvent.Trigger interval;
        timer.Interval <- interval)

    member x.RandomTick = tickEvent.Publish
    member x.Start() = timer.Start()
    member x.Stop() = timer.Stop()
    interface IDisposable with
        member x.Dispose() = timer.Dispose()
//type RandomTicker =
//  class
//    interface System.IDisposable
//    new : approxInterval:int -> RandomTicker
//    member Start : unit -> unit
//    member Stop : unit -> unit
//    member RandomTick : IEvent<int>
//  end

> let rt = new RandomTicker(1000);;
//val rt : RandomTicker

> rt.RandomTick.Add(fun nextInterval -> printfn "Tick, next = %A" nextInterval);;
> rt.Start();;
//Tick, next = 1072
//Tick, next = 927
//Tick, next = 765
//...

> rt.Stop();;

open System.ComponentModel
open System.Windows.Forms

let worker = new BackgroundWorker()
let numIterations = 1000

worker.DoWork.Add(fun args ->

    let rec computeFibonacci resPrevPrev resPrev i =
        // Compute the next result
        let res = resPrevPrev + resPrev

        // At the end of the computation write the result into mutable state
        if i = numIterations then
            args.Result <- box res
        else
            // Compute the next result
            computeFibonacci resPrev res (i + 1)

    computeFibonacci 1 1 2)

worker.RunWorkerCompleted.Add(fun args ->
    MessageBox.Show(sprintf "Result = %A" args.Result) |> ignore)

// Execute the worker
worker.RunWorkerAsync()

open System.ComponentModel
open System.Windows.Forms

/// An IterativeBackgroundWorker follows the BackgroundWorker design pattern
/// but instead of running an arbitrary computation it iterates a function
/// a fixed number of times and reports intermediate and final results.
/// The worker is paramaterized by its internal state type.
///
/// Percentage progress is based on the iteration number. Cancellation checks
/// are made at each iteration. Implemented via an internal BackgroundWorker.
type IterativeBackgroundWorker<'T>(oneStep : ('T -> 'T),
                                   initialState : 'T,
                                   numIterations : int) =

    let worker =
        new BackgroundWorker(WorkerReportsProgress = true,
                             WorkerSupportsCancellation = true)


    // Create the events that we will later trigger
    let completed = new Event<_>()
    let error = new Event<_>()
    let cancelled = new Event<_>()
    let progress = new Event<_>()

    do worker.DoWork.Add(fun args ->
        // This recursive function represents the computation loop.
        // It runs at "maximum speed", i.e. is an active rather than
        // a reactive process, and can only be controlled by a
        // cancellation signal.
        let rec iterate state i =
            // At the end of the computation terminate the recursive loop
            if worker.CancellationPending then
               args.Cancel <- true
            elif i < numIterations then
                // Compute the next result
                let state' = oneStep state

                // Report the percentage computation and the internal state
                let percent = int ((float (i + 1) / float numIterations) * 100.0)
                do worker.ReportProgress(percent, box state);

                // Compute the next result
                iterate state' (i + 1)
            else
                args.Result <- box state

        iterate initialState 0)

    do worker.RunWorkerCompleted.Add(fun args ->
        if args.Cancelled then cancelled.Trigger()
        elif args.Error <> null then error.Trigger args.Error
        else completed.Trigger (args.Result :?> 'T))

    do worker.ProgressChanged.Add(fun args ->
        progress.Trigger (args.ProgressPercentage,(args.UserState :?> 'T)))

    member x.WorkerCompleted = completed.Publish
    member x.WorkerCancelled = cancelled.Publish
    member x.WorkerError = error.Publish
    member x.ProgressChanged = progress.Publish

    // Delegate the remaining members to the underlying worker
    member x.RunWorkerAsync() = worker.RunWorkerAsync()
    member x.CancelAsync() = worker.CancelAsync()
//type IterativeBackgroundWorker<'T> =
//  class
//    new : oneStep:('T -> 'T) * initialState:'T * numIterations:int ->
//            IterativeBackgroundWorker<'T>
//    member CancelAsync : unit -> unit
//    member RunWorkerAsync : unit -> unit
//    member ProgressChanged : IEvent<int * 'T>
//    member WorkerCancelled : IEvent<unit>
//    member WorkerCompleted : IEvent<'T>
//    member WorkerError : IEvent<exn>
//  end

let fibOneStep (fibPrevPrev : bigint, fibPrev) = (fibPrev, fibPrevPrev + fibPrev);;
//val fibOneStep :
//  fibPrevPrev:bigint * fibPrev:Numerics.BigInteger ->
//    Numerics.BigInteger * Numerics.BigInteger


> let worker = new IterativeBackgroundWorker<_>(fibOneStep, (1I, 1I), 100);;
//val worker : IterativeBackgroundWorker<bigint * Numerics.BigInteger>

> worker.WorkerCompleted.Add(fun result ->
      MessageBox.Show(sprintf "Result = %A" result) |> ignore);;

> worker.ProgressChanged.Add(fun (percentage, state) ->
    printfn "%d%% complete, state = %A" percentage state);;

> worker.RunWorkerAsync();;
//1% complete, state = (1I, 1I)
//2% complete, state = (1I, 2I)
//3% complete, state = (2I, 3I)
//4% complete, state = (3I, 5I)
//...
//98% complete, state = (135301852344706746049I, 218922995834555169026I)
//99% complete, state = (218922995834555169026I, 354224848179261915075I)
//100% complete, state = (354224848179261915075I, 573147844013817084101I)

open System
open System.Threading

// Pseudo-code for adding event-raising to this object
type IterativeBackgroundWorker<'T>(...) =

    let worker = ...

    // The constructor captures the synchronization context. This allows us to post
    // messages back to the GUI thread where the BackgroundWorker was created.
    let syncContext = SynchronizationContext.Current
    do if syncContext = null then failwith "no synchronization context found"

    let started = new Event<_>()

    // Raise the event when the worker starts. This is done by posting a message
    // to the captured synchronization context.
    do worker.DoWork.Add(fun args ->
        syncContext.Post(SendOrPostCallback(fun _ -> started.Trigger(DateTime.Now)),
                         state = null)
        ...

    /// The Started event gets raised when the worker starts. It is
    /// raised on the GUI thread (i.e. in the synchronization context of
    /// the thread where the worker object was created).
    // It has type IEvent<DateTime>
    member x.Started = started.Publish

open System.Drawing
open System.Windows.Forms

let form = new Form(Visible = true, TopMost = true)

let panel = new FlowLayoutPanel(Visible = true,
                                Height = 20,
                                Dock = DockStyle.Bottom,
                                BorderStyle = BorderStyle.FixedSingle)

let progress = new ProgressBar(Visible = false,
                               Anchor = (AnchorStyles.Bottom ||| AnchorStyles.Top),
                               Value = 0)

let text = new Label(Text = "Paused",
                     Anchor = AnchorStyles.Left,
                     Height = 20,
                     TextAlign = ContentAlignment.MiddleLeft)

panel.Controls.Add(progress)
panel.Controls.Add(text)
form.Controls.Add(panel)

let fibOneStep (fibPrevPrev : bigint, fibPrev) = (fibPrev, fibPrevPrev + fibPrev)

// Run the iterative algorithm 500 times before reporting intermediate results
let rec repeatMultipleTimes n f s = 
    if n <= 0 then s else repeatMultipleTimes (n - 1) f (f s)

// Burn some additional cycles to make sure it runs slowly enough
let rec burnSomeCycles n f s = 
    if n <= 0 then f s else ignore (f s); burnSomeCycles (n - 1) f s

let step = (repeatMultipleTimes 500 (burnSomeCycles 1000 fibOneStep))

// Create the iterative worker.
let worker = new IterativeBackgroundWorker<_>(step, (1I, 1I), 80)

worker.ProgressChanged.Add(fun (progressPercentage, state)->
    progress.Value <- progressPercentage)

worker.WorkerCompleted.Add(fun (_, result) ->
    progress.Visible <- false;
    text.Text <- "Paused";
    MessageBox.Show(sprintf "Result = %A" result) |> ignore)

worker.WorkerCancelled.Add(fun () ->
    progress.Visible <- false;
    text.Text <- "Paused";
    MessageBox.Show(sprintf "Cancelled OK!") |> ignore)

worker.WorkerError.Add(fun exn ->
    text.Text <- "Paused";
    MessageBox.Show(sprintf "Error: %A" exn) |> ignore)

form.Menu <- new MainMenu()
let workerMenu = form.Menu.MenuItems.Add("&Worker")

workerMenu.MenuItems.Add(new MenuItem("Run", onClick = (fun _ args ->
    text.Text <- "Running";
    progress.Visible <- true;
    worker.RunWorkerAsync())))

workerMenu.MenuItems.Add(new MenuItem("Cancel", onClick = (fun _ args ->
    text.Text <- "Cancelling";
    worker.CancelAsync())))

form.Closed.Add(fun _ -> worker.CancelAsync())

open System.Net
open System.IO

let museums = ["MOMA", "http://moma.org/";
               "British Museum", "http://www.thebritishmuseum.ac.uk/";
               "Prado", "http://www.museodelprado.es/"]

let fetchAsync(nm, url : string) = async {
    printfn "Creating request for %s..." nm
    let req = WebRequest.Create(url)

    let! resp = req.AsyncGetResponse()

    printfn "Getting response stream for %s..." nm
    let stream = resp.GetResponseStream()

    printfn "Reading response for %s..." nm
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()

    printfn "Read %d characters for %s..." html.Length nm}

Async.Parallel [for nm, url in museums -> fetchAsync(nm, url)]
    |> Async.Ignore
    |> Async.RunSynchronously

//val museums : (string * string) list =
//  [("MOMA", "http://moma.org/");
//   ("British Museum", "http://www.thebritishmuseum.ac.uk/");
//   ("Prado", "http://www.museodelprado.es/")]
//val fetchAsync : nm:string * url:string -> Async<unit>
//Creating request for Creating request for Creating request for Prado...
//British Museum...
//MOMA...
//Getting response stream for MOMA...
//Reading response for MOMA...
//Getting response stream for Prado...
//Reading response for Prado...
//Read 38207 characters for MOMA...
//Read 21591 characters for Prado...
//Getting response stream for British Museum...
//Reading response for British Museum...
//Read 36607 characters for British Museum...

let tprintfn fmt =
    printf "[.NET Thread %d]" System.Threading.Thread.CurrentThread.ManagedThreadId;
    printfn fmt

open System.Net
open System.IO

let museums = ["MOMA", "http://moma.org/";
               "British Museum", "http://www.thebritishmuseum.ac.uk/";
               "Prado", "http://www.museodelprado.es/"]

let fetchAsync(nm, url : string) = async {
    tprintfn "Creating request for %s..." nm
    let req = WebRequest.Create(url)

    let! resp = req.AsyncGetResponse()

    tprintfn "Getting response stream for %s..." nm
    let stream = resp.GetResponseStream()

    tprintfn "Reading response for %s..." nm
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()

    tprintfn "Read %d characters for %s..." html.Length nm}

Async.Parallel [for nm, url in museums -> fetchAsync(nm, url)]
    |> Async.Ignore
    |> Async.RunSynchronously
//[.NET Thread 9]Creating request for British Museum...
//[.NET Thread 5]Creating request for Prado...
//[.NET Thread 3]Creating request for MOMA...
//[.NET Thread 8]Getting response stream for MOMA...
//[.NET Thread 8]Reading response for MOMA...
//[.NET Thread 10]Getting response stream for Prado...
//[.NET Thread 10]Reading response for Prado...
//[.NET Thread 8]Read 38216 characters for MOMA...
//[.NET Thread 10]Read 21591 characters for Prado...
//[.NET Thread 10]Getting response stream for British Museum...
//[.NET Thread 10]Reading response for British Museum...
//[.NET Thread 10]Read 36607 characters for British Museum...


    open System.Threading

    ThreadPool.QueueUserWorkItem(fun _ -> printf "Hello!") |> ignore
    
let showMethods(t : System.Type) =
  t.GetMethods() |> Seq.iter (printfn "%A")
// SOURCE: http://stackoverflow.com/questions/3142082/get-description-of-types-in-f-interactive

//type Async<'T> = Async of ('T -> unit) * (exn -> unit) -> unit
//type AsyncBuilder with
//    member Return : value:'T -> Async<'T>
//    member Delay : generator:(unit -> Async<'T>) -> Async<'T>
//    member Using: resource:'T * binding:('T -> Async<'U>) -> Async<'U> when 'T :> System.IDisposable
//    member Bind: computation:Async<'T> * binder:('T -> Async<'U>) -> Async<'U>

#r @".\packages\FSPowerPack.Community.2.1.3.1\Lib\Net40\FSharp.PowerPack.dll"
open System.Net
open System.IO
open Microsoft.FSharp.Control.WebExtensions

async {let req = WebRequest.Create("http://moma.org/")
       let! resp = req.AsyncGetResponse()
       let stream = resp.GetResponseStream()
       let reader = new StreamReader(stream)
       let! html = reader.AsyncReadToEnd()
       html}
// warning FS0020: This expression should have type 'unit', but has type 'string'. Use 'ignore' to discard the result of the expression, or 'let' to bind the result to a name.
//val it : Async<unit> =
//  Microsoft.FSharp.Control.FSharpAsync`1[Microsoft.FSharp.Core.Unit]

async.Delay(fun () ->
    let req = WebRequest.Create("http://moma.org/")
    async.Bind(req.AsyncGetResponse(), (fun resp ->
        let stream = resp.GetResponseStream() 
        let reader = new StreamReader(stream)  
        async.Bind(reader.AsyncReadToEnd(), (fun html ->
            async.Return html)))))
//warning FS0044: This construct is deprecated. The extension method now resides in the 'WebExtensions' module in the F# core library. Please add 'open Microsoft.FSharp.Control.WebExtensions' to access this method
//val it : Async<string> = Microsoft.FSharp.Control.FSharpAsync`1[System.String]

open System.IO
let numImages = 200
let size = 512
let numPixels = size * size

let makeImageFiles () =
    printfn "making %d %dx%d images... " numImages size size
    let pixels = Array.init numPixels (fun i -> byte i)
    for i = 1 to numImages  do
        System.IO.File.WriteAllBytes(sprintf "Image%d.tmp" i, pixels)
    printfn "done."

let processImageRepeats = 20

let transformImage (pixels, imageNum) =
    printfn "transformImage %d" imageNum;
    // Perform a CPU-intensive operation on the image.
    for i in 1 .. processImageRepeats do 
        pixels |> Array.map (fun b -> b + 1uy) |> ignore
    pixels |> Array.map (fun b -> b + 1uy)

let processImageSync i =
    use inStream =  File.OpenRead(sprintf "Image%d.tmp" i)
    let pixels = Array.zeroCreate numPixels
    let nPixels = inStream.Read(pixels, 0, numPixels);
    let pixels' = transformImage(pixels, i)
    use outStream =  File.OpenWrite(sprintf "Image%d.done" i)
    outStream.Write(pixels', 0, numPixels)

let processImagesSync () =
    printfn "processImagesSync...";
    for i in 1 .. numImages do
        processImageSync(i)
//val numImages : int = 200
//val size : int = 512
//val numPixels : int = 262144
//val makeImageFiles : unit -> unit
//val processImageRepeats : int = 20
//val transformImage : pixels:byte [] * imageNum:int32 -> byte []
//val processImageSync : i:int32 -> unit
//val processImagesSync : unit -> unit

> System.Environment.CurrentDirectory <- __SOURCE_DIRECTORY__;;
> System.Environment.CurrentDirectory <- @"C:\temp\11AsyncImages";;
> makeImageFiles();;

#time "off"
#time "on"

let processImageAsync i =
    async {use inStream = File.OpenRead(sprintf "Image%d.tmp" i)
           let! pixels = inStream.AsyncRead(numPixels)
           let  pixels' = transformImage(pixels, i)
           use outStream = File.OpenWrite(sprintf "Image%d.done" i)
           do! outStream.AsyncWrite(pixels')}

let processImagesAsync() =
    printfn "processImagesAsync...";
    let tasks = [for i in 1 .. numImages -> processImageAsync(i)]
    Async.RunSynchronously (Async.Parallel tasks) |> ignore
    printfn "processImagesAsync finished!"

//val processImageAsync : i:int32 -> Async<unit>
//val processImagesAsync : unit -> unit

Async.FromContinuations
//val it :
//  arg00:(('a -> unit) * (exn -> unit) *
//         (System.OperationCanceledException -> unit) -> unit) -> Async<'a>
Async.FromBeginEnd
//error FS0505: The member or object constructor 'FromBeginEnd' does not take 1 argument(s). An overload was found taking 3 arguments.
Async.AwaitTask
//val it : arg00:System.Threading.Tasks.Task<'a> -> Async<'a>
Async.Parallel
//al it : arg00:seq<Async<'a>> -> Async<'a []>
Async.RunSynchronously
//val it : arg00:Async<'a> -> 'a
Async.Start
//val it : arg00:Async<unit> -> unit
Async.StartImmediate
//val it : arg00:Async<unit> -> unit
Async.StartChild
//val it : arg00:Async<'a> -> Async<Async<'a>>

Stream.AsyncRead
Stream.AsyncWrite
Socket.AsyncAccept
Socket.AsyncReceive
Socket.AsyncSend
WebRequest.AsyncGetResponse
SqlCommand.AsyncExecuteReader
SqlCommand.AsyncExecuteXmlReader
SqlCommand.AsynExecuteNonQuery

Async.AwaitTask
//val it : arg00:System.Threading.Tasks.Task<'a> -> Async<'a>
Async.StartAsTask
//val it : arg00:Async<'a> -> System.Threading.Tasks.Task<'a>

> let failingTask = async {do failwith "fail"};;
//val failingTask: Async<unit>

> Async.RunSynchronously failingTask;;
System.Exception: fail
   at Microsoft.FSharp.Control.AsyncBuilderImpl.commit[a](Result`1 res)
   at Microsoft.FSharp.Control.CancellationTokenOps.RunSynchronously[a](CancellationToken token, FSharpAsync`1 computation, FSharpOption`1 timeout)
   at Microsoft.FSharp.Control.FSharpAsync.RunSynchronously[T](FSharpAsync`1 computation, FSharpOption`1 timeout, FSharpOption`1 cancellationToken)
   at <StartupCode$FSI_0036>.$FSI_0036.main@()
Stopped due to error

> let failingTasks = [async {do failwith "fail A"};
                      async {do failwith "fail B"}];;
//val failingTasks : Async<unit> list =
//  [Microsoft.FSharp.Control.FSharpAsync`1[Microsoft.FSharp.Core.Unit];
//   Microsoft.FSharp.Control.FSharpAsync`1[Microsoft.FSharp.Core.Unit]]

> Async.RunSynchronously (Async.Parallel failingTasks);;
//System.Exception: fail B
//   at Microsoft.FSharp.Control.AsyncBuilderImpl.commit[a](Result`1 res)
//   at Microsoft.FSharp.Control.CancellationTokenOps.RunSynchronously[a](CancellationToken token, FSharpAsync`1 computation, FSharpOption`1 timeout)
//   at Microsoft.FSharp.Control.FSharpAsync.RunSynchronously[T](FSharpAsync`1 computation, FSharpOption`1 timeout, FSharpOption`1 cancellationToken)
//   at <StartupCode$FSI_0039>.$FSI_0039.main@()
//Stopped due to error


> Async.RunSynchronously (Async.Parallel failingTasks);;
//Microsoft.FSharp.Core.FailureException: fail B
//stopped due to error

Async.Catch
//static member Catch : computation:Async<'T> -> Async<Choice<'T,exn>>
//val it : arg00:Async<'a> -> Async<Choice<'a,exn>>

> Async.RunSynchronously (Async.Catch failingTask);;
//val it : Choice<unit,exn> =
//  Choice2Of2
//    System.Exception: fail
//  ...

Async.Parallel
//val it : arg00:seq<Async<'a>> -> Async<'a []>

let forkJoinParallel(taskSeq) =
    Async.FromContinuations (fun (cont, econt, ccont) ->
        let tasks = Seq.toArray taskSeq
        let count = ref tasks.Length
        let results = Array.zeroCreate tasks.Length
        tasks |> Array.iteri (fun i p ->
            Async.Start
               (async {let! res = p
                       results.[i] <- res;
                       let n = System.Threading.Interlocked.Decrement(count)
                       if n = 0 then cont results})))
//val forkJoinParallel : taskSeq:seq<Async<'a>> -> Async<'a []>

open System.Threading
open System

// Initialize an array by a parallel init using all available processors
// Note, this primitive doesn't support cancellation.
let parallelArrayInit n f = 
   let currentLine = ref -1
   let res = Array.zeroCreate n
   let rec loop () = 
       let y = Interlocked.Increment(&currentLine.contents)
       if y < n then res.[y] <- f y; loop()

   // Start just the right number of tasks, one for each physical CPU
   Async.Parallel [for i in 1 .. Environment.ProcessorCount -> async {do loop()}]
      |> Async.Ignore 
      |> Async.RunSynchronously

   res
//val parallelArrayInit : n:int -> f:(int -> 'a) -> 'a []

> let rec fib x = if x < 2 then 1 else fib (x - 1) + fib (x - 2)
//val fib : x:int -> int

> parallelArrayInit 25 (fun x -> fib x);;
//val it : int [] =
//  [|1; 1; 2; 3; 5; 8; 13; 21; 34; 55; 89; 144; 233; 377; 610; 987; 1597; 2584;
//    4181; 6765; 10946; 17711; 28657; 46368; 75025; 121393; 196418; 317811;
//    514229; 832040|]

type Agent<'T> = MailboxProcessor<'T>

let counter =
    new Agent<_>(fun inbox ->
        let rec loop n =
            async {printfn "n = %d, waiting..." n
                   let! msg = inbox.Receive()
                   return! loop (n + msg)}
        loop 0)
//type Agent<'T> = MailboxProcessor<'T>
//val counter : Agent<int>

> counter.Start();;
//n = 0, waiting...

> counter.Post(1);;
//n = 1, waiting...

> counter.Post(2);;
//n = 3, waiting...

> counter.Post(1);;
//n = 4, waiting...

inbox.Receive
//member Receive: unit -> Async<'Message>

let agent =
    MailboxProcessor.Start(fun inbox ->

        // The states of the state machine
        let rec state1(args) = async { ... }
        and state2(args) = async { ... }
        ...
        and stateN(args) = async { ... }

        // Enter the initial state
        state1(initialArgs))

/// The internal type of messages for the agent
type internal msg = Increment of int | Fetch of AsyncReplyChannel<int> | Stop

type CountingAgent() =
    let counter = MailboxProcessor.Start(fun inbox ->
         // The states of the message-processing state machine...
         let rec loop n =
             async {let! msg = inbox.Receive()
                    match msg with
                    | Increment m ->
                        // increment and continue...
                        return! loop(n + m)
                    | Stop ->
                        // exit
                        return ()
                    | Fetch replyChannel  ->
                        // post response to reply channel and continue
                        do replyChannel.Reply n
                        return! loop n}

         // The initial state of the message-processing state machine...
         loop(0))

    member a.Increment(n) = counter.Post(Increment n)
    member a.Stop() = counter.Post Stop
    member a.Fetch() = counter.PostAndReply(fun replyChannel -> Fetch replyChannel)
//type internal msg =
//  | Increment of int
//  | Fetch of AsyncReplyChannel<int>
//  | Stop
//type CountingAgent =
//  class
//    new : unit -> CountingAgent
//    member Fetch : unit -> int
//    member Increment : n:int -> unit
//    member Stop : unit -> unit
//  end

> let counter = new CountingAgent();;
//val counter : CountingAgent

> counter.Increment(1);;

> counter.Fetch();;
//val it : int = 1

> counter.Increment(2);;

> counter.Fetch();;
//val it : int = 3

> counter.Stop();;

agent.Post
agent.Receive
agent.Scan
agent.TryReceive
agent.TryScan

type Message =
    | Message1
    | Message2 of int
    | Message3 of string

let agent =
    MailboxProcessor.Start(fun inbox ->
        let rec loop() =
            inbox.Scan(function
                | Message1 ->
                   Some (async {do printfn "message 1!"
                                return! loop()})
                | Message2 n ->
                   Some (async {do printfn "message 2!"
                                return! loop()})
                | Message3 _ ->
                   None)
        loop())
//type Message =
//  | Message1
//  | Message2 of int
//  | Message3 of string
//val agent : MailboxProcessor<Message>

> agent.Post(Message1);;
//message 1!
//val it : unit = ()

> agent.Post(Message2(100));;
//message 2!
//val it : unit = ()

> agent.Post(Message3("abc"));;
//val it : unit = ()

> agent.Post(Message2(100));;
message 2!
//val it : unit = ()

> agent.CurrentQueueLength;;
//val it : int = 1

open System.Collections.Generic
open System.Net
open System.IO
open System.Threading
open System.Text.RegularExpressions

let limit = 50
let linkPat = "href=\s*\"[^\"h]*(http://[^&\"]*)\""
let getLinks (txt : string) =
    [for m in Regex.Matches(txt, linkPat) -> m.Groups.Item(1).Value]

// A type that helps limit the number of active web requests
type RequestGate(n : int) =
    let semaphore = new Semaphore(initialCount = n, maximumCount = n)
    member x.AsyncAcquire(?timeout) =
        async {let! ok = Async.AwaitWaitHandle(semaphore,
                                               ?millisecondsTimeout = timeout)
               if ok then
                   return
                     {new System.IDisposable with
                         member x.Dispose() =
                             semaphore.Release() |> ignore}
               else
                   return! failwith "couldn't acquire a semaphore" }

// Gate the number of active web requests
let webRequestGate = RequestGate(5)

// Fetch the URL, and post the results to the urlCollector.
let collectLinks (url : string) =
    async { // An Async web request with a global gate
            let! html = async {
                // Acquire an entry in the webRequestGate. Release
                // it when 'holder' goes out of scope
                use! holder = webRequestGate.AsyncAcquire()

                let req = WebRequest.Create(url, Timeout = 5)

                // Wait for the WebResponse
                use! response = req.AsyncGetResponse()

                // Get the response stream
                use reader = new StreamReader(response.GetResponseStream())

                // Read the response stream (note: a synchronous read)
                return reader.ReadToEnd()}

            // Compute the links, synchronously
            let links = getLinks html

            // Report, synchronously
            do printfn "finished reading %s, got %d links" url (List.length links)

            // We're done
            return links}

/// 'urlCollector' is a single agent that receives URLs as messages. It creates new
/// asynchronous tasks that post messages back to this object.
let urlCollector =
    MailboxProcessor.Start(fun self ->

        // This is the main state of the urlCollector
        let rec waitForUrl (visited : Set<string>) = async {
            // Check the limit
            if visited.Count < limit then

                // Wait for a URL...
                let! url = self.Receive()
                if not (visited.Contains(url)) then
                    // Start off a new task for the new url. Each collects
                    // links and posts them back to the urlCollector.
                    do! Async.StartChild (async {
                        let! links = collectLinks url
                        for link in links do
                            self.Post link}) |> Async.Ignore

                // Recurse into the waiting state
                return! waitForUrl(visited.Add(url))}

        // This is the initial state.
        waitForUrl(Set.empty))
//val limit : int = 50
//val linkPat : string = "href=\s*"[^"h]*(http://[^&"]*)""
//val getLinks : txt:string -> string list
//type RequestGate =
//  class
//    new : n:int -> RequestGate
//    member AsyncAcquire : ?timeout:int -> Async<System.IDisposable>
//  end
//val webRequestGate : RequestGate
//val collectLinks : url:string -> Async<string list>
//val urlCollector : MailboxProcessor<string>

> urlCollector <-- "http://news.google.com";;
> urlCollector.Post "http://news.google.com";;
//finished reading http://news.google.com, got 191 links
//finished reading http://news.google.com/?output=rss, got 0 links
//finished reading http://www.ktvu.com/politics/13732578/detail.html, got 14 links
//finished reading http://www.washingtonpost.com/wp-dyn/content/art..., got 218 links
//finished reading http://www.newsobserver.com/politics/story/646..., got 56 links
//finished reading http://www.foxnews.com/story/0,2933,290307,0...l, got 22 links
//...

> open System.Windows.Forms;;

> let form = new Form(Text = "Click Form", Visible = true, TopMost = true);;
//val form : Form

> form.Click |> Observable.add (fun evArgs -> printfn "Clicked!");;
//val it : unit = ()

open System.Threading
let t = new Thread(ThreadStart(fun _ ->
                printfn "Thread %d: Hello" Thread.CurrentThread.ManagedThreadId));
t.Start();
printfn "Thread %d: Waiting!" Thread.CurrentThread.ManagedThreadId
t.Join();
printfn "Done!"

//val t : Thread
//Thread 1: Waiting!
//Thread 10: Hello
//Done!

type MutablePair<'T, 'U>(x : 'T, y : 'U) =
    let mutable currentX = x
    let mutable currentY = y
    member p.Value = (currentX, currentY)
    member p.Update(x, y) =
        // Race condition: This pair of updates is not atomic
        currentX <- x
        currentY <- y

let p = new MutablePair<_, _>(1, 2)
do Async.Start (async {do (while true do p.Update(10, 10))})
do Async.Start (async {do (while true do p.Update(20, 20))})

open System.Threading
let lock (lockobj : obj) f  =
    Monitor.Enter lockobj
    try
        f()
    finally
        Monitor.Exit lockobj

do Async.Start (async {do (while true do lock p (fun () -> p.Update(10, 10)))})
do Async.Start (async {do (while true do lock p (fun () -> p.Update(20, 20)))})
//type MutablePair<'T,'U> =
//  class
//    new : x:'T * y:'U -> MutablePair<'T,'U>
//    member Update : x:'T * y:'U -> unit
//    member Value : 'T * 'U
//  end
//val p : MutablePair<int,int>
//val lock : lockobj:obj -> f:(unit -> 'a) -> 'a
//val it : unit = ()

open System.Threading

let readLock (rwlock : ReaderWriterLock) f  =
  rwlock.AcquireReaderLock(Timeout.Infinite)
  try
      f()
  finally
      rwlock.ReleaseReaderLock()

let writeLock (rwlock : ReaderWriterLock) f  =
  rwlock.AcquireWriterLock(Timeout.Infinite)
  try
      f()
      Thread.MemoryBarrier()
  finally
      rwlock.ReleaseWriterLock()
//val readLock :
//  rwlock:System.Threading.ReaderWriterLock -> f:(unit -> 'a) -> 'a
//val writeLock :
//  rwlock:System.Threading.ReaderWriterLock -> f:(unit -> unit) -> unit

Listing 11-15 shows how to use these functions to protect the MutablePair class.
Listing 11-15. Shared-Memory Code with a Race Condition
type MutablePair<'T, 'U>(x : 'T, y : 'U) =
    let mutable currentX = x
    let mutable currentY = y
    let rwlock = new ReaderWriterLock()
    member p.Value =
        readLock rwlock (fun () ->
            (currentX, currentY))
    member p.Update(x, y) =
        writeLock rwlock (fun () ->
            currentX <- x
            currentY <- y)
type MutablePair<'T,'U> =
  class
    new : x:'T * y:'U -> MutablePair<'T,'U>
    member Update : x:'T * y:'U -> unit
    member Value : 'T * 'U
  end

System.Threading.WaitHandle
System.Threading.AutoResetEvent
System.Threading.ManualResetEvent
System.Threading.Mutex
System.Threading.Semaphore
System.Threading.Interlocked