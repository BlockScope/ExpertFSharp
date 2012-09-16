open System.Net
open System.Net.Sockets
open System.IO
open System.Text

type AsyncTcpServer(addr, port, handleServerRequest) = 
    let socket = new TcpListener(addr, port)
    member x.Start() = async {do x.Run()} |> Async.Start
    member x.Run() = 
        socket.Start()
        while true do
            let client = socket.AcceptTcpClient()
            async {try do! handleServerRequest client with e -> ()}
            |> Async.Start

let quoteSize = 8
let quoteHeaderSize = 4
let quoteSeriesLength = 3
let header = Array.init<byte> quoteSize (fun i -> 1uy)
let quote = Array.init<byte> quoteSize (fun i -> byte(i % 256))

let handleRequest (client : TcpClient) = async {
    use stream = client.GetStream()
    do! stream.AsyncWrite(header, 0, quoteHeaderSize)  // write header
    for _ in [0 .. quoteSeriesLength] do
        do! stream.AsyncWrite(quote, 0, quote.Length) 
        // Mock an I/O wait for the next quote
        do! Async.Sleep 1000}

let server = new AsyncTcpServer(IPAddress.Loopback, 10003, handleRequest)

let printQuotes = async {
    let client = new TcpClient()
    client.Connect(IPAddress.Loopback, 10003)
    use stream = client.GetStream()
    let header = Array.create quoteHeaderSize 0uy
    let! readHeader = stream.AsyncRead(header, 0, quoteHeaderSize)
    if readHeader = 0 then return () else printfn "Header: %A" header
    while true do
        let buffer = Array.create quoteSize 0uy
        let! read = stream.AsyncRead(buffer, 0, quoteSize)
        if read = 0 then return () else printfn "Quote: %A" buffer}
//type AsyncTcpServer =
//  class
//    new : addr:System.Net.IPAddress * port:int *
//          handleServerRequest:(System.Net.Sockets.TcpClient -> Async<unit>) ->
//            AsyncTcpServer
//    member Run : unit -> unit
//    member Start : unit -> unit
//  end
//val quoteSize : int = 8
//val quoteHeaderSize : int = 4
//val quoteSeriesLength : int = 3
//val header : byte [] = [|1uy; 1uy; 1uy; 1uy; 1uy; 1uy; 1uy; 1uy|]
//val quote : byte [] = [|0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy|]
//val handleRequest : client:System.Net.Sockets.TcpClient -> Async<unit>
//val server : AsyncTcpServer
//val printQuotes : Async<unit>

server.Start()
Async.Start printQuotes
//Header: [|1uy; 1uy; 1uy; 1uy|]
//Quote: [|0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy|]
//Quote: [|0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy|]
//Quote: [|0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy|]
//Quote: [|0uy; 1uy; 2uy; 3uy; 4uy; 5uy; 6uy; 7uy|]
