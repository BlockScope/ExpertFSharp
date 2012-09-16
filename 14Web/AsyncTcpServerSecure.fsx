open System.Net
open System.Net.Sockets
open System.IO
open System.Text
open System.Security.Cryptography.X509Certificates
open System.Net.Security
open System.Security.Authentication

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

let handleRequestStream (stream : Stream) = async {
    do! stream.AsyncWrite(header, 0, quoteHeaderSize)  // write header
    for _ in [0 .. quoteSeriesLength] do
        do! stream.AsyncWrite(quote, 0, quote.Length) 
        // Mock an I/O wait for the next quote
        do! Async.Sleep 1000}

let handleRequest (client : TcpClient) = async {
    use stream = client.GetStream()
    do! handleRequestStream stream}

type AsyncTcpServerSecure(addr, port, handleServerRequest) = 

    /// Gets the first self-signed certificate with a friendly name of localhost.
    let getCertficate() =
        let store = new X509Store(StoreName.My, StoreLocation.LocalMachine)
        store.Open(OpenFlags.ReadOnly)
        let certs =
            store.Certificates.Find(
                findType = X509FindType.FindBySubjectName,
                findValue = Dns.GetHostName(),
                validOnly = true)
        seq {
        for c in certs do if c.FriendlyName = "localhost" then yield Some(c)
        yield None}
        |> Seq.head

    let handleServerRequestSecure (client : TcpClient) = 
        async {
            let cert = getCertficate()
            if cert.IsNone then printfn "No cert"; return ()
            let stream = client.GetStream()
            let sslStream = new SslStream(innerStream = stream, leaveInnerStreamOpen = true)
            try
                sslStream.AuthenticateAsServer(
                    serverCertificate = cert.Value,
                    clientCertificateRequired = false,
                    enabledSslProtocols = SslProtocols.Default,
                    checkCertificateRevocation = false)
            with _ -> printfn "Can't authenticate"; return()
            
            printfn "IsAuthenticated: %A" sslStream.IsAuthenticated
            if sslStream.IsAuthenticated then
                // In this example only the server is authenticated.
                printfn "IsEncrypted: %A" sslStream.IsEncrypted
                printfn "IsSigned: %A" sslStream.IsSigned

                // Indicates whether the current side of the connection 
                // is authenticated as a server.
                printfn "IsServer: %A" sslStream.IsServer

            return! handleRequestStream stream
        }

    let server = AsyncTcpServer(addr, port, handleServerRequestSecure)

    member x.Start() = server.Start()

let server = new AsyncTcpServerSecure(IPAddress.Loopback, 10003, handleRequest)

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
//val handleRequestStream : stream:System.IO.Stream -> Async<unit>
//val handleRequest : client:System.Net.Sockets.TcpClient -> Async<unit>
//type AsyncTcpServerSecure =
//  class
//    new : addr:System.Net.IPAddress * port:int *
//          handleServerRequest:(System.Net.Sockets.TcpClient -> Async<unit>) ->
//            AsyncTcpServerSecure
//    member Start : unit -> unit
//  end
//val server : AsyncTcpServerSecure
//val printQuotes : Async<unit>

// On Windows 7, to create a self-signed certificate, open IIS7, server certificates
// and choose the action create self-signed certificate.
server.Start()
Async.Start printQuotes
