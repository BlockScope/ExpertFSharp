(* CODE: Page 7a *)
/// Split a string into words at spaces
let splitAtSpaces (text:string) = 
    text.Split ' ' 
    |> Array.toList 

/// Analyze a string for duplicate words
let wordCount text =
    let words = splitAtSpaces text
    let wordSet = Set.ofList words
    let numWords = words.Length
    let numDups = numWords - wordSet.Count
    (numWords, numDups)

/// Analyze a string for duplicate words and display the results.
let showWordCount text =
    let numWords,numDups = wordCount text
    printfn "--> %d words in the text" numWords
    printfn "--> %d duplicate words" numDups

(* OUTPUT: Page 8a *)
//val splitAtSpaces : text:string -> string list
//val wordCount : text:string -> int * int
//val showWordCount : text:string -> unit

let (numWords, numDups) = wordCount "All the king's horses and all the king's men";;
(* OUTPUT: Page 8b *)
//val numWords : int = 9
//val numDups : int = 2

showWordCount "Couldn't put Humpty together again";;
(* OUTPUT: Page 8c *)
//--> 5 words in the text
//--> 0 duplicate words
//val it : unit = ()

(* SNIPPET: Page 9a *)
//let wordCount (text:string) =
//    let words = ...

wordCount;;
(* OUTPUT: Page 10a *)
//val it : (string -> int * int) = <fun:it@36>

(* SNIPPET: Page 12a *)
//let wordCount (text: string) =
//    let words = splitAtSpaces text

splitAtSpaces "hello world";;
(* OUTPUT: Page 13a *)
//val it : string list = [ "hello"; "world" ]

(* CODE: Page 13b *)
/// Analyze a string for duplicate words
let wordCount text =
    let words = splitAtSpaces text in
    let wordSet = Set.ofList words in
    let numWords = words.Length in
    let numDups = numWords - wordSet.Count in
    (numWords, numDups)

let powerOfFour n =
    let nSquared = n * n in nSquared * nSquared

powerOfFour 3;;
(* OUTPUT: Page 14a *)
//val it : int = 81

let badDefinition1 =
    let words = splitAtSpaces text
//                              ^^^^  error: text is not defined
    let text = "We three kings"
    words.Length

//gives
//      let words = splitAtSpaces text
//  ------------------------------^^^^
(* OUTPUT: Page 14a *)
//error FS0039: The value or constructor 'text' is not defined

let badDefinition2 = badDefinition2 + 1

(* OUTPUT: Page 14b *)
//error FS0039: The value or constructor 'badDefinition2' is not defined

(* CODE: Page 15a *)
let powerOfFourPlusTwo n =
    let n = n * n
    let n = n * n
    let n = n + 2
    n

let powerOfFourPlusTwo n =
    let n1 = n * n
    let n2 = n1 * n1
    let n3 = n2 + 2
    n3

(* CODE: Page 16a *)
let powerOfFourPlusTwoTimesSix n =
    let n3 =
        let n1 = n * n
        let n2 = n1 * n1
        n2 + 2
    let n4 = n3 * 6
    n4

let invalidFunction n =
    let n3 =
        let n1 = n + n
        let n2 = n1 * n1
        n1 * n2
    let n4 = n1 + n2 + n3     // Error! n3 is in scope, but n1 and n2 are not!
    n4

(* MISSING-OUTPUT: Page 16a *)
//Script.fsx(110,14): error FS0039: The value or constructor 'n1' is not defined

(* SNIPPET: Page 16a *)
//let wordCount (text:string) =
//    let words = splitAtSpaces text
//    let wordSet = Set.ofList words
//    ...

Set.ofList ["b"; "a"; "b"; "b"; "c"];;
(* OUTPUT: Page 17a *)
//val it : Set<string> = set ["a"; "b"; "c"]

Set.toList (Set.ofList ["abc"; "ABC"]);;
(* OUTPUT: Page 17b *)
//val it : string list = ["ABC"; "abc"]

(* SNIPPET: Page 17a *)
//  let numWords = words.Length
//  let numDups = numWords - wordSet.Count

let length (inp:'T list) = inp.Length
(* MISSING-OUTPUT: Page 18a *)
//val length : inp:'T list -> int

(* SNIPPET: Page 18a *)
//let numWords = List.length words
//let numDups = numWords - Set.count wordSet

let length inp = inp.Length;;
(* OUTPUT: Page 18a *)
//error FS0072: Lookup on object of indeterminate type based on information prior to this program point. A type annotation may be needed prior to this program point to constrain the type of the object. This may allow the lookup to be resolved.

(* SNIPPET: Page 19a *)
//    ...
//    let numWords = words.Length
//    let numDups = numWords - wordSet.Count
//    (numWords, numDups)

let site1 = ("www.cnn.com", 10)
let site2 = ("news.bbc.com", 5)
let site3 = ("www.msnbc.com", 4)
let sites = (site1, site2, site3)
(* OUTPUT: Page 19a *)
//val site1 : string * int = ("www.cnn.com", 10)
//val site2 : string * int = ("news.bbc.com", 5)
//val site3 : string * int = ("www.msnbc.com", 4)
//val sites : (string * int) * (string * int) * (string * int) =
//  (("www.cnn.com", 10), ("news.bbc.com", 5), ("www.msnbc.com", 4))

fst site1;;
(* OUTPUT: Page 19b *)
//val it : string = "www.cnn.com"

let relevance = snd site1;;
(* OUTPUT: Page 20a *)
//val relevance : int = 10

(* SNIPPET: Page 20a *)
//let fst (a, b) = a
//let snd (a, b) = b

let url, relevance = site1
let siteA, siteB, siteC = sites
(* MISSING-OUTPUT: Page 20a *)
//val url : string = "www.cnn.com"
//val relevance : int = 10
//val siteC : string * int = ("www.msnbc.com", 4)
//val siteB : string * int = ("news.bbc.com", 5)
//val siteA : string * int = ("www.cnn.com", 10)

let a, b = (1, 2, 3);;
(* OUTPUT: Page 20b *)
//error FS0001: Type mismatch. Expecting a
//    'a * 'b    
//but given a
//    'a * 'b * 'c    
//The tuples have differing lengths of 2 and 3

let showResults (numWords, numDups) =
    printfn "--> %d words in the text" numWords
    printfn "--> %d duplicate words" numDups

let showWordCount text = showResults (wordCount text)

(* MISSING-OUTPUT: Page 20b *)
//val showResults : int * int -> unit
//val showWordCount : string -> unit

printfn "--> %d words in the text" numWords
printfn "--> %d duplicate words" numDups
(* MISSING-OUTPUT: Page 21a *)
//--> 9 words in the text
//--> 2 duplicate words

(* SNIPPET: Page 22a *)
//System.Console.WriteLine("--> {0} words in the text", box numWords)
//System.Console.WriteLine("--> {0} duplicate words", box numDups)

let two = (printfn "Hello World"; 1 + 1)
let four = two + two
(* MISSING-OUTPUT: Page 20b *)
//Hello World
//
//val two : int = 2
//val four : int = 4

(printfn "--> %d words in the text" numWords;
 printfn "--> %d duplicate words" numDups)
(* MISSING-OUTPUT: Page 22a *)
//--> 9 words in the text
//--> 2 duplicate words
//val it : unit = ()

/// Split a string into words at spaces
let splitAtSpaces (text:string) = 
    text.Split ' ' 
    |> Array.toList 
(* MISSING-OUTPUT: Page 23a *)
//val splitAtSpaces : string -> string list

open System.Windows.Forms

let form = new Form(Visible = true, TopMost = true, Text = "Welcome to F#")

let textB = new RichTextBox(Dock = DockStyle.Fill, Text = "Here is some initial text")
form.Controls.Add textB

open System.IO
open System.Net

/// Get the contents of the URL via a web request
let http (url:string) =
    let req = System.Net.WebRequest.Create(url)
    let resp = req.GetResponse()
    let stream = resp.GetResponseStream()
    let reader = new StreamReader(stream)
    let html = reader.ReadToEnd()
    resp.Close()
    html

textB.Text <- http "http://news.bbc.co.uk"
(* GUI-OUTPUT: Page 23a *)

(* SNIPPET: Page 24a *)
//open System.Windows.Forms

(* SNIPPET: Page 24b *)
let form = new System.Windows.Forms.Form(Visible = true, TopMost = true, Text = "Welcome to F#")

(* SNIPPET: Page 25a *)
let form = new Form(Visible = true, TopMost = true, Text = "Welcome to F#")

open System.Windows.Forms
let form = new Form()
form.Visible <- true
form.TopMost <- true
form.Text <- "Welcome to F#"

form.Text <- "Programming is Fun!"

let form2 = form
form2.Text <- "F# Forms are Fun"

let textB = new RichTextBox(Dock = DockStyle.Fill)
form.Controls.Add(textB)
(* GUI-OUTPUT: Page 27a *)

open System;;
open System.IO;;
open System.Net;;

let req = WebRequest.Create("http://www.microsoft.com");;
(* OUTPUT: Page 28a *)
//val req : WebRequest

let resp = req.GetResponse();;
(* OUTPUT: Page 28b *)
//val resp : WebResponse

let stream = resp.GetResponseStream();;
(* OUTPUT: Page 28c *)
//val stream : Stream

let reader = new StreamReader(stream);;
(* OUTPUT: Page 28d *)
//val reader : StreamReader

let html = reader.ReadToEnd();;
(* OUTPUT: Page 28e *)
//val html : string =
//  "<html><head><title>Microsoft Corporation</title><meta http-eq"+[959 chars]

textB.Text <- html;;
(* GUI-OUTPUT: Page 28a *)

http;;
(* OUTPUT: Page 28f *)
//val http : string -> string

(* SNIPPET: Page 17a - to support review comment on underscore being used as a type variable name. *)
let len (s : Set<_>) = s.Count
let len (s : Set<'T>) = s.Count
let len (s : 'T Set) = s.Count
let len (s : _ Set) = s.Count
let fst ((a,  b) : 'a * 'b) = a
let fst ((a,  b) : 'a * _) = a
let fst ((a,  b) : _ * 'b) = a
let fst ((a,  b) : _ * _) = a

(* SNIPPET: Page 20a - to support review comment on tuples and the absence of enclosing parens. *) 
let a = (1, 2);;
let a = 1, 2;;
let (a) = 1, 2;;
//val a : int * int = (1, 2)

let a,b = (1,2);;
let (a,b) = (1,2);;
let a,b = 1,2;;
let (a,b) = 1,2;;
//val b : int = 2
//val a : int = 1

let [a; b] = [1; 2];;
//warning FS0025: Incomplete pattern matches on this expression. For example, the value '[_;_;_]' may indicate a case not covered by the pattern(s).
//val b : int = 2
//val a : int = 1

let a::b::_ = [1; 2];;
//warning FS0025: Incomplete pattern matches on this expression. For example, the value '[_]' may indicate a case not covered by the pattern(s).
//val b : int = 2
//val a : int = 1

let a; b = [1; 2];;
//error FS0010: Unexpected symbol ';' in binding. Expected '=' or other token.

let [|a; b|] = [|1; 2|];;
//warning FS0025: Incomplete pattern matches on this expression. For example, the value '[|_; _; _|]' may indicate a case not covered by the pattern(s).
//val b : int = 2
//val a : int = 1

let a; b = [|1; 2|];;
//error FS0010: Unexpected symbol ';' in binding. Expected '=' or other token.