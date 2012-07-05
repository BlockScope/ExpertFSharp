> let buf = new System.Text.StringBuilder();;
//val buf : System.Text.StringBuilder

> buf.Append("Humpty Dumpty");;

> buf.Append(" sat on the wall");;

> buf.ToString();;

//val it : string = "Humpty Dumpty sat on the wall"

//Table 3-6. String and Character Literals
"Humpty Dumpty"
"c:\\Program Files"
@"c:\Program Files"
""@"c:\Program I "like" youFiles"""
"xyZy3d2"B
'c'	

> "MAGIC"B;;
//val it : byte [] = [|77uy; 65uy; 71uy; 73uy; 67uy|]

> let dir  = @"c:\Program Files";;
//val dir : string = "c:\Program Files"

> let text  = """I "like" you""";;
//val text : string = "I "like" you"

> let s = "All the kings horses
- and all the kings men""";;

//val s : string = " All the kings horses 
//and all the kings men"

> let s2 = """All the kings' "horses"
- and all the kings men""";;

//val s2 : string = " All the kings' "horses" 
//and all the kings men"

(*
Table 3-7. Escape Characters in Nonverbatim Strings
\n	New line
10	"\n"
\r	Carriage return
13	"\r"
\t	Tab	9	"\t"
\b	Backspace	8	
\NNN	Trigraph	NNN	"\032" (space)
\uNNNN	Unicode character
NNNN	"\u00a9" 
\UNNNNNNNN	Long Unicode character
NNNN NNNN	"\U00002260"
*)

> sprintf "Name: %s, Age: %d" "Anna" 3;;
//val it : string = "Name: Anna, Age: 3"

> sprintf "Name: %s, Age: %d" 3 10;;
------------------------------^

error: FS0001: This expression was expected to have type string but here 
has type int

(*
Table 4-5. Formatting Codes for printf-style String and Output Formatting
Code	Type Accepted	Notes
%b	bool	Prints true or false
%s	string	Prints the string
%d, %x, %X, %o, %u	int/int32	Decimal/hex/octal format for any integer types
%e, %E, %f, %g	float	Floating-point formats
%M	decimal	See the .NET documentation
%A	Any type	Uses structured formatting, discussed in the section “Generic Structural Formatting” and in Chapter 5
%+A	Any type	Uses structured formatting, but accesses to private internals of data structures if they are accessible via reflection
%O	Any type	Uses Object.ToString()
%a	Any type	Takes two arguments: one is a formatting function, and one is the value to format
%t	Function	Runs the function given as an argument
*)

> System.DateTime.Now.ToString();;
//val it : string = "28/06/20.. 17:14:07 PM"

> sprintf "It is now %O" System.DateTime.Now;;
//val it : string = "It is now 28/06/20... 17:14:09"

> printf "The result is %A\n" [1;2;3];;

//"The result is [1; 2; 3]"

> System.String.Format("{0} {1} {2}", 12, "a", 1.23);;
//val it : string = "12 a 1.23"

> open System;;
> DateTime.Parse("13 July 1968");;

val it : DateTime = 13/07/1968 00:00:00 {Day = 13;
                                         DayOfWeek = Saturday;
                                         DayOfYear = 195;
                                         Hour = 0;
                                         Millisecond = 0;
                                         Minute = 0;
                                         Month = 7;
                                         Second = 0;
                                         Ticks = 620892000000000000L;
                                         TimeOfDay = 00:00:00;
                                         Year = 1968;}

> let date x = DateTime.Parse(x);;
//val date : string -> DateTime

> printfn "date = %A" (date "13 July 1968");;
//date = 13/07/1968 00:00:00

> printfn "birth = %A" (date "18 March 2003, 6:21:01pm");;
//birth = 18/03/2003 18:21:01

> open System;;

> Uri.TryCreate("http://www.thebritishmuseum.ac.uk/", UriKind.Absolute);;
//val it : bool * System.Uri = (true,   http://www.thebritishmuseum.ac.uk/)

> Uri.TryCreate("e3£%//ww.gibberish.com", UriKind.Absolute);;
//val it : bool * Uri = (false, null)

> let line = "Smith, John, 20 January 1986, Software Developer";;
//val line : string = "Smith, John, 20 January 1986, Software Developer"

> line.Split ',';;
//val it : string [] = [|"Smith"; " John"; " 20 January 1986"; " Software Developer"|]

> line.Split ',' |> Array.map (fun s -> s.Trim());;
//val it : string [] = [|"Smith"; "John"; "20 January 1986"; "Software Developer"|]

let splitLine (line: string) =
    line.Split [| ',' |] |> Array.map (fun s -> s.Trim())

let parseEmployee (line: string) =
    match splitLine line with
    | [| last; first; startDate; title |] ->
        last, first, System.DateTime.Parse(startDate), title
    | _ ->
        failwithf "invalid employee format: '%s'" line

//val parseEmployee : string -> string * string * System.DateTime * string

> parseEmployee line;;

//val it : string * string * System.DateTime * string
//       = ("Smith", "John", 20/01/1986 00:00:00 { ... }, "Software Developer")

File.WriteAllLines("employees.txt", Array.create 10000 line)

let readEmployees (fileName : string) = 
    fileName |> File.ReadLines |> Seq.map parseEmployee

let firstThree = readEmployees "employees.txt" |> Seq.truncate 3 |> Seq.toList

> firstThree |> Seq.iter (fun (last,first,startDate,title)  -> 
      printfn "%s %s started on %A" first last startDate);;

//John Smith started on 20/01/1986 00:00:00
//John Smith started on 20/01/1986 00:00:00
//John Smith started on 20/01/1986 00:00:00

open System.Text.RegularExpressions

let parseHttpRequest line =
    let result = Regex.Match(line, @"GET (.*?) HTTP/1\.([01])$")
    let file = result.Groups.[1].Value
    let version = result.Groups.[2].Value
    file, version

open System.Text.RegularExpressions
let regex s = new Regex(s)

let (=~) s (re:Regex) = re.IsMatch(s)
let (<>~) s (re:Regex) = not (s =~ re)

//val regex : string -> Regex
//val ( =~ ) : string -> Regex -> bool
//val ( <>~ ) : string -> Regex -> bool

> let samplestring = "This is a string";;
//val samplestring : string

> if samplestring =~ regex "his" then
       printfn "A Match! ";;
//A Match!

> "This is a string" =~ regex "(is )+";;
//val it : bool = true

> (regex( " ").Split("This is a string");;
//val it : string [] = [|"This"; "is"; "a"; "string"|]

> (regex( @"\s+").Split("I'm a little      teapot");;
//val it : string [] = [|"I'm"; "a"; "little"; "teapot"|]

> (regex( @"\s+").Split("I'm a little  \t\t\n\t\n\t teapot");;
//val it : string [] = [|"I'm"; "a"; "little"; "teapot"|]

> let m = (regex( @"joe").Match("maryjoewashere");;
//val m : Match

> if m.Success then
     printfn "Matched at position %d" m.Index;;

//Matched at position 4

> let text = "was a dark and stormy night";;
//val text: string

> let t2 = (regex( @"\w+").Replace(text, "WORD");;
//val t2: string  = "WORD WORD WORD WORD WORD WORD"

> samplestring =~ regex "(?i)HIS";;
//val it : bool = true

> samplestring =~ regex "HIS";;
//val it : bool = false

let entry = @"
Jolly Jethro
13 Kings Parade
Cambridge, Cambs CB2 1TJ
"

let re =
 regex @"(?<=\n)\s*(?<city>[^\n]+)\s*,\s*(?<county>\w+)\s+(?<pcode>.{3}\s*.{3}).*$"

> let r = re.Match(entry);;
//val r : Match

> r.Groups.["city"].Value;;
//val it : string = "Cambridge"

> r.Groups.["county"].Value;;
//val it : string = "Cambs"

> r.Groups.["pcode"].Value;;
//val it : string = "CB2 1TJ"

let (|IsMatch|_|) (re: string) (inp:string) = 
    if Regex(re).IsMatch(inp)  then Some() else None
    
let (|MatchGroups|_|) (re: string) (inp:string) = 
    let results = Regex(re).Match(inp) 
    if results.Success then Some results.Groups else None

> match "This is a string" with 
| IsMatch "(?i)HIS" -> "yes, it matched"
| IsMatch "ABC" -> "this would not match"
| _ -> "nothing matched"

//val it : string = "yes, it matched " 

let firstAndSecondWord (inp:string) = 
    let re = regex "(?<word1>\w+)\s+(?<word2>\w+)"
    let results = re.Match(inp) 
    if results.Success then 
        Some (results.Groups.["word1"].Value, results.Groups.["word2"].Value)
    else 
        None

> match matchTwo "This is a super string" with 
  | MatchGroups "(?<word1>T\w+).*(?<word2>s\w+)" groups -> 
      (groups.["word1"].Value, groups.["word2"].Value)
  | _ -> failwith "nothing matched"

//val it : string * string = ("This", "stringis")

let (?) (results:Match) (name:string) = 
    results.Groups.[name].Value

let firstAndSecondWord (inp:string) = 
    let re = regex "(?<word1>\w+)\s+(?<word2>\w+)"
    let results = re.Match(inp) 
    if results.Success then 
        Some (results?word1, results?word2)
    else 
        None
> match "This is a string is super" with 
  | MatchGroups "(?<word1>T\w+).*(?<word2>s\w+)" groups -> (groups?word1, groups?word2)
  | _ -> failwith "nothing matched"

//val it : string * string = ("This", "super")

let inp = """
    <?xml version=\"1.0\" encoding=\"utf-8\" ?>
           <Scene>
                 <Composite>
                 <Circle radius='2' x='1' y='0'/>
                 <Composite>
                   <Circle radius='2' x='4' y='0'/>
                   <Square side='2' left='-3' top='0'/>
                 </Composite>
                 <Ellipse top='2' left='-2' width='3' height='4'/>
              </Composite>
           </Scene>"""

> open System.Xml;;
> let doc = new XmlDocument();;
//val doc : XmlDocument

> doc.LoadXml(inp);;
//val it : unit = ()

> doc.ChildNodes;;
//val it : XmlNodeList
//    = seq [seq []; seq [seq [seq []; seq [seq []; seq []]; seq []]]]

> fsi.AddPrinter(fun (x:XmlNode) -> x.OuterXml);;

> doc.ChildNodes;;

//val it : XmlNodeList
// = seq
//    [<?xml version="1.0" encoding="utf-8"?>;
//     <Scene><Composite><Circle radius="2" x="1" y="0" /><Composite>...</Scene>]

> doc.ChildNodes.Item(1);;

//val it : XmlNode
//  = <Scene><Composite><Circle radius="2" x="1" y="0" /><Composite>...</Scene>

> doc.ChildNodes.Item(1).ChildNodes.Item(0);;

//val it : XmlNode
//  = <Composite><Circle radius="2" x="1" y="0" /><Composite>...</Composite>

> doc.ChildNodes.Item(1).ChildNodes.Item(0).ChildNodes.Item(0);;
//val it : XmlNode = <Circle radius="2" x="1" y="0" />

> doc.ChildNodes.Item(1).ChildNodes.Item(0).ChildNodes.Item(0).Attributes;;
//val it : val it : XmlAttributeCollection = seq [radius="2"; x="1"; y="0"]

open System.Drawing
type Scene =
    | Ellipse   of RectangleF
    | Rect      of RectangleF
    
    | Composite of Scene list

open System.Xml
open System.Drawing
type Scene =
    | Ellipse of RectangleF
    | Rect    of RectangleF
    | Composite   of Scene list

    /// A derived constructor
    static member Circle(center:PointF,radius) =
        Ellipse(RectangleF(center.X-radius,center.Y-radius,
                           radius*2.0f,radius*2.0f))

    /// A derived constructor
    static member Square(left,top,side) =
        Rect(RectangleF(left,top,side,side))

/// Extract a number from an XML attribute collection
let extractFloat32 attrName (attribs: XmlAttributeCollection) =
    float32 (attribs.GetNamedItem(attrName).Value)

/// Extract a Point from an XML attribute collection
let extractPointF (attribs: XmlAttributeCollection) =
    PointF(extractFloat32 "x" attribs,extractFloat32 "y" attribs)

/// Extract a Rectangle from an XML attribute collection
let extractRectangleF (attribs: XmlAttributeCollection) =
    RectangleF(extractFloat32 "left" attribs,extractFloat32 "top" attribs,
               extractFloat32 "width" attribs,extractFloat32 "height" attribs)

/// Extract a Scene from an XML node
let rec extractScene (node: XmlNode) =
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
        Scene.Square(extractFloat32 "left" attribs,extractFloat32 "top" attribs,
                     extractFloat32 "side" attribs)

    | "Composite"   ->
        Scene.Composite [ for child in childNodes -> extractScene(child) ]
    | _ -> failwithf "unable to convert XML '%s'" node.OuterXml

/// Extract a list of Scenes from an XML document
let extractScenes (doc: XmlDocument) =
   [ for node in doc.ChildNodes do
       if node.Name = "Scene" then
          yield (Composite
                     [ for child in node.ChildNodes -> extractScene(child) ]) ]


The inferred types of these functions are as follows:
type Scene =
    | Ellipse of RectangleF
    | Rect of RectangleF
    | Composite of Scene list
    static member Circle : PointF * float32 -> Scene
    static member Square : float32 * float32 * float32 -> Scene

//val extractFloat32 : string -> XmlAttributeCollection -> float32
//val extractPointF : XmlAttributeCollection -> PointF
//val extractRectangleF :  XmlAttributeCollection -> RectangleF
//val extractScene : XmlNode -> Scene
//val extractScenes : XmlDocument -> Scene list

> fsi.AddPrinter(fun (r:RectangleF) ->
      sprintf "[%A,%A,%A,%A]" r.Left r.Top r.Width r.Height);;

> extractScenes doc;;

//val it : Scene list
//= [Composite
//    [Composite
//      [Ellipse [-1.0f,-2.0f,4.0f,4.0f];
//       Composite [Ellipse [2.0f,-2.0f,4.0f,4.0f]; Rect [-3.0f,0.0f,2.0f,2.0f]];
//       Ellipse [-2.0f,2.0f,3.0f,4.0f]]]]

type Term =
    | Term  of int * string * int
    | Const of int

type Polynomial = term list

[Term (1,"x",5); Term (-2,"x",3); Const 20]

type Token = 
    | ID of string 
    | INT of int
    | HAT
    | PLUS 
    | MINUS

let tokenR = regex @"((?<token>(\d+|\w+|\^|\+|-))\s*)*"

let tokenize (s:string) = 
    [ for x in tokenR.Match(s).Groups.["token"].Captures do 
         let token = 
             match x.Value with 
             | "^" -> HAT 
             | "-" -> MINUS
             | "+" -> PLUS
             | s when System.Char.IsDigit s.[0] -> INT (int s)
             | s -> ID s 
         yield token ]    TBD
//val tokenize : string -> Token list

> tokenize "x^5 - 2x^3 + 20";;

//val it : Token list = 
//    [ID "x"; HAT; INT 5; MINUS; INT 2; ID "x"; HAT; INT 3; PLUS; INT 20] 

type Term =
    | Term  of int * string * int
    | Const of int

type Polynomial = term list

[Term (1,"x",5); Term (-2,"x",3); Const 20]

[ID "x"; HAT; INT 5; MINUS; INT 2; ID "x"; HAT; INT 3; PLUS; INT 20]

type Term =
    | Term  of int * string * int
    | Const of int

type Polynomial = Term list
type TokenStream = Token list

let tryToken (src: TokenStream) =
    match src with
    | tok :: rest -> Some(tok, rest)
    | _ -> None

let parseIndex src =
    match tryToken src with
    | Some (HAT, src) ->
        match tryToken src with
        | Some (INT num2, src) ->
            num2, src
        | _ -> failwith "expected an integer after '^'"
    | _ -> 1, src

let parseTerm src =
    match tryToken src with
    | Some (INT num, src) ->
        match tryToken src with
        | Some (ID id, src) ->
           let idx, src = parseIndex src
           Term (num, id, idx), src
        | _ -> Const num, src
    | Some (ID id, src) ->
         let idx, src = parseIndex src
         Term(1, id, idx), src
    | _ -> failwith "end of token stream in term"

let rec parsePolynomial src =
    let t1, src = parseTerm src
    match tryToken src with
    | Some (PLUS, src) ->
        let p2, src = parsePolynomial src
        (t1 :: p2), src
    | _ -> [t1], src

let parse input =
    let src = tokenize input
    let result, src = parsePolynomial src
    match tryToken src with
    | Some _ -> failwith "unexpected input at end of token stream!"
    | None -> result
open SimpleTokensLex
open Microsoft.FSharp.Text.Lexing

type Term =
    | Term  of int * string * int
    | Const of int

type Polynomial = term list
type TokenStream = LazyList<token * Position * Position>

let tryToken (src: TokenStream ) =
    match src with
    | LazyList.Cons ((tok, startPos, endPos), rest) -> Some(tok, rest)
    | _ -> None

let parseIndex src =
    match tryToken src with
    | Some (HAT, src) ->
        match tryToken src with
        | Some (INT num2, src) ->
            num2, src
        | _ -> failwith "expected an integer after '^'"
    | _ -> 1, src

let parseTerm src =
    match tryToken src with
    | Some (INT num, src) ->
        match tryToken src with
        | Some (ID id, src) ->
           let idx, src = parseIndex src
           Term (num, id, idx), src
        | _ -> Const num, src
    | Some (ID id, src) ->
         let idx, src = parseIndex src
         Term(1, id, idx), src
    | _ -> failwith "end of token stream in term"

let rec parsePolynomial src =
    let t1, src = parseTerm src
    match tryToken src with
    | Some (PLUS, src) ->
        let p2, src = parsePolynomial src
        (t1 :: p2), src
    | _ -> [t1], src

//val tryToken        : TokenStream  -> (token * TokenStream ) option
//val parseIndex      : TokenStream  -> int * TokenStream 
//val parseTerm       : TokenStream  -> Term * TokenStream 
//val parsePolynomial : TokenStream  -> Polynomial * TokenStream 

let getTokenStream  inp : TokenStream  =
    // Generate the token stream as a seq<token>
    seq { let lexbuf = LexBuffer<_>.FromString inp
          while not lexbuf.IsPastEndOfStream do
              match SimpleTokensLex.token lexbuf with
              | EOF -> yield! []
              | token -> yield (token, lexbuf.StartPos, lexbuf.EndPos) }

    // Convert to a lazy list
    |> LazyList.ofSeq

let parse input =
    let src = getTokenStream  tokenize input
    let result, src = parsePolynomial src
    match tryToken src with
    | Some _ -> failwith "unexpected input at end of token stream!"
    | None -> result

//val getTokenStream : string -> TokenStream 
//val parse: string -> polynomialPolynomial

> parse "1+3";;
//val it : term list = [Const 1; Const 3]

> parse "2x^2+3x+5";;
//val it : term list = [Term (2,"x",2); Term (3,"x",1); Const 5]

type OoutSstate = System.IO.BinaryWriter
type IinSstate  = System.IO.BinaryReader

type Ppickler<'T> = 'T -> OoutSstate -> unit
type Uunpickler<'T> = IinSstate -> 'T

let byteP (b: byte) (st: OoutSstate) = st.Write(b)
let byteU (st: IinSstate) = st.ReadByte()

let boolP b st = byteP (if b then 1uy else 0uy) st
let boolU st = let b = byteU st in (b = 1uy)

let int32P i st =
    byteP (byte (i &&& 0xFF)) st
    byteP (byte ((i >>> 8) &&& 0xFF)) st
    byteP (byte ((i >>> 16) &&& 0xFF)) st
    byteP (byte ((i >>> 24) &&& 0xFF)) st

let int32U st =
    let b0 = int (byteU st)
    let b1 = int (byteU st)
    let b2 = int (byteU st)
    let b3 = int (byteU st)
    b0 ||| (b1 <<< 8) ||| (b2 <<< 16) ||| (b3 <<< 24)

//val byteP  : Ppickler<byte>
//val byteU  : Uunpickler<byte>
//val boolP  : Ppickler<bool>
//val boolU  : Uunpickler<bool>
//val int32P : Ppickler<int>
//val int32U : Uunpickler<int>

let tup2P p1 p2 (a, b) (st: OoutSstate) =
    (p1 a st : unit)
    (p2 b st : unit)

let tup3P p1 p2 p3 (a, b, c) (st: OoutSstate) =
    (p1 a st : unit)
    (p2 b st : unit)
    (p3 c st : unit)

let tup2U p1 p2 (st: IinSstate) =
    let a = p1 st
    let b = p2 st
    (a, b)

let tup3U p1 p2 p3 (st: IinSstate) =
    let a = p1 st
    let b = p2 st
    let c = p3 st
    (a, b, c)

// Outputs a list into the given output stream by pickling each element via f.
let rec listP f lst st =
    match lst with
    | [] ->     byteP 0uy st
    | h :: t -> byteP 1uy st; f h st; listP f t st

// Reads a list from a given input stream by unpickling each element via f.
let listU f st =
    let rec ulist_auxloop acc =
        let tag = byteU st
        match tag with
        | 0uy -> List.rev acc
        | 1uy -> let a = f st in ulist_auxloop (a::acc)
        | n ->   failwithf "listU: found number %d" n
    ulist_aux []

//val tup2P : 'a Ppickler<'a> -> 'b Ppickler<'b> -> Pickler< ('a * 'b>) pickler
//val tup3P : 'a Ppickler<'a> -> 'b Ppickler<'b> -> 'c Ppickler<'c> -> Pickler<('a * 'b * 'c>) pickler
//val tup2U : U'a unpickler<'a> -> U'b unpickler<'b> -> Unpickler<('a * 'b>) unpickler
//val tup3U : Unpickler<'a>  unpickler -> Unpickler<'b>  unpickler -> Unpickler<'c> unpickler -> Unpickler<('a* 'b* 'c)> unpickler
//val listP : Pickler<'a> pickler -> Pickler<'a list> pickler
//val listU : Unpickler<'a>  unpickler -> Unpickler<'a list> unpickler

type format = list<int32 * bool>

let formatP = listP (tup2P int32P boolP)
let formatU = listU (tup2U int32U boolU)

open System.IO

let writeData file data =
    use outStream = new BinaryWriter(File.OpenWrite(file))
    formatP data outStream

let readData file  =
    use inStream = new BinaryReader(File.OpenRead(file))
    formatU inStream

> writeData "out.bin" [(102, true); (108, false)] ;;
//val it : unit = ()

> readData "out.bin";;
//val it : (int * bool) list = [(102, true); (108, false)]
