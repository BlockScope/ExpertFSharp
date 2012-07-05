> let buf = new System.Text.StringBuilder();;
//val buf : System.Text.StringBuilder = 

> buf.Append("Humpty Dumpty");;
> buf.Append(" sat on the wall");;
> buf.ToString();;
//val it : string = "Humpty Dumpty sat on the wall"

//Table 3-6. String and Character Literals
"Humpty Dumpty"
"c:\\Program Files"
@"c:\Program Files"
"""I "like" you"""
"xyZy3d2"B
'c'	

> "MAGIC"B;;
//val it : byte [] = [|77uy; 65uy; 71uy; 73uy; 67uy|]

> let dir = @"c:\Program Files";;
//val dir : string = "c:\Program Files"

> let text = """I "like" you""";;
//val text : string = "I "like" you"

> let s = "All the kings horses
- and all the kings men";;

//val s : string = "All the kings horses
//- and all the kings men"

> let s2 = """All the kings' "horses"
- and all the kings men""";;

//val s2 : string = "All the kings' "horses"
//- and all the kings men"

"\n"
//val it : string = "
//"

"\r"
//val it : string = "
//"

"\t"
val it : string = "	"

"\b"
val it : string = ""

"a\b"

"\032"
val it : string = ""

"\u00a9" 
val it : string = " "

"\U00002260"
// unicode character output

> sprintf "Name: %s, Age: %d" "Anna" 3;;
//val it : string = "Name: Anna, Age: 3"

> sprintf "Name: %s, Age: %d" 3 10;;
//error FS0001: This expression was expected to have type
//    string    
//but here has type
//    int    

>sprintf "%b";;
//val it : (bool -> string) = <fun:it@72-6>

>sprintf "%b" true;;
//val it : string = "true"

>sprintf "%s";;
//val it : (string -> string) = <fun:it@78-7>

>sprintf "%s" "Hello";;
//val it : string = "Hello"

>sprintf "%d";;
//val it : (int -> string) = <fun:it@84-8>

>sprintf "%d" 42;;
//val it : string = "42"

>sprintf "%x";;
//val it : (int -> string) = <fun:it@90-9>

>sprintf "%x" 42;;
//val it : string = "2a"

>sprintf "%X";;
//val it : (int -> string) = <fun:it@96-10>

>sprintf "%X" 42;;
//val it : string = "2A"

>sprintf "%o";;
//val it : (int -> string) = <fun:it@102-11>

>sprintf "%o" 42;;
//val it : string = "52"

>sprintf "%u";;
//val it : (int -> string) = <fun:it@108-12>

>sprintf "%u" 42;;
//val it : string = "42"

>open System;;

>sprintf "%e";;
//val it : (float -> string) = <fun:it@116-13>

>sprintf "%e" Math.PI;;
//val it : string = "3.141593e+000"

>sprintf "%E";;
//val it : (float -> string) = <fun:it@122-14>

>sprintf "%E" Math.PI;;
//val it : string = "3.141593E+000"

>sprintf "%f";;
//val it : (float -> string) = <fun:it@128-15>

>sprintf "%f" Math.PI;;
//val it : string = "3.141593"

>sprintf "%g";;
//val it : (float -> string) = <fun:it@134-16>

>sprintf "%g" Math.PI;;
//val it : string = "3.14159"

sprintf "%M" (Decimal Math.PI);;
//val it : string = "3.14159265358979"

>let f : (DateTime -> _) = sprintf "%A";;
//val f : (DateTime -> string)

>(sprintf "%A" : DateTime -> _);;

>sprintf "%A" DateTime.Now;;
//val it : string = "5/07/2012 4:52:46 p.m."

>sprintf "%+A" DateTime.Now;;
//val it : string = "5/07/2012 4:54:17 p.m."

>let f : (DateTime -> string) = sprintf "%O";;
//val f : (DateTime -> string)

>sprintf "%O" DateTime.Now;;
val it : string = "5/07/2012 4:55:30 p.m."

>open System;;
>(sprintf "%a" : (_ -> DateTime -> string) -> _ -> _);;
sprintf "%a" (fun () -> (fun (d:DateTime) -> sprintf "%A" d.DayOfWeek)) DateTime.Now;;

>sprintf "%t" (fun () -> "Hello");;
//val it : string = "Hello"

>sprintf;;
//val it : (Printf.StringFormat<'a> -> 'a) = <fun:clo@167-1>

> System.DateTime.Now.ToString();;
//val it : string = "28/06/20.. 17:14:07 PM"

> sprintf "It is now %O" System.DateTime.Now;;
//val it : string = "It is now 28/06/20... 17:14:09"

> printf "The result is %A\n" [1; 2; 3];;
//The result is [1; 2; 3]
//val it : unit = ()

> System.String.Format("{0} {1} {2}", 12, "a", 1.23);;
//val it : string = "12 a 1.23"

> open System;;
> DateTime.Parse("13 July 1968");;

//val it : DateTime = 13/07/1968 12:00:00 a.m. {Date = 13/07/1968 12:00:00 a.m.;
//                                              Day = 13;
//                                              DayOfWeek = Saturday;
//                                              DayOfYear = 195;
//                                              Hour = 0;
//                                              Kind = Unspecified;
//                                              Millisecond = 0;
//                                              Minute = 0;
//                                              Month = 7;
//                                              Second = 0;
//                                              Ticks = 620892000000000000L;
//                                              TimeOfDay = 00:00:00;
//                                              Year = 1968;}

> let date x = DateTime.Parse(x);;
//val date : x:string -> DateTime

> let ciNZ = new System.Globalization.CultureInfo(name = "en-NZ");;
> let ciUS = new System.Globalization.CultureInfo(name = "en-US");;
> System.Threading.Thread.CurrentThread.CurrentUICulture <- ciNZ;;
> System.Threading.Thread.CurrentThread.CurrentUICulture <- ciUS;;
> printfn "date = %A" (date "13 July 1968");;
//date = 13/07/1968 12:00:00 a.m.
//val it : unit = ()

> printfn "birth = %A" (date "18 March 2003, 6:21:01pm");;
//birth = 18/03/2003 6:21:01 p.m.
//val it : unit = ()

> open System;;

> Uri.TryCreate("http://www.thebritishmuseum.ac.uk/", UriKind.Absolute);;
//val it : bool * Uri =
//  (true,
//   http://www.thebritishmuseum.ac.uk/
//     {AbsolutePath = "/";
//      AbsoluteUri = "http://www.thebritishmuseum.ac.uk/";
//      Authority = "www.thebritishmuseum.ac.uk";
//      DnsSafeHost = "www.thebritishmuseum.ac.uk";
//      Fragment = "";
//      Host = "www.thebritishmuseum.ac.uk";
//      HostNameType = Dns;
//      IsAbsoluteUri = true;
//      IsDefaultPort = true;
//      IsFile = false;
//      IsLoopback = false;
//      IsUnc = false;
//      LocalPath = "/";
//      OriginalString = "http://www.thebritishmuseum.ac.uk/";
//      PathAndQuery = "/";
//      Port = 80;
//      Query = "";
//      Scheme = "http";
//      Segments = [|"/"|];
//      UserEscaped = false;
//      UserInfo = "";})

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
//val splitLine : line:string -> string []

let parseEmployee (line: string) =
    match splitLine line with
    | [| last; first; startDate; title |] ->
        last, first, System.DateTime.Parse(startDate), title
    | _ ->
        failwithf "invalid employee format: '%s'" line
//val parseEmployee : line:string -> string * string * DateTime * string

> parseEmployee line;;
//val it : string * string * DateTime * string =
//  ("Smith", "John", 20/01/1986 12:00:00 a.m. {Date = 20/01/1986 12:00:00 a.m.;
//                                              Day = 20;
//                                              DayOfWeek = Monday;
//                                              DayOfYear = 20;
//                                              Hour = 0;
//                                              Kind = Unspecified;
//                                              Millisecond = 0;
//                                              Minute = 0;
//                                              Month = 1;
//                                              Second = 0;
//                                              Ticks = 626421600000000000L;
//                                              TimeOfDay = 00:00:00;
//                                              Year = 1986;},
//   "Software Developer")

open System.IO
File.WriteAllLines("employees.txt", Array.create 10000 line)
//val it : unit = ()

let readEmployees (fileName : string) = 
    fileName |> File.ReadLines |> Seq.map parseEmployee
//val readEmployees :
//  fileName:string -> seq<string * string * DateTime * string>

let firstThree = readEmployees "employees.txt" |> Seq.truncate 3 |> Seq.toList
//val firstThree : (string * string * DateTime * string) list =
//  [("Smith", "John", 20/01/1986 12:00:00 a.m., "Software Developer");
//   ("Smith", "John", 20/01/1986 12:00:00 a.m., "Software Developer");
//   ("Smith", "John", 20/01/1986 12:00:00 a.m., "Software Developer")]

> firstThree |> Seq.iter (fun (last,first,startDate,title)  -> 
      printfn "%s %s started on %A" first last startDate);;
//John Smith started on 20/01/1986 12:00:00 a.m.
//John Smith started on 20/01/1986 12:00:00 a.m.
//John Smith started on 20/01/1986 12:00:00 a.m.
//val it : unit = ()

open System.Text.RegularExpressions

let parseHttpRequest line =
    let result = Regex.Match(line, @"GET (.*?) HTTP/1\.([01])$")
    let file = result.Groups.[1].Value
    let version = result.Groups.[2].Value
    file, version
//val parseHttpRequest : line:string -> string * string

open System.Text.RegularExpressions
let regex s = new Regex(s)
let (=~) s (re:Regex) = re.IsMatch(s)
let (<>~) s (re:Regex) = not (s =~ re)
//val regex : s:string -> Regex
//val ( =~ ) : s:string -> re:Regex -> bool
//val ( <>~ ) : s:string -> re:Regex -> bool

> let samplestring = "This is a string";;
//val samplestring : string = "This is a string"

> if samplestring =~ regex "his" then
       printfn "A Match! ";;
//A Match! 
//val it : unit = ()

> "This is a string" =~ regex "(is )+";;
//val it : bool = true

> regex(" ").Split("This is a string");;
//val it : string [] = [|"This"; "is"; "a"; "string"|]

> regex(@"\s+").Split("I'm a little      teapot");;
//val it : string [] = [|"I'm"; "a"; "little"; "teapot"|]

> regex(@"\s+").Split("I'm a little  \t\t\n\t\n\t teapot");;
//val it : string [] = [|"I'm"; "a"; "little"; "teapot"|]

> let m = regex("joe").Match("maryjoewashere");;
//val m : Match = joe

> if m.Success then
     printfn "Matched at position %d" m.Index;;
//Matched at position 4
//val it : unit = ()

> let text = "was a dark and stormy night";;
//val text : string = "was a dark and stormy night"

> let t2 = regex(@"\w+").Replace(text, "WORD");;
//val t2 : string = "WORD WORD WORD WORD WORD WORD"

> samplestring =~ regex "(?i)HIS";;
//val it : bool = true

> samplestring =~ regex "HIS";;
//val it : bool = false

let entry = @"
Jolly Jethro
13 Kings Parade
Cambridge, Cambs CB2 1TJ
"
//val entry : string = "
//Jolly Jethro
//13 Kings Parade
//Cambridge, Cambs CB2 1TJ
//"

let re =
 regex @"(?<=\n)\s*(?<city>[^\n]+)\s*,\s*(?<county>\w+)\s+(?<pcode>.{3}\s*.{3}).*$"
//val re : Regex =
//  (?<=\n)\s*(?<city>[^\n]+)\s*,\s*(?<county>\w+)\s+(?<pcode>.{3}\s*.{3}).*$

> let r = re.Match(entry);;
//val r : Match = Cambridge, Cambs CB2 1TJ

> r.Groups.["city"].Value;;
//val it : string = "Cambridge"

> r.Groups.["county"].Value;;
//val it : string = "Cambs"

> r.Groups.["pcode"].Value;;
//val it : string = "CB2 1TJ"

let (|IsMatch|_|) (re: string) (inp:string) = 
    if Regex(re).IsMatch(inp)  then Some() else None
//val ( |IsMatch|_| ) : re:string -> inp:string -> unit option
    
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
//val firstAndSecondWord : inp:string -> (string * string) option

> firstAndSecondWord "This is a super string"
//val it : (string * string) option = Some ("This", "is")

let (?) (results:Match) (name:string) = 
    results.Groups.[name].Value
//val ( ? ) : results:Match -> name:string -> string

let firstAndSecondWord (inp:string) = 
    let re = regex "(?<word1>\w+)\s+(?<word2>\w+)"
    let results = re.Match(inp) 
    if results.Success then 
        Some (results?word1, results?word2)
    else 
        None
//val firstAndSecondWord : inp:string -> (string * string) option

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
//val inp : string =
//  "
//    <?xml version=\"1.0\" encoding=\"utf-8\" ?>
//           <"+[366 chars]

> open System.Xml;;
> let doc = new XmlDocument();;
//val doc : XmlDocument

> doc.LoadXml(inp);;
//System.Xml.XmlException: Unexpected XML declaration. The XML declaration must be the first node in the document, and no white space characters are allowed to appear before it. Line 2, position 7.
//   at System.Xml.XmlTextReaderImpl.Throw(Exception e)
//   at System.Xml.XmlTextReaderImpl.Throw(String res, String arg)
//   at System.Xml.XmlTextReaderImpl.ParsePI(StringBuilder piInDtdStringBuilder)
//   at System.Xml.XmlTextReaderImpl.ParseDocumentContent()
//   at System.Xml.XmlTextReaderImpl.Read()
//   at System.Xml.XmlLoader.LoadNode(Boolean skipOverWhitespace)
//   at System.Xml.XmlLoader.LoadDocSequence(XmlDocument parentDoc)
//   at System.Xml.XmlLoader.Load(XmlDocument doc, XmlReader reader, Boolean preserveWhitespace)
//   at System.Xml.XmlDocument.Load(XmlReader reader)
//   at System.Xml.XmlDocument.LoadXml(String xml)
//   at <StartupCode$FSI_0007>.$FSI_0007.main@()
//Stopped due to error

// Closed triple quotes, no line break on first line and no backslash escaping within triple quoted string.
let inp = """<?xml version="1.0" encoding="utf-8" ?>
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

> doc.LoadXml(inp);;
//val it : unit = ()

> doc.ChildNodes;;
//val it : XmlNodeList =
//  seq [seq []; seq [seq [seq []; seq [seq []; seq []]; seq []]]]

> fsi.AddPrinter(fun (x:XmlNode) -> x.OuterXml);;
//val it : unit = ()

> doc.ChildNodes;;
//val it : XmlNodeList =
//  seq
//    [<?xml version="1.0" encoding="utf-8"?>;
//     <Scene><Composite><Circle radius="2" x="1" y="0" /><Composite><Circle radius="2" x="4" y="0" /><Square side="2" left="-3" top="0" /></Composite><Ellipse top="2" left="-2" width="3" height="4" /></Composite></Scene>]

> doc.ChildNodes.Item(1);;
//val it : XmlNode =
//  <Scene><Composite><Circle radius="2" x="1" y="0" /><Composite><Circle radius="2" x="4" y="0" /><Square side="2" left="-3" top="0" /></Composite><Ellipse top="2" left="-2" width="3" height="4" /></Composite></Scene>

> doc.ChildNodes.Item(1).ChildNodes.Item(0);;
//val it : XmlNode =
//  <Composite><Circle radius="2" x="1" y="0" /><Composite><Circle radius="2" x="4" y="0" /><Square side="2" left="-3" top="0" /></Composite><Ellipse top="2" left="-2" width="3" height="4" /></Composite>

> doc.ChildNodes.Item(1).ChildNodes.Item(0).ChildNodes.Item(0);;
//val it : XmlNode = <Circle radius="2" x="1" y="0" />

> doc.ChildNodes.Item(1).ChildNodes.Item(0).ChildNodes.Item(0).Attributes;;
//val it : XmlAttributeCollection = seq [radius="2"; x="1"; y="0"]

open System.Drawing
type Scene =
    | Ellipse   of RectangleF
    | Rect      of RectangleF
    
    | Composite of Scene list
//type Scene =
//  | Ellipse of RectangleF
//  | Rect of RectangleF
//  | Composite of Scene list

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
//type Scene =
//  | Ellipse of RectangleF
//  | Rect of RectangleF
//  | Composite of Scene list
//  with
//    static member Circle : center:PointF * radius:float32 -> Scene
//    static member Square : left:float32 * top:float32 * side:float32 -> Scene
//  end
//val extractFloat32 :
//  attrName:string -> attribs:XmlAttributeCollection -> float32
//val extractPointF : attribs:XmlAttributeCollection -> PointF
//val extractRectangleF : attribs:XmlAttributeCollection -> RectangleF
//val extractScene : node:XmlNode -> Scene
//val extractScenes : doc:XmlDocument -> Scene list

> fsi.AddPrinter(fun (r:RectangleF) ->
      sprintf "[%A,%A,%A,%A]" r.Left r.Top r.Width r.Height);;
//val it : unit = ()

> extractScenes doc;;
//val it : Scene list =
//  [Composite
//     [Composite
//        [Ellipse [-1.0f,-2.0f,4.0f,4.0f];
//         Composite
//           [Ellipse [2.0f,-2.0f,4.0f,4.0f]; Rect [-3.0f,0.0f,2.0f,2.0f]];
//         Ellipse [-2.0f,2.0f,3.0f,4.0f]]]]

type Term =
    | Term  of int * string * int
    | Const of int
//type Term =
//  | Term of int * string * int
//  | Const of int

type Polynomial = Term list
//type Polynomial = Term list

[Term (1,"x",5); Term (-2,"x",3); Const 20]
//val it : Term list = [Term (1,"x",5); Term (-2,"x",3); Const 20]

type Token = 
    | ID of string 
    | INT of int
    | HAT
    | PLUS 
    | MINUS
//type Token =
//  | ID of string
//  | INT of int
//  | HAT
//  | PLUS
//  | MINUS

open System.Text.RegularExpressions
let regex s = new Regex(s)
//val regex : s:string -> Regex

let tokenR = regex @"((?<token>(\d+|\w+|\^|\+|-))\s*)*"
//val tokenR : Regex = ((?<token>(\d+|\w+|\^|\+|-))\s*)*

let tokenize (s:string) = 
    [ for x in tokenR.Match(s).Groups.["token"].Captures do 
         let token = 
             match x.Value with 
             | "^" -> HAT 
             | "-" -> MINUS
             | "+" -> PLUS
             | s when System.Char.IsDigit s.[0] -> INT (int s)
             | s -> ID s 
         yield token ]
//val tokenize : s:string -> Token list

> tokenize "x^5 - 2x^3 + 20";;
//val it : Token list =
//  [ID "x"; HAT; INT 5; MINUS; INT 2; ID "x"; HAT; INT 3; PLUS; INT 20]

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
//type Term =
//  | Term of int * string * int
//  | Const of int
//type Polynomial = Term list
//type TokenStream = Token list
//val tryToken : src:TokenStream -> (Token * Token list) option
//val parseIndex : src:TokenStream -> int * Token list
//val parseTerm : src:TokenStream -> Term * Token list
//val parsePolynomial : src:TokenStream -> Term list * Token list
//val parse : input:string -> Term list

let parse input =
    let src = tokenize input
    let result, src = parsePolynomial src
    match tryToken src with
    | Some _ -> failwith "unexpected input at end of token stream!"
    | None -> result
//val parse : input:string -> Term list

> parse "1+3";;
//val it : Term list = [Const 1; Const 3]

> parse "2x^2+3x+5";;
//val it : Term list = [Term (2,"x",2); Term (3,"x",1); Const 5]

type OutState = System.IO.BinaryWriter
type InState  = System.IO.BinaryReader

type Pickler<'T> = 'T -> OutState -> unit
type Unpickler<'T> = InState -> 'T

let byteP (b: byte) (st: OutState) = st.Write(b)
let byteU (st: InState) = st.ReadByte()

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

//type OutState = BinaryWriter
//type InState = BinaryReader
//type Pickler<'T> = 'T -> OutState -> unit
//type Unpickler<'T> = InState -> 'T
//val byteP : b:byte -> st:OutState -> unit
//val byteU : st:InState -> byte
//val boolP : b:bool -> st:OutState -> unit
//val boolU : st:InState -> bool
//val int32P : i:int -> st:OutState -> unit
//val int32U : st:InState -> int

let tup2P p1 p2 (a, b) (st: OutState) =
    (p1 a st : unit)
    (p2 b st : unit)

let tup3P p1 p2 p3 (a, b, c) (st: OutState) =
    (p1 a st : unit)
    (p2 b st : unit)
    (p3 c st : unit)

let tup2U p1 p2 (st: InState) =
    let a = p1 st
    let b = p2 st
    (a, b)

let tup3U p1 p2 p3 (st: InState) =
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
    let rec loop acc =
        let tag = byteU st
        match tag with
        | 0uy -> List.rev acc
        | 1uy -> let a = f st in loop (a::acc)
        | n ->   failwithf "listU: found number %d" n
    loop []

//val tup2P :
//  p1:('a -> OutState -> unit) ->
//    p2:('b -> OutState -> unit) -> a:'a * b:'b -> st:OutState -> unit
//val tup3P :
//  p1:('a -> OutState -> unit) ->
//    p2:('b -> OutState -> unit) ->
//      p3:('c -> OutState -> unit) -> a:'a * b:'b * c:'c -> st:OutState -> unit
//val tup2U : p1:(InState -> 'a) -> p2:(InState -> 'b) -> st:InState -> 'a * 'b
//val tup3U :
//  p1:(InState -> 'a) ->
//    p2:(InState -> 'b) -> p3:(InState -> 'c) -> st:InState -> 'a * 'b * 'c
//val listP :
//  f:('a -> 'b -> unit) -> lst:'a list -> st:'b -> unit when 'b :> OutState
//val listU : f:('a -> 'b) -> st:'a -> 'b list when 'a :> InState

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
//type format = (int32 * bool) list
//val formatP : ((int * bool) list -> System.IO.BinaryWriter -> unit)
//val formatU : (System.IO.BinaryReader -> (int * bool) list)
//val writeData : file:string -> data:(int * bool) list -> unit
//val readData : file:string -> (int * bool) list

> writeData "out.bin" [(102, true); (108, false)] ;;
//val it : unit = ()

> readData "out.bin";;
//val it : (int * bool) list = [(102, true); (108, false)]
