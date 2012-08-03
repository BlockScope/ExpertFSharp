type Var = string

type Prop =
    | And of Prop * Prop
    | Var of Var
    | Not of Prop
    | Exists of Var * Prop
    | False

let True = Not(False)
let Or(p,q)      = Not(And(Not(p),Not(q)))
let Iff(p,q)     = Or(And(p,q),And(Not(p),Not(q)))
let Implies(p,q) = Or(Not(p),q)
let Forall(v,p)  = Not(Exists(v,Not(p)))

let (&&&) p q = And(p,q)
let (|||) p q = Or(p,q)
let (~~~) p   = Not (p)
let (<=>) p q = Iff(p,q)
let (===) p q = (p <=> q)
let (==>) p q = Implies(p,q)
let (^^^) p q = Not (p <=> q)

let var (nm:Var) = Var(nm)

let fresh =
    let count = ref 0
    fun nm -> incr count; (sprintf "_%s%d" nm !count : Var)

let rec eval (env : Map<Var,bool>) inp =
    match inp with
    | Exists(v,p) -> eval (env.Add(v,false)) p || eval (env.Add(v,true)) p
    | And(p1,p2)  -> eval env p1 && eval env p2
    | Var(v)      -> if env.ContainsKey(v) then env.[v]
                     else failwithf "env didn't contain a value for %A" v
    | Not(p)      -> not (eval env p)
    | False       -> false

let rec support f =
    match f with
    | And(x,y)    -> Set.union (support x) (support y)
    | Exists(v,p) -> (support p).Remove(v)
    | Var(p)      -> Set.singleton p
    | Not(x)      -> support x
    | False       -> Set.empty

let rec cases supp =
    seq { match supp with
          | [] ->  yield Map.empty
          | v::rest ->
              yield! rest |> cases |> Seq.map (Map.add v false)
              yield! rest |> cases |> Seq.map (Map.add v true) }

let truthTable x =
    x |> support |> Set.toList |> cases |> Seq.map (fun env -> env,eval env x)

let satisfiable x =
    x |> truthTable |> Seq.exists(fun (env,res) -> res)

let tautology x =
    x |> truthTable |> Seq.forall (fun (env,res) -> res)

let tautologyWithCounterExample x =
    x |> truthTable |> Seq.tryFind (fun (env,res) -> not res) |> Option.map fst

let printCounterExample =
    (function None -> printfn "tautology verified OK"
            | Some env -> printfn "tautology failed on %A" (Seq.toList env))

//val eval : Map<Var,bool> -> Prop -> bool
//val support : Prop -> Set<Var>
//val cases : 'a list -> seq<Map<'a,bool>> when 'a : comparison
//val truthTable : Prop -> seq<Map<Var,bool> * bool>
//val satisfiable : Prop -> bool
//val tautology : Prop -> bool
//val tautologyWithCounterExample : Prop -> Map<Var,bool> option
//val printCounterExample : seq<'a> option -> unit

let stringOfBit b = (if b then "T" else "F")
let stringOfEnv env =
    Map.fold (fun acc k v -> sprintf "%s=%s;" k (stringOfBit v)+acc) "" env
let stringOfLine (env,res) = sprintf "%20s %s" (stringOfEnv env) (stringOfBit res)
let stringOfTruthTable tt =
    "\n" + (tt |> Seq.toList |> List.map stringOfLine |> String.concat "\n")

> fsi.AddPrinter(fun tt -> tt |> Seq.truncate 20 |> stringOfTruthTable);;

> truthTable (var "x");;
> val it : seq<Map<Var,bool> * bool>
=
                x=F; F
                x=T; T

> truthTable (var "x" &&&  var "y");;
> val it : seq<Map<Var,bool> * bool>
=
            x=F;y=F; F
            x=F;y=T; F
            x=T;y=F; F
            x=T;y=T; T

> truthTable (var "x" ||| ~~~(var "x"));;
> val it : seq<Map<Var,bool> * bool>
=
                x=F; T
                x=T; T

let sumBit x y = (x ^^^ y)
let carryBit x y = (x &&& y)
let halfAdder x y sum carry =
    (sum === sumBit x y)  &&&
    (carry === carryBit x y)

let fullAdder x y z sum carry =
    let xy = (sumBit x y)
    (sum === sumBit xy z) &&&
    (carry === (carryBit x y ||| carryBit xy z))

let twoBitAdder (x1,x2) (y1,y2) (sum1,sum2) carryInner carry =
    halfAdder x1 y1 sum1 carryInner &&&
    fullAdder x2 y2 carryInner sum2 carry

//val sumBit : Prop -> Prop -> Prop
//val carryBit : Prop -> Prop -> Prop
//val halfAdder : Prop -> Prop -> Prop -> Prop -> Prop
//val fullAdder : Prop -> Prop -> Prop -> Prop -> Prop -> Prop
//val twoBitAdder : Prop * Prop -> Prop * Prop -> Prop * Prop -> Prop -> Prop -> Prop

type bit = Prop
type bitvec = bit[]

let Lo : bit = False
let Hi : bit = True
let vec n nm : bitvec = Array.init n (fun i -> var (sprintf "%s%d" nm i))
let bitEq (b1:bit) (b2:bit) = (b1 <=> b2)
let AndL l = Seq.reduce (fun x y -> And(x,y)) l
let vecEq (v1:bitvec) (v2:bitvec) = AndL (Array.map2 bitEq v1 v2)

//type bit = Prop
//type bitvec = bit []
//val Lo : bit
//val Hi : bit
//val vec : int -> string -> bitvec
//val bitEq : bit -> bit -> Prop
//val AndL : #seq<Prop> -> Prop
//val vecEq : bitvec -> bitvec -> Prop

let fourBitAdder  (x:bitvec) (y:bitvec) (sum:bitvec) (carry:bitvec) =
    halfAdder  x.[0] y.[0]           sum.[0] carry.[0] &&&
    fullAdder  x.[1] y.[1] carry.[0] sum.[1] carry.[1] &&&
    fullAdder  x.[2] y.[2] carry.[1] sum.[2] carry.[2] &&&
    fullAdder  x.[3] y.[3] carry.[2] sum.[3] carry.[3]

let Blocks l = AndL l

let nBitCarryRippleAdder (n:int) (x:bitvec) (y:bitvec) (sum:bitvec) (carry:bitvec) =
    Blocks [ for i in 0 .. n-1 ->
                if i = 0
                then halfAdder x.[i] y.[i] sum.[i] carry.[i]
                else fullAdder x.[i] y.[i] carry.[i-1] sum.[i] carry.[i]  ]

let rippleAdder (n:int) (x:bitvec) (y:bitvec) (sum:bitvec) (carry:bitvec)  =
    Blocks [ for i in 0 .. n-1 ->
                fullAdder x.[i] y.[i] carry.[i] sum.[i] carry.[i+1] ]

> halfAdder (var "x") (var "y") (var "sum") (var "carry");;
> val it : Prop =
  And
    (Not
       (And
          (Not
             (And
                (Var "sum",
                 Not
                   (Not
                      (And
                         (Not (And (Var "x",Var "y")),
                          Not (And (Not (Var "x"),Not (Var "y")))))))),
           Not
             (And
                (Not (Var "sum"),
                 Not
                   (Not
                      (Not
                         (And
                            (Not (And (Var "x",Var "y")),
                             Not (And (Not (Var "x"),Not (Var "y"))))))))))),
     Not
       (And
          (Not (And (Var "carry",And (Var "x",Var "y"))),
           Not (And (Not (Var "carry"),Not (And (Var "x",Var "y")))))))

let twoBitAdderWithHiding (x1,x2) (y1,y2) (sum1,sum2) carry =
    let carryInnerVar = fresh "carry"
    let carryInner = var(carryInnerVar)
    Exists(carryInnerVar, halfAdder x1 y1 sum1 carryInner &&&
                          fullAdder x2 y2 carryInner sum2 carry)

> tautology (fullAdder Lo Lo Lo Lo Lo);;
//val it : bool = true

> satisfiable (fullAdder Lo Lo Lo Hi Lo);;
//val it : bool = false

> tautology (halfAdder (var "x") (var "x") Lo (var "x"));;
//val it : bool = true
> tautology
    (nBitCarryRippleAdder 2 (vec 2 "x") (vec 2 "y") (vec 2 "sum") (vec 3 "carry")
 === nBitCarryRippleAdder 2 (vec 2 "y") (vec 2 "x") (vec 2 "sum") (vec 3 "carry"));;
//val it : bool = true

open System.Collections.Generic

let memoize f =
    let tab = new Dictionary<_,_>()
    fun x -> if tab.ContainsKey(x) then tab.[x]
             else let res = f x in tab.[x] <- res; res

type BddIndex = int
type Bdd = Bdd of BddIndex
type BddNode = Node of Var * BddIndex * BddIndex
type BddBuilder(order : Var -> Var -> int) =

    // The core data structures that preserve uniqueness
    let uniqueTab = new Dictionary<BddNode,BddIndex>()
    let nodeTab   = new Dictionary<BddIndex,BddNode>()

    // Keep track of the next index
    let mutable nextIdx = 2
    let trueIdx = 1
    let falseIdx = -1
    let trueNode = Node("",trueIdx,trueIdx)
    let falseNode = Node("",falseIdx,falseIdx)

    // Map indexes to nodes. Negative indexes go to their negation. The special
    // indexes -1 and 1 go to special true/false nodes.
    let idxToNode(idx) =
        if idx = trueIdx then trueNode
        elif idx = falseIdx then falseNode
        elif idx > 0 then nodeTab.[idx]
        else let (Node(v,l,r)) = nodeTab.[-idx]
             Node(v,-l,-r)

    // Map nodes to indexes. Add an entry to the table if needed.
    let nodeToUniqueIdx(node) =
        if uniqueTab.ContainsKey(node) then uniqueTab.[node]
        else
            let idx = nextIdx
            uniqueTab.[node] <- idx
            nodeTab.[idx] <- node
            nextIdx <- nextIdx + 1
            idx

    // Get the canonical index for a node. Preserve the invariant that the
    // left-hand node of a conditional is always a positive node
    let mkNode(v:Var,l:BddIndex,r:BddIndex) =
        if l = r then l
        elif l >= 0 then nodeToUniqueIdx(Node(v,l,r) )
        else -nodeToUniqueIdx(Node(v,-l,-r))

    // Construct the BDD for a conjunction "m1 AND m2"
    let rec mkAnd(m1,m2) =
        if m1 = falseIdx || m2 = falseIdx then falseIdx
        elif m1 = trueIdx then m2 elif m2 = trueIdx then m1
        else
            let (Node(x,l1,r1)) = idxToNode(m1)
            let (Node(y,l2,r2)) = idxToNode(m2)
            let v,(la,lb),(ra,rb) =
                match order x y with
                | c when c = 0 -> x,(l1,l2),(r1,r2)
                | c when c < 0 -> x,(l1,m2),(r1,m2)
                | c            -> y,(m1,l2),(m1,r2)
            mkNode(v,mkAnd(la,lb), mkAnd(ra,rb))

    // Memoize this function
    let mkAnd = memoize mkAnd


    // Publish the construction functions that make BDDs from existing BDDs
    member g.False = Bdd falseIdx
    member g.And(Bdd m1,Bdd m2) = Bdd(mkAnd(m1,m2))
    member g.Not(Bdd m) = Bdd(-m)
    member g.Var(nm) = Bdd(mkNode(nm,trueIdx,falseIdx))
    member g.NodeCount = nextIdx

//val memoize : ('a -> 'b) -> ('a -> 'b) when 'a : equality
//type BddIndex = int
//type Bdd = Bdd of BddIndex
//type BddNode = Node of Var * BddIndex * BddIndex
//type BddBuilder =
//    new : order:(Var -> Var -> int) -> BddBuilder
//    member And : _arg1:Bdd * _arg2:Bdd -> Bdd
//    member Not : _arg3:Bdd -> Bdd
//    member Var : nm:Var -> Bdd
//    member False : Bdd
//    member NodeCount : int

type BddBuilder(order : Var -> Var -> int) =
    ...
    member g.ToString(Bdd idx) =
        let rec fmt depth idx =
            if depth > 3 then "..." else
            let (Node(p,l,r)) = idxToNode(idx)
            if p = "" then if l = trueIdx then "T" else "F"
            else sprintf "(%s => %s | %s)" p (fmt (depth+1) l) (fmt (depth+1) r)
        fmt 1 idx

    member g.Build(f) =
        match f with
        | And(x,y) -> g.And(g.Build x, g.Build y)
        | Var(p) -> g.Var(p)
        | Not(x) -> g.Not(g.Build x)
        | False -> g.False
        | Exists(v,p) -> failwith "Exists node"

    member g.Equiv p1 p2 = (g.Build(p1) = g.Build(p2))

> let bddBuilder = BddBuilder(compare);;
//val bddBuilder: BddBuilder

> fsi.AddPrinter(fun bdd -> bddBuilder.ToString(bdd));;
//val it: unit = ()

> bddBuilder.Build(var "x");;
//val it : Bdd = (x => T | F)

> bddBuilder.Build(var "x" &&& var "x");;
//val it : Bdd = (x => T | F)

> bddBuilder.Build(var "x") = bddBuilder.Build(var "x" &&& var "x");;
//val it : bool = true

> (var "x") = (var "x" &&& var "x");;
//val it : bool = false

> bddBuilder.Build(var "x" &&& var "y");;
//val it : Bdd = (x => (y => T | F) | F)

> bddBuilder.Equiv (var "x") (var "x" &&& var "x");;
//val it : bool = true

> bddBuilder.Equiv
    (nBitCarryRippleAdder 8 (vec 8 "x") (vec 8 "y") (vec 8 "sum") (vec 9 "carry"))
    (nBitCarryRippleAdder 8 (vec 8 "y") (vec 8 "x") (vec 8 "sum") (vec 9 "carry"));;
//val it : bool = true

let mux a b c = ((~~~a ==> b) &&& (a ==> c))

let carrySelectAdder
       totalSize maxBlockSize
       (x:bitvec) (y:bitvec)
       (sumLo:bitvec) (sumHi:bitvec)
       (carryLo:bitvec) (carryHi:bitvec)
       (sum:bitvec) (carry:bitvec) =
  Blocks
    [ for i in 0..maxBlockSize..totalSize-1 ->
        let sz = min (totalSize-i) maxBlockSize
        let j = i+sz-1
        let carryLo = Array.append [| False |] carryLo.[i+1..j+1]
        let adderLo = rippleAdder sz x.[i..j] y.[i..j] sumLo.[i..j] carryLo
        let carryHi = Array.append [| True  |] carryHi.[i+1..j+1]
        let adderHi = rippleAdder sz x.[i..j] y.[i..j]  sumHi.[i..j] carryHi
        let carrySelect = (carry.[j+1] === mux carry.[i] carryLo.[sz] carryHi.[sz])
        let sumSelect =
            Blocks [for k in i..j ->
                         sum.[k] === mux carry.[i] sumLo.[k] sumHi.[k]]
        adderLo &&& adderHi &&& carrySelect &&& sumSelect ]

let checkAdders n k =
    let x = vec n "x"
    let y = vec n "y"
    let sumA    = vec n "sumA"
    let sumB    = vec n "sumB"
    let sumLo   = vec n "sumLo"
    let sumHi   = vec n "sumHi"
    let carryA  = vec (n+1) "carryA"
    let carryB  = vec (n+1) "carryB"
    let carryLo = vec (n+1) "carryLo"
    let carryHi = vec (n+1) "carryHi"
    let adder1 = carrySelectAdder n k x y sumLo sumHi carryLo  carryHi  sumA carryA
    let adder2 = rippleAdder n x y sumB carryB
    (adder1 &&& adder2 &&& (carryA.[0] === carryB.[0]) ==>
         (vecEq sumA sumB &&& bitEq carryA.[n] carryB.[n]))

> bddBuilder.Equiv (checkAdders 5 2) True;;
//val it : bool = true

let approxCompareOn f x y =
    let c = compare (f x) (f y)
    if c <> 0 then c else compare x y
let bddBuilder2 = BddBuilder(approxCompareOn hash)

> bddBuilder2.Equiv (checkAdders 7 2) True;;
//val it : bool = true

open System
type Expr =
    | Var
    | Num of int
    | Sum of Expr * Expr
    | Prod of Expr * Expr

let rec deriv expr =
    match expr with
    | Var           -> Num 1
    | Num _         -> Num 0
    | Sum (e1, e2)  -> Sum (deriv e1, deriv e2)
    | Prod (e1, e2) -> Sum (Prod (e1, deriv e2), Prod (e2, deriv e1))
//val deriv : Expr -> Expr

> let e1 = Sum (Num 1, Prod (Num 2, Var));;
//val e1 : Expr = Sum (Num 1,Prod (Num 2,Var))

> deriv e1;;
//val it : Expr = Sum (Num 0,Sum (Prod (Num 2,Num 1),Prod (Var,Num 0)))

let precSum = 10
let precProd = 20

let rec stringOfExpr prec expr =
    match expr with
    | Var   -> "x"
    | Num i -> i.ToString()
    | Sum (e1, e2) ->
        if prec > precSum
        then "(" + stringOfExpr precSum e1 + "+" + stringOfExpr precSum e2 + ")"
        else       stringOfExpr precSum e1 + "+" + stringOfExpr precSum e2
    | Prod (e1, e2) ->
        stringOfExpr precProd e1 + "*" + stringOfExpr precProd e2

> fsi.AddPrinter (fun expr -> stringOfExpr 0 expr);;
//val it : unit = ()

> let e3 = Prod (Var, Prod (Var, Num 2));;
//val e3 : Expr = x*x*2

> deriv e3;;
//val it : Expr = x*(x*0+2*1)+x*2*1

let simpSum = function
    | Num n, Num m -> Num (n+m)      // constants!
    | Num 0, e | e, Num 0 -> e       // 0+e = e+0 = e
    | e1, e2 -> Sum(e1,e2)

let simpProd = function
    | Num n, Num m -> Num (n*m)      // constants!
    | Num 0, e | e, Num 0 -> Num 0   // 0*e=0
    | Num 1, e | e, Num 1 -> e       // 1*e = e*1 = e
    | e1, e2 -> Prod(e1,e2)

let rec simpDeriv = function
    | Var           -> Num 1
    | Num _         -> Num 0
    | Sum (e1, e2)  -> simpSum (simpDeriv e1, simpDeriv e2)
    | Prod (e1, e2) -> simpSum (simpProd (e1, simpDeriv e2),
                                simpProd (e2, simpDeriv e1))

> simpDeriv e3;;
//val it : Expr = x*2+x*2

namespace Symbolic.Expressions

type Expr =
    | Num  of decimal
    | Var  of string
    | Neg  of Expr
    | Add  of Expr list
    | Sub  of Expr * Expr list
    | Prod of Expr * Expr
    | Frac of Expr * Expr
    | Pow  of Expr * decimal
    | Sin  of Expr
    | Cos  of Expr
    | Exp  of Expr

    static member StarNeeded e1 e2 =
        match e1, e2 with
        | Num _, Neg _ | _, Num _ -> true
        | _ -> false

    member self.IsNumber =
        match self with
        | Num _ -> true | _ -> false

    member self.NumOf =
        match self with
        | Num num -> num | _ -> failwith "NumOf: Not a Num"

    member self.IsNegative =
        match self with
        | Num num | Prod (Num num, _) -> num < 0M
        | Neg e -> true | _ -> false

    member self.Negate =
        match self with
        | Num num -> Num (-num)
        | Neg e -> e
        | exp -> Neg exp

C:\samples> fsyacc ExprParser.fsy --module ExprParser
C:\samples> fslex ExprLexer.fsl --unicode

module ExprLexer

open System
open Microsoft.FSharp.Text.Lexing
open ExprParser

let lexeme = LexBuffer<_>.LexemeString

let special lexbuf = function
    | "+" -> PLUS    | "-" -> MINUS
    | "*" -> TIMES   | "/" -> DIV
    | "(" -> LPAREN  | ")" -> RPAREN  | "^" -> HAT
    | _   -> failwith "Invalid operator"

let id lexbuf = function
    | "sin" -> SIN   | "cos" -> COS
    | "e"   -> E     | id    -> ID id
}

let digit     = ['0'-'9']
let int       = digit+
let float     = int ('.' int)? (['e' 'E'] int)?
let alpha     = ['a'-'z' 'A'-'Z']
let id        = alpha+ (alpha | digit | ['_' '$'])*
let ws        = ' ' | '\t'
let nl        = '\n' | '\r' '\n'
let special   = '+' | '-' | '*' | '/' | '(' | ')' | '^'

rule main = parse
    | int        { INT     (Convert.ToInt32(lexeme lexbuf)) }
    | float      { FLOAT   (Convert.ToDouble(lexeme lexbuf)) }
    | id         { id      lexbuf (lexeme lexbuf) }
    | special    { special lexbuf (lexeme lexbuf) }
    | ws | nl    { main    lexbuf }
    | eof        { EOF }
    | _          { failwith (lexeme lexbuf) }

%{
open System
open Symbolic.Expressions
%}

%token <int> INT
%token <float> FLOAT
%token <string> ID

%token EOF LPAREN RPAREN PLUS MINUS TIMES DIV HAT SIN COS E

%left ID
%left prec_negate
%left LPAREN
%left PLUS MINUS
%left TIMES DIV
%left HAT


%start expr
%type <Expr> expr
%%

expr:
    | exp EOF { $1 }

number:
    | INT                           { BigNum.of_int $1 }
    | FLOAT                         { BigNum.of_string (Float.to_string $1) }
    | MINUS INT %prec prec_negate   { BigNum.of_int (-$2) }
    | MINUS FLOAT %prec prec_negate { BigNum.of_string (Float.to_string (-$2)) }

exp:
    | number                  { Num $1 }
    | ID                      { Var $1 }
    | exp PLUS exp            { Add [$1; $3] }
    | exp MINUS exp           { Sub ($1, [$3]) }
    | exp TIMES exp           { Prod ($1, $3) }
    | exp DIV exp             { Frac ($1, $3) }
    | SIN LPAREN exp RPAREN   { Sin $3 }
    | COS LPAREN exp RPAREN   { Cos $3 }
    | E HAT exp               { Exp $3 }
    | term                    { $1 }
    | exp HAT number          { Pow ($1, $3) }
    | LPAREN exp RPAREN       { $2 }
    | MINUS LPAREN exp RPAREN { Neg $3 }

term:
    | number ID               { Prod (Num $1, Var $2) }
    | number ID HAT number    { Prod (Num $1, Pow (Var $2, $4)) }
    | ID HAT number           { Prod (Num 1M, Pow (Var $1, $3)) }

module Symbolic.Expressions.Utils

open Symbolic.Expressions

/// A helper function to map/select across a list while threading state
/// through the computation
let selectFold f l s =
    let l,s' = List.fold
                  (fun (l',s') x ->
                       let x',s'' = f x s'
                       (List.rev x') @ l',s'')
                  ([],s) l
    List.rev l,s'

/// Collect constants
let rec collect = function
    | Prod (e1, e2) ->
        match collect e1, collect e2 with
        | Num num1, Num num2       -> Num (num1 * num2)
        | Num n1, Prod (Num n2, e)
        | Prod (Num n2, e), Num n1 -> Prod (Num (n1 * n2), e)
        | Num n, e | e, Num n      -> Prod (Num n, e)
        | Prod (Num n1, e1), Prod (Num n2, e2) ->
            Prod (Num (n1 * n2), Prod (e1, e2))
        | e1', e2'                 -> Prod (e1', e2')
    | Num _ | Var _ as e   -> e
    | Neg e                -> Neg (collect e)
    | Add exprs            -> Add (List.map collect exprs)
    | Sub (e1, exprs)      -> Sub (collect e1, List.map collect exprs)
    | Frac (e1, e2)        -> Frac (collect e1, collect e2)
    | Pow (e1, num)        -> Pow (collect e1, num)
    | Sin e                -> Sin (collect e)
    | Cos e                -> Cos (collect e)
    | Exp _ as e           -> e

/// Push negations through an expression
let rec negate = function
    | Num num           -> Num (-num)
    | Var v as exp      -> Neg exp
    | Neg e             -> e
    | Add exprs         -> Add (List.map negate exprs)
    | Sub _             -> failwith "unexpected Sub"
    | Prod (e1, e2)     -> Prod (negate e1, e2)
    | Frac (e1, e2)     -> Frac (negate e1, e2)
    | exp               -> Neg exp

/// Simplify an expression
let rec simp = function
    | Num num           -> Num num
    | Var v             -> Var v
    | Neg e             -> negate (simp e)
    | Add exprs ->
        let filterNums (e:Expr) n =
           if e.IsNumber
           then [], n + e.NumOf
           else [e], n
        let summands = function | Add es -> es | e -> [e]
        let exprs', num =
            selectFold (simp >> summands >> selectFold filterNums) exprs 0N
        match exprs' with
        | [Num _ as n] when num = 0M -> n
        | []                         -> Num num
        | [e] when num = 0M          -> e
        | _ when num = 0M            -> Add exprs'
        | _                          -> Add (exprs' @ [Num num])
    | Sub (e1, exprs) ->
         simp (Add (e1 :: List.map Neg exprs))
    | Prod (e1, e2) ->
        match simp e1, simp e2 with
        | Num num, _ | _, Num num when num = 0M -> Num 0M
        | Num num, e | e, Num num when num = 1M -> e
        | Num num1, Num num2                    -> Num (num1 * num2)
        | e1, e2                                -> Prod (e1, e2)
    | Frac (e1, e2) ->
        match simp e1, simp e2 with
        | Num num, _ when num = 0M  -> Num num
        | e1, Num num when num = 1M -> e1
        | Num (_  as num), Frac (Num (_ as num2), e) ->
             Prod (Frac (Num num, Num num2), e)
        | Num (_  as num), Frac (e, Num (_ as num2)) ->
             Frac (Prod (Num num, Num num2), e)
        | e1, e2                    -> Frac (e1, e2)
    | Pow (e, n) when n=1M -> simp e
    | Pow (e, n)           -> Pow (simp e, n)
    | Sin e                -> Sin (simp e)
    | Cos e                -> Cos (simp e)
    | Exp e                -> Exp (simp e)

let Simplify e = e |> simp |> simp |> collect


let Differentiate v e =
    let rec diff v = function
        | Num num               -> Num 0M
        | Var v' when v'=v      -> Num 1M
        | Var v'                -> Num 0M
        | Neg e                 -> diff v (Prod ((Num -1M), e))
        | Add exprs             -> Add (List.map (diff v) exprs)
        | Sub (e1, exprs)       -> Sub (diff v e1, List.map (diff v) exprs)
        | Prod (e1, e2)         -> Add [Prod (diff v e1, e2); Prod (e1, diff v e2)]
        | Frac (e1, e2) ->
            Frac (Sub (Prod (diff v e1, e2), [Prod (e1, diff v e2)]), Pow (e2, 2N))
        | Pow (e1, num) ->
            Prod (Prod(Num num, Pow (e1, num - 1M)), diff v e1)
        | Sin e                 -> Prod (Cos e, diff v e)
        | Cos e                 -> Neg (Prod (Sin e, diff v e))
        | Exp (Var v') as e when v'=v  -> e
        | Exp (Var v') as e when v'<>v -> Num 0M
        | Exp e                 -> Prod (Exp e, diff v e) 
    diff v e

namespace Symbolic.Expressions.Visual

open Symbolic.Expressions
open System.Drawing
open System.Drawing.Imaging

type RenderOptions =
    { NormalFont: Font;  SmallFont: Font;  IsSuper: bool;  Pen: Pen; }

    static member Default =
        { NormalFont = new Font("Courier New",18.0f,FontStyle.Regular);
          SmallFont = new Font("Courier New", 12.0f, FontStyle.Regular);
          IsSuper = false;
          Pen = new Pen(Color.Black, 1.0f); }

    member self.Brush =
        (new SolidBrush(Color.FromArgb(255, self.Pen.Color)) :> Brush)


type VisualElement =
    | Symbol   of string * ExprSize
    | Power    of VisualElement * VisualElement * ExprSize
    | Sequence of VisualElement list * ExprSize
    | Fraction of VisualElement * VisualElement * ExprSize
    member self.Size =
        match self with
        | Symbol (_, size)   | Power (_, _, size)
        | Sequence (_, size) | Fraction (_, _, size) -> size

    member self.Height = self.Size.height
    member self.Width = self.Size.width
    member self.Midline = self.Size.midline

and ExprSize =
    {  width: int;  height: int;  midline: int; }

    member self.CenterOnMidline size x y =
        x + (size.width-self.width)/2, y + (size.midline-self.midline)

    member self.Frac size opt =
        { width = max self.width size.width;
          height = self.height + size.height + self.FracSepHeight opt;
          midline = self.height + (self.FracSepHeight opt)/2; }

    member self.FracSepHeight (opt: RenderOptions) =
        max (int (opt.Pen.Width*5.0f)) 4

    member self.AddPower (e: VisualElement) =
        {  width = self.width + e.Width;
           height = self.height + e.Height;
           midline = self.midline + e.Height; }

    static member ExpandOne (size: ExprSize) (e: VisualElement) =
        { width   = size.width + e.Width;
          height  = max size.height e.Height;
          midline = max size.midline e.Midline; }

    member self.Expand (exprs: VisualElement list) =
        List.fold ExprSize.ExpandOne self exprs

    static member Seq (exprs: VisualElement list) =
        List.fold ExprSize.ExpandOne ExprSize.Zero exprs

    static member Zero =
        { width=0; height=0; midline=0; }


type VisualExpr =
    {  Expression : VisualElement;  RenderOptions: RenderOptions; }

    static member OfExpr (opt: RenderOptions) e =
        use bmp = new Bitmap(100, 100, PixelFormat.Format32bppArgb)
        use gra = Graphics.FromImage(bmp)
        let sizeOf (opt: RenderOptions) s =
            use sFormat = new StringFormat(StringFormat.GenericTypographic)
            let font = if opt.IsSuper then opt.SmallFont else opt.NormalFont
            let size = gra.MeasureString(s, font, PointF(0.0f, 0.0f), sFormat)
            let height = int size.Height
            { width = int size.Width + 2;
              height = height;
              midline = height/2; }
        let precPow = 70
        let precProd1, precProd2 = 30, 40
        let precAdd1, precAdd2 = 10, 11
        let precStart = 5
        let precNeg1, precNeg2 = 1, 20
        let sym opt s = Symbol (s, sizeOf opt s)

        let applyPrec opt pprec prec exprs (size: ExprSize) =
            if pprec > prec then
                sym opt "(" :: exprs @ [sym opt ")"],
                size.Expand [sym opt "("; sym opt ")"]
            else
                exprs, size

        let mkSequence opt pprec prec exprs =
            let size = ExprSize.Seq exprs
            let exprs, size = applyPrec opt pprec prec exprs size
            Sequence (exprs, size)

        let rec expFunc opt f par =
            let f' = sym opt f
            let exprs' = [sym opt "("; exp opt precStart par; sym opt ")"]
            Sequence (f' :: exprs', f'.Size.Expand exprs')

        and exp (opt: RenderOptions) prec = function
            | Num n ->
                let s = n.ToString() in Symbol (s, sizeOf opt s)
            | Var v ->
                Symbol (v, sizeOf opt v)
            | Neg e ->
                 let e' = exp opt precNeg1 e
                 let exprs, size = applyPrec opt prec precNeg1 [e'] e'.Size
                 let exprs' = [sym opt "-"] @ exprs
                 mkSequence opt prec precNeg2 exprs'
            | Add exprs ->
                 let exprs' =
                     [ for i,e in Seq.mapi (fun i x -> (i,x)) exprs do
                           let first = (i=0)
                           let e' = exp opt (if first then precAdd1 else precAdd2) e
                           if first || e.IsNegative
                           then yield! [e']
                           else yield! [sym opt "+"; e'] ]
                 mkSequence opt prec precAdd1 exprs'
            | Sub (e1, exprs) ->
                 let e1' = exp opt prec e1
                 let exprs' =
                     [ for e in exprs do
                           if e.IsNegative then
                              let e' = exp opt precAdd2 e.Negate
                              yield! [sym opt "+"; e']
                           else
                              let e' = exp opt precAdd2 e
                              yield! [sym opt "-"; e'] ]

                 mkSequence opt prec precAdd1 (e1'::exprs')
            | Prod (e1, e2) ->
                 let e1' = exp opt precProd1 e1
                 let e2' = exp opt precProd2 e2
                 let exprs' =
                     if Expr.StarNeeded e1 e2
                     then [e1'; sym opt "*"; e2']
                     else [e1'; e2']
                 mkSequence opt prec precProd1 exprs'
            | Pow (e1, e2) ->
                 let e1' = exp opt precPow e1
                 let e2' = exp { opt with IsSuper=true } precPow (Num e2)
                 Power (e1', e2', e1'.Size.AddPower e2')
            | Sin e ->
                 expFunc opt "sin" e
            | Cos e ->
                 expFunc opt "cos" e
            | Exp expo ->
                 let e' = sym opt "e"
                 let expo' = exp { opt with IsSuper=true } precPow expo
                 Power (e', expo', e'.Size.AddPower expo')

            | Frac (e1, e2) ->
                 let e1' = exp opt precStart e1
                 let e2' = exp opt precStart e2
                 Fraction (e1', e2', e1'.Size.Frac e2'.Size opt)
        let exp = exp opt precStart e
        { Expression=exp; RenderOptions=opt; }

| Prod (e1, e2) ->
     let e1' = exp opt precProd1 e1
     let e2' = exp opt precProd2 e2
     let exprs' =
         if Expr.StarNeeded e1 e2
         then [e1'; sym opt "*"; e2']
         else [e1'; e2']
     mkSequence opt prec precProd1 exprs'



type VisualExpr =
    ...
    member self.Render =
        let pt x y = PointF(float32 x, float32 y)
        let rec draw (gra: Graphics) opt x y psize = function
            | Symbol (s, size) ->
                let font = if opt.IsSuper then opt.SmallFont else opt.NormalFont
                let x', y' = size.CenterOnMidline psize x y
                gra.DrawString(s, font, opt.Brush, pt x' y')
            | Power (e1, e2, size) ->
                let x', y' = size.CenterOnMidline psize x y
                draw gra opt x' (y'+e2.Height) e1.Size e1
                draw gra { opt with IsSuper=true } (x'+e1.Width) y' e2.Size e2
            | Sequence (exps, size) ->
                let x', y' = size.CenterOnMidline psize x y
                List.fold (fun (x, y) (e: VisualElement) ->
                     let psize' = { width = e.Width; height = psize.height;
                                    midline=size.midline; }
                     draw gra opt x y psize' e
                     x+e.Width, y) (x', y') exps |> ignore
            | Fraction (e1, e2, size) as e ->
                let psize1 = { psize with height=e1.Height; midline=e1.Midline }
                let psize2 = { psize with height=e2.Height; midline=e2.Midline }
                draw gra opt x y psize1 e1
                gra.DrawLine(self.RenderOptions.Pen, x, y+size.midline,
                             x+psize.width, y+size.midline);
                draw gra opt x (y+e1.Height+size.FracSepHeight opt) psize2 e2
        let bmp = new Bitmap(self.Expression.Width, self.Expression.Height,
                             PixelFormat.Format32bppArgb)
        let gra = Graphics.FromImage(bmp)
        gra.FillRectangle(new SolidBrush(Color.White), 0, 0,
                          self.Expression.Width+1, self.Expression.Height+1)
        draw gra self.RenderOptions 0 0 self.Expression.Size self.Expression
        bmp

module Symbolic.Expressions.UI

open Symbolic.Expressions
open Symbolic.Expressions.Visual
open System.Windows.Forms
open System.Drawing

let CreateScrollableChildWindow parent =
    let scroll = new ScrollableControl(Dock=DockStyle.Fill, AutoScroll=true)
    let form2 = new Form(MdiParent=parent, BackColor=Color.White)
    form2.Controls.Add scroll
    form2, scroll

let NewExpression parent s es =
    let form, scroll = CreateScrollableChildWindow parent
    let AddLabel (top, maxw) (parent: Control) s =
        let l = new Label(Text=s, AutoSize=true, Top=top)
        parent.Controls.Add l
        (top+l.Height), max maxw l.Width
    let AddPic (top, maxw) (parent: Control) (e: Expr) =
        let e' = VisualExpr.OfExpr RenderOptions.Default e
        let bmp = e'.Render
        let pic = new PictureBox(Image=bmp, Height=bmp.Height,
                                 Width=bmp.Width, Top=top)
        parent.Controls.Add pic
        (top+bmp.Height), max maxw bmp.Width
    let height, width = List.fold (fun top (lab, e) ->
        AddPic (AddLabel top scroll lab) scroll e) (0, 0) es
    form.Text <- s
    form.Height <- min 640 (height+40)
    form.Width <- min 480 (width+40)
    form.Show()

let UpdatePreview (scroll: Control) e =
    let e' = VisualExpr.OfExpr RenderOptions.Default e
    let bmp = e'.Render
    let pic = new PictureBox(Image=bmp, Height=bmp.Height, Width=bmp.Width)
    scroll.Controls.Clear()
    scroll.Controls.Add pic

let NewExpressionError form s =
    let cform, scroll = CreateScrollableChildWindow form
    let label = new Label(Text=s, Font=new Font("Courier New", 10.f), AutoSize=true)
    scroll.Controls.Add label
    cform.Show()

exception SyntaxError

let Parse s =
    let lex = Lexing.LexBuffer<char>.FromString s
    try ExprParser.expr ExprLexer.main lex
    with _ -> raise SyntaxError

let NewStringExpression form s =
    try
        let e1 = Parse s
        let e2 = Utils.Simplify e1
        let e3 = Utils.Differentiate "x" e2
        let e4 = Utils.Simplify e3
        NewExpression form s ["Original:", e1; "Simplified:", e2;
                              "Derivative:", e3; "Simplified:", e4]
    with
      | SyntaxError ->
          let msg = Printf.sprintf "Syntax error in:\n%s" s
          NewExpressionError form msg
      | Failure msg ->
          NewExpressionError form msg

let ConstructMainForm () =
    let form   = new Form(Text="Symbolic Differentiation Example",
                          IsMdiContainer=true,
                          Visible=true, Height=600, Width=700)
    let label   = new Label(Text="Enter function=", Width=100, Height=20)
    let tb      = new TextBox(Width=150, Left=100)
    let panel   = new Panel(Dock=DockStyle.Top, Height=tb.Height+50)
    let preview = new Panel(Dock=DockStyle.Bottom, BackColor=Color.White,
                            Height=50, BorderStyle=BorderStyle.FixedSingle)
    panel.Controls.AddRange([|label; preview; tb |])
    form.Controls.Add(panel)
    tb.KeyUp.Add (fun arg ->
       if arg.KeyCode = Keys.Enter then
           NewStringExpression form tb.Text
           tb.Text <- ""
           tb.Focus() |> ignore
       else
           try
               let e = Parse tb.Text
               UpdatePreview preview e
           with
           | _ -> ())
    form

let form = ConstructMainForm ()
NewStringExpression form "cos(sin(1/(x^2+1)))"
Application.Run(form)