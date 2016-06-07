open Abstract_syntax_tree
open Cfg

type t = Bottom | Top | Int of Z.t

let top = Top
let bottom = Bottom
let const x = Int x

let rand x1 x2 = if x1=x2 then Int x1 else if x1>x2 then Bottom else Top

let unary v = function
| AST_UNARY_PLUS -> v
| AST_UNARY_MINUS -> (match v with
    | Bottom | Top -> v
    | Int x -> Int(Z.neg x))

let binary v1 v2 op = match v1,v2 with
| (Bottom as v),_ | _,(Bottom as v)| (Top as v),_ | _,(Top as v) ->
    v
| Int x1, Int x2 -> Int((match op with
    | AST_PLUS -> Z.add
    | AST_MINUS -> Z.sub
    | AST_MULTIPLY -> Z.mul
    | AST_DIVIDE -> Z.div
    | AST_MODULO -> Z.rem
    ) x1 x2)

let swap (a,b) = (b,a)

let rec compare v1 v2 op = match v1,v2,op with
| Bottom,_,_ | _,Bottom,_ -> Bottom,Bottom
| Top,Top,_ -> Top,Top
| Int x1,Int x2,AST_EQUAL -> if x1=x2 then v1,v2 else Bottom,Bottom
| Int x,_,AST_EQUAL | _,Int x,AST_EQUAL -> Int x, Int x
| Int x1,Int x2,AST_NOT_EQUAL ->
    if x1<>x2 then v1,v2 else Bottom,Bottom
| _,_,AST_NOT_EQUAL -> v1,v2
| Int x1,Int x2,AST_LESS -> if x1 < x2 then v1,v2 else Bottom,Bottom
| Int x1,Int x2,AST_LESS_EQUAL -> if x1 <= x2 then v1,v2 else Bottom,Bottom
| _,_,AST_LESS | _,_,AST_LESS_EQUAL -> v1,v2
| _,_,AST_GREATER -> swap (compare v2 v1 AST_LESS)
| _,_,AST_GREATER_EQUAL -> swap (compare v2 v1 AST_LESS_EQUAL)

let bwd_unary x op r = match x,r with
| Bottom,_ | _,Bottom -> Bottom
| _,Top -> x
| Top,r -> unary r op
| Int x, Int y -> (match op with
    | AST_UNARY_PLUS -> if x = y then Int x else Bottom
    | AST_UNARY_MINUS -> if x = (Z.neg y) then Int x else Bottom
    )

let bwd_binary v1 v2 op res = match v1,v2,res with
| Bottom,_,_ | _,Bottom,_ | _,_,Bottom -> Bottom,Bottom
| _,_,Top -> v1,v2
| Top,Top,_ -> v1,v2
| Int _,Int _,Int _ -> 
    if binary v1 v2 op = res then v1,v2 else Bottom,Bottom
| Int x1,Top,Int r ->
    (match op with
    | AST_PLUS -> v1, Int (Z.sub r x1)
	| AST_MINUS -> v1, Int (Z.add x1 r)
	| AST_MULTIPLY ->
        if x1 = Z.zero then v1,Top
        else
            let qu,rem = Z.div_rem r x1 in
            if rem = Z.zero then v1, Int qu else Bottom,Bottom
	| AST_DIVIDE ->
        if r = Z.zero then
            if x1 = Z.zero then v1,Top else Bottom,Bottom
        else
            let qu,rem = Z.div_rem x1 r in
            if rem = Z.zero then v1,Int qu else Bottom,Bottom
	| AST_MODULO -> v1,Top
    )
| Top,Int x2,Int r ->
    (match op with
    | AST_PLUS -> Int (Z.sub r x2),v2
	| AST_MINUS -> Int (Z.add x2 r),v2
	| AST_MULTIPLY ->
        if x2 = Z.zero then Top,v2
        else
            let qu,rem = Z.div_rem r x2 in
            if rem = Z.zero then Int qu,v2 else Bottom,Bottom
	| AST_DIVIDE -> Int (Z.mul x2 r), v2
	| AST_MODULO ->
        if r >= x2 then Bottom,Bottom else Top,v2
    )

let join v1 v2 = match (v1,v2) with
| Top,_ | _,Top -> Top
| v,Bottom | Bottom,v -> v
| Int x1, Int x2 ->
        if x1 = x2 then Int x1 else Top
        
let meet v1 v2 = match v1,v2 with
| Bottom,_ | _,Bottom -> Bottom
| Top,v | v,Top -> v
| Int x1, Int x2 ->
        if x1 = x2 then Int x1 else Bottom

let widen v1 v2 = join v1 v2

let subset d1 d2 = match d1,d2 with
| Bottom,_ | _,Top -> true
| Top,_ | _,Bottom -> false
| Int v1, Int v2 -> v1 = v2

let is_bottom = function
| Bottom -> true
| Int _ | Top -> false

let print ch d =
    Printf.fprintf ch (match d with
    | Top -> "⊤"
    | Bottom -> "⊥"
    | Int x -> Scanf.format_from_string (Z.to_string x) "" )
