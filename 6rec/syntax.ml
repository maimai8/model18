(* op_t : ２項演算子の型 *)
(* NotEqual と OrLess は syntax を足してしまって良いのかしら！！ *)
type op_t = Plus | Minus | Times | Equal | Less | NotEqual | OrLess

(* ２項演算子を文字列にする関数 *)
(* op_to_string : op_t -> string *)
let op_to_string op = match op with
    Plus -> " + "
  | Minus -> " - "
  | Times -> " * "
  | Equal -> " = "
  | NotEqual -> " <> "
  | Less -> " < "
  | OrLess -> " <= "

(* Syntax.t : プログラムを表す型 *)
type t = Number of int
       | Bool of bool
       | Variable of string
       | Op of t * op_t * t
       | If of t * t * t
       | Let of string * t * t
       | LetRec of string * string * t * t
       | Fun of string * t
       | App of t * t
(* 本当は、App の型は t * t list にしたい... *)

(* プログラムを文字列にする関数 *)
(* Syntax.to_string : Syntax.t -> string *)
let rec to_string exp = match exp with
    Number (n) -> string_of_int n
  | Bool (b) -> if b then "true" else "false"
  | Variable (name) -> name
  | Op (arg1, op, arg2) ->
	"(" ^ to_string arg1
	    ^ op_to_string op
     ^ to_string arg2 ^ ")"
  | If (arg1, arg2, arg3) ->
    "if " ^ to_string arg1
    ^ " then " ^ to_string arg2
    ^ " else " ^ to_string arg3
  | Let (name, arg1, arg2) ->
    "let " ^ name ^ " = " ^ to_string arg1
    ^ " in " ^ to_string arg2
  | LetRec (fname, argname, arg1, arg2) ->
    "let rec " ^ fname ^ " " ^ argname ^ " = " ^ to_string arg1
    ^ " in " ^ to_string arg2
  | Fun (name, arg1) ->
    "fun " ^ name ^ " -> " ^ to_string arg1
  | App (arg1, arg2) ->
    "(" ^ to_string arg1 ^ " " ^ to_string arg2 ^ ")"

(* プログラムをプリントする関数 *)
(* Syntax.print : Syntax.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
