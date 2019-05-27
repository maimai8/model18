open Syntax

(* Value.t : プログラムの実行結果を表す型 *)
type t = VNumber of int
       | VBool of bool
       | VClo of string * Syntax.t * (string, t) Env.t
       | VCloR of string * string * Syntax.t * (string, t) Env.t
       | VList of t list

(* Value.a : プログラムの最終結果を表す型 *)
type a = Ok of t
       | Error of int

let ok x = Ok (x)

(* string list -> string  *)
let rec lst_to_string lst = match lst with
    [] -> "[]"
  | first :: rest -> first ^ " :: " ^ lst_to_string rest

(* プログラムを文字列にする関数 *)
(* Value.to_stringS : Syntax.t -> Env.t -> string list -> string *)
let rec to_stringS exp env checklst =
  match exp with
    Number (n) -> string_of_int n
  | Bool (b) -> if b then "true" else "false"
  | Variable (name) ->
    if (List.mem name checklst) then name
    else
      (begin
        try
          match Env.get env name with
          | VNumber (n) -> string_of_int n
          | VBool (b) -> if b then "true" else "false"
          | VClo (name, arg, env) ->
            let newchecklst = name :: checklst in
            name ^ " -> " ^ to_stringS arg env newchecklst
          | VCloR (fname, argname, arg, env) ->
            let newchecklst = fname :: argname :: checklst in
            argname ^ " -> " ^ to_stringS arg env newchecklst
          | VList (l) -> "list..."
        with
        | _ -> failwith ("Unbound variable2: " ^ name)
      end)
  | Op (arg1, op, arg2) ->
    "(" ^ to_stringS arg1 env checklst
    ^ op_to_string op
    ^ to_stringS arg2 env checklst ^ ")"
  | If (arg1, arg2, arg3) ->
    "if " ^ to_stringS arg1 env checklst
    ^ " then " ^ to_stringS arg2 env checklst
    ^ " else " ^ to_stringS arg3 env checklst
  | Let (name, arg1, arg2) ->
    let newchecklst = name :: checklst in
    "let " ^ name ^ " = " ^ to_stringS arg1 env checklst
    ^ " in " ^ to_stringS arg2 env newchecklst
  | LetRec (fname, argname, arg1, arg2) ->
    let newchecklst = fname :: argname :: checklst in
    "let rec " ^ fname ^ " " ^ argname ^ " = " ^ to_stringS arg1 env newchecklst
    ^ " in " ^ to_stringS arg2 env newchecklst
  | Fun (name, arg1) ->
    let newchecklst = name :: checklst in
    name ^ " -> " ^ to_stringS arg1 env newchecklst
  | App (arg1, arg2) ->
    "(" ^ to_stringS arg1 env checklst ^ " " ^ to_stringS arg2 env checklst ^ ")"
  | Nil -> "[]"
  | Cons (arg1, arg2) ->
    to_stringS arg1 env checklst ^ "::" ^ to_stringS arg2 env checklst
  | Match (arg1, arg2, name1, name2, arg3) ->
    let newchecklst = name1 :: name2 :: checklst in
    "match " ^ to_stringS arg1 env checklst ^ " with [] -> " ^
    to_stringS arg2 env checklst ^ " | " ^ name1 ^ " :: " ^
    name2 ^ " -> " ^ to_stringS arg3 env newchecklst
  | Raise (arg) ->
    "raise (Error " ^ to_stringS arg env checklst ^ ") "
  | Try (arg1, name, arg2) ->
    "try " ^ to_stringS arg1 env checklst ^ " with Error " ^
    name ^ " -> " ^ to_stringS arg2 env checklst


(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let rec to_string value = match value with
    VNumber (n) -> string_of_int n
  | VBool (b) -> if b then "true" else "false"
  | VClo (x, arg, e) ->
    let s = to_stringS arg e [x] in
    x ^ " -> " ^ s ^ " = <fun>"
  | VCloR (g, x, arg, e) ->
    let s = to_stringS arg e [g; x] in
    x ^ " -> " ^ s ^ " = <fun>"
  | VList (l) -> lst_to_string (List.map (fun n -> to_string n) l)

let to_stringA ans = match ans with
  | Ok (v) -> to_string v
  | Error (n) -> "Error" ^ string_of_int n

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.a -> unit *)
let print exp =
  let str = to_stringA exp in
  print_string str
