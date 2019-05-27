open Syntax

(* Value.t : プログラムの実行結果を表す型 *)
type t = VNumber of int
       | VBool of bool
       | VClo of string * Syntax.t * (string, t) Env.t

(* プログラムを文字列にする関数 *)
(* Syntax.to_string : Syntax.t -> Env.t -> string list -> string *)
let rec to_stringS exp env checklst = match exp with
    Number (n) -> string_of_int n
  | Bool (b) -> if b then "true" else "false"
  | Variable (name) -> if (List.mem name checklst) then name
    else
      (begin
        try
          match Env.get env name with
          | VNumber (n) -> string_of_int n
          | VBool (b) -> if b then "true" else "false"
          | VClo (name, arg, env) ->
            "fun " ^ name ^ " -> " ^ to_stringS arg env checklst
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
    "let " ^ name ^ " = " ^ to_stringS arg1 env checklst
    ^ " in " ^ to_stringS arg2 env checklst
  | Fun (name, arg1) ->
    let newchecklst = name :: checklst in
    name ^ " -> " ^ to_stringS arg1 env newchecklst
  | App (arg1, arg2) ->
    "(" ^ to_stringS arg1 env checklst ^ " " ^ to_stringS arg2 env checklst ^ ")"

(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let rec to_string value = match value with
    VNumber (n) -> string_of_int n
  | VBool (b) -> if b then "true" else "false"
  | VClo (x, arg, e) ->
    let s = to_stringS arg e [x] in
    x ^ " -> " ^ s ^ " = <fun>" (* ここを改良するのが課題！ *)

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
