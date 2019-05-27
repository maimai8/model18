(* Value.t : プログラムの実行結果を表す型 *)
type t = VNumber of int
       | VBool of bool
       | VClo of string * Syntax.t * (string, t) Env.t

(* プログラムの実行結果を文字列にする関数 *)
(* Value.to_string : Value.t -> string *)
let rec to_string value = match value with
    VNumber (n) -> string_of_int n
  | VBool (b) -> if b then "true" else "false"
  | VClo (x, arg, e) -> "<fun>"

(* プログラムの実行結果をプリントする関数 *)
(* Value.print : Value.t -> unit *)
let print exp =
  let str = to_string exp in
  print_string str
