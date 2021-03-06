(* シグネチャ
type ('a, 'b) t
(* キーが 'a 型、値が 'b 型の環境を表す型 *)

val empty : ('a, 'b) t
(* 使い方：empty *)
(* 空の環境 *)

val get : ('a, 'b) t -> 'a -> 'b
(* 使い方：get env var *)
(* 環境 env の中で変数 var の値を返す *)
(* 変数 var が見つからなかったら例外 Not_found を起こす *)

val extend : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t
(* 使い方：extend env var value *)
(* 環境 env に変数 var の値を value に登録した新たな環境を返す *)
 *)

 (* 環境：
    変数 key の値が value であることを保持するテーブル *)

(* type ('a, 'b) t =
  | ENil
  | ECons of ('a, 'b) t * 'a * 'b *)

type ('a, 'b) t = ('a * 'b) list

(* 使い方：empty *)
 (* 空の環境 *)
let empty = []

(* get : ('a, 'b) t -> 'a -> 'b *)
(* 使い方：get env var *)
(* 環境 env の中で変数 var の値を返す *)
(* 変数 var が見つからなかったら例外 Not_found を起こす *)
let rec get env var =
  match env with
    [] -> raise (Not_found)
  | (first_var, first_value) :: rest ->
    if var = first_var then first_value
    else get rest var

(* extend : ('a, 'b) t -> 'a -> 'b -> ('a, 'b) t *)
(* 使い方：extend env var value *)
(* 環境 env に変数 var の値を value に登録した新たな環境を返す *)
let extend env var value = (var, value) :: env

let rec length env = match env with
    [] -> 0
  | first :: rest -> 1 + length rest
