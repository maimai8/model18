(* Type.t : プログラムの型を表す型 *)
type t = TInt
       | TBool
       | TFun of t * t (* 引数と返り値 *)
       | TVar of t option ref (* 型変数 *)
       | TList of t list (* リスト型 *)

(* 新しい型を作る *)
(* Type.gen_type : unit -> Type.t *)
let gen_type () = TVar (ref None)

(* 型変数を中身で置き換えた型を返す。返ってくる型には型変数は含まれない *)
(* deref_type : Type.t -> Type.t *)
let rec deref_type ty = match ty with
  | TInt -> TInt
  | TBool -> TBool
  | TFun (ty1, ty2) ->
    TFun (deref_type ty1, deref_type ty2)
  | TVar (r) ->
    begin match !r with
      | None ->
        r := Some (TInt); (* 何でも良い。こうすると、とりあえずInt型 *)
        (* TList([TInt]) *)
        TInt
      | Some (ty') ->
        let ty'' = deref_type ty' in
        r := Some (ty'');
        ty''
    end
  | TList (l) -> TList (List.map deref_type l)


(* プログラムの型を文字列にする関数 *)
(* Type.to_string : Type.t -> string *)
let rec to_string ty = match ty with
  | TInt -> "int"
  | TBool -> "bool"
  | TFun (ty1, ty2) ->
    to_string ty1 ^ " -> " ^ to_string ty2
  | TVar (r) -> "tvar"
  | TList (l) ->
    begin match l with
      | [] -> ""
      | f :: r -> to_string f ^ " list"
    end

(* プログラムの型をプリントする関数 *)
(* Type.print : Type.t -> unit *)
let print ty =
  let str = to_string (deref_type ty) in
  print_string str
