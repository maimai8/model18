open Syntax
open Value

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> Value.t *)
let rec f expr = match expr with
  Number (n) -> VNumber (n)
| Bool (b) -> VBool (b)
| Op (arg1, Plus, arg2) ->
  let v1 = f arg1 in
  let v2 = f arg2 in
  begin match (v1, v2) with
    (VNumber (n1), VNumber (n2)) -> VNumber (n1 + n2)
  | (_, _) -> failwith ("Bad arguments to +: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
  end
| Op (arg1, Minus, arg2) ->
  let v1 = f arg1 in
  let v2 = f arg2 in
  begin match (v1, v2) with
    (VNumber (n1), VNumber (n2)) -> VNumber (n1 - n2)
  | (_, _) -> failwith ("Bad arguments to -: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
  end
| Op (arg1, Times, arg2) ->
  let v1 = f arg1 in
  let v2 = f arg2 in
  begin match (v1, v2) with
    (VNumber (n1), VNumber (n2)) -> VNumber (n1 * n2)
  | (_, _) -> failwith ("Bad arguments to *: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
  end
| Op (arg1, Equal, arg2) ->
  let v1 = f arg1 in
  let v2 = f arg2 in
  begin match (v1, v2) with
  (VNumber (n1), VNumber (n2)) ->
    if n1 = n2 then VBool (true)
    else VBool (false)
  | (_, _) -> failwith ("Bad arguments to =: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
  end
| Op (arg1, NotEqual, arg2) ->
  let v1 = f arg1 in
  let v2 = f arg2 in
  begin match (v1, v2) with
    (VNumber (n1), VNumber (n2)) ->
      if n1 <> n2 then VBool (true)
      else VBool (false)
  | (_, _) -> failwith ("Bad arguments to <>: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
  end
| Op (arg1, Less, arg2) ->
  let v1 = f arg1 in
  let v2 = f arg2 in
  begin match (v1, v2) with
    (VNumber (n1), VNumber (n2)) ->
      if n1< n2 then VBool (true)
      else VBool (false)
  | (_, _) -> failwith ("Bad arguments to <: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
  end
| Op (arg1, OrLess, arg2) ->
  let v1 = f arg1 in
  let v2 = f arg2 in
  begin match (v1, v2) with
    (VNumber (n1), VNumber (n2)) ->
      if n1 <= n2 then VBool (true)
      else VBool (false)
  | (_, _) -> failwith ("Bad arguments to <=: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
  end
