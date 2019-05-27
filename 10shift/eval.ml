open Syntax
open Value
open Env

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> () Env.t -> Value.a *)
let rec f expr env cont = match expr with
  | Number (n) -> cont (VNumber (n))
  | Bool (b) -> cont (VBool (b))
  | Variable (name) ->
      let v = try Env.get env name with
        | _ -> failwith ("Unbound variable: " ^ name) in
      cont v
  | Op (arg1, Plus, arg2) ->
    let cont' =
      fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 + n2))
               | (_, _) -> failwith ("Bad arguments to +: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end)
    in f arg1 env cont' (* t1を実行する *)
  | Op (arg1, Minus, arg2) ->
    let cont' =
      fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 - n2))
               | (_, _) -> failwith ("Bad arguments to -: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end)
    in f arg1 env cont' (* t1を実行する *)
  | Op (arg1, Times, arg2) ->
    let cont' =
      fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 * n2))
               | (_, _) -> failwith ("Bad arguments to *: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end)
    in f arg1 env cont' (* t1を実行する *)
  | Op (arg1, Equal, arg2) ->
    let cont' =
      fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VBool (n1 = n2))
               | (_, _) -> failwith ("Bad arguments to =: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end)
    in f arg1 env cont' (* t1を実行する *)
  | Op (arg1, NotEqual, arg2) ->
    let cont' =
      fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VBool (n1 <> n2))
               | (_, _) -> failwith ("Bad arguments to <>: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end)
    in f arg1 env cont' (* t1を実行する *)
  | Op (arg1, Less, arg2) ->
    let cont' =
      fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VBool (n1 < n2))
               | (_, _) -> failwith ("Bad arguments to <: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end)
    in f arg1 env cont' (* t1を実行する *)
  | Op (arg1, OrLess, arg2) ->
    let cont' =
      fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VBool (n1 <= n2))
               | (_, _) -> failwith ("Bad arguments to <=: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end)
    in f arg1 env cont' (* t1を実行する *)
  | If (arg1, arg2, arg3) ->
    let cont' =
      fun v1 -> 
        begin match v1 with
            VBool (b) ->
            if b then f arg2 env (fun v2 -> cont v2)
            else f arg3 env (fun v3 -> cont v3)
          | _ -> failwith ("Predicate part is not a boolean: "
                           ^ Value.to_string v1)
        end
    in f arg1 env cont' (* t1を実行する *)
  | Let (name, arg1, arg2) ->
    let cont' =
      fun v1 ->
        let newenv = Env.extend env name v1 in
        f arg2 newenv (fun v2 -> cont v2)
    in f arg1 env cont' (* t1を実行する *)
  | LetRec (fname, argname, arg1, arg2) ->
    let newenv = Env.extend env fname (VCloR(fname, argname, arg1, env)) in
    f arg2 newenv cont
  | Fun (name, arg) -> cont (VClo (name, arg, env))
  | App (arg1, arg2) ->
    let cont' =
      fun v1 -> f arg2 env
          (fun v2 ->
             begin match v1 with
               | VClo (vname, varg, venv) ->
                 let newenv = Env.extend venv vname v2 in
                 f varg newenv cont
               | VCloR (g, x, varg, venv) ->
                 let newenv = Env.extend (Env.extend venv g v1) x v2 in
                 f varg newenv cont
               | VCont (k) -> cont (k v2)
               | _ -> failwith (Value.to_string v1 ^ " is not a function")
             end)
    in f arg1 env cont' (* t1を実行する *)
  | Nil -> cont (VList([]))
  | Cons (arg1, arg2) ->
    let cont' =
      fun v1 -> f arg2 env
          (fun v2 ->
             begin match v2 with
               | VList (l2) -> cont (VList (v1 :: l2))
               | _ -> failwith (Value.to_string v2 ^ " is not a list")
             end)
    in f arg1 env cont' (* t1を実行する *)
  | Match (arg1, arg2, name1, name2, arg3) ->
    let cont' =
      fun v1 -> begin match v1 with
          | VList ([]) -> f arg2 env (fun v2 -> cont v2)
          | VList (first :: rest) ->
            let env' = Env.extend env name1 first in
            let newenv = Env.extend env' name2 (VList(rest)) in
            f arg3 newenv (fun v3 -> cont v3)
          | _ -> failwith (Value.to_string v1 ^ " is not a list")
        end
    in f arg1 env cont' (* t1を実行する *)
  | Shift (k, arg) ->
    let newenv = Env.extend env k (VCont (cont)) in
    let cont' =
      fun v -> (fun x -> x) v in
    f arg newenv cont'
  | Reset (arg) ->
    let v1 = f arg env (fun x -> x) in
    cont v1
        
