open Syntax
open Value
open Env

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> () Env.t -> Value.a *)
let rec f expr env cont =
  (* print_endline ("f : " ^ Syntax.to_string expr);
   * print_int (Env.length env);
   * print_newline(); *)
  match expr with
  | Number (n) -> cont (VNumber (n))
  | Bool (b) -> cont (VBool (b))
  | Variable (name) ->
    let v = 
      try Env.get env name with
      | _ -> failwith ("Unbound variable: " ^ name) in
    cont v
  | Op (arg1, Plus, arg2) ->
    f arg1 env
      (fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 + n2))
               | (_, _) -> failwith ("Bad arguments to +: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end))
  | Op (arg1, Minus, arg2) ->
    f arg1 env
      (fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 - n2))
               | (_, _) -> failwith ("Bad arguments to -: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end))
  | Op (arg1, Times, arg2) ->
    f arg1 env
      (fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VNumber (n1 * n2))
               | (_, _) -> failwith ("Bad arguments to *: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end))
  | Op (arg1, Devide, arg2) ->
    f arg1 env
      (fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) ->
                 if n2 = 0 then Error (0)
                 else cont (VNumber(n1 / n2))
               | (_, _) -> failwith ("Bad arguments to /: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end))
  | Op (arg1, Equal, arg2) ->
    f arg1 env
      (fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VBool (n1 = n2))
               | (_, _) -> failwith ("Bad arguments to =: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end))
  | Op (arg1, NotEqual, arg2) ->
    f arg1 env
      (fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VBool (n1 <> n2))
               | (_, _) -> failwith ("Bad arguments to <>: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end))
  | Op (arg1, Less, arg2) ->
    f arg1 env
      (fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VBool (n1 < n2))
               | (_, _) -> failwith ("Bad arguments to <: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end))
  | Op (arg1, OrLess, arg2) ->
    f arg1 env
      (fun v1 -> f arg2 env
          (fun v2 ->
             begin match (v1, v2) with
                 (VNumber (n1), VNumber (n2)) -> cont (VBool (n1 <= n2))
               | (_, _) -> failwith ("Bad arguments to <=: " ^
                                     Value.to_string v1 ^ ", " ^
                                     Value.to_string v2)
             end))
  | If (arg1, arg2, arg3) ->
    f arg1 env
      (fun v1 -> 
         begin match v1 with
             VBool (b) ->
             if b then f arg2 env cont
             else f arg3 env cont
           | _ -> failwith ("Predicate part is not a boolean: "
                            ^ Value.to_string v1)
         end)
  | Let (name, arg1, arg2) ->
    f arg1 env
      (fun v1 ->
         let newenv = Env.extend env name v1 in
         f arg2 newenv cont)
  | LetRec (fname, argname, arg1, arg2) ->
    let newenv = Env.extend env fname (VCloR(fname, argname, arg1, env)) in
    f arg2 newenv cont
  | Fun (name, arg) -> cont (VClo (name, arg, env))
  | App (arg1, arg2) ->
    f arg1 env
      (fun v1 -> f arg2 env
          (fun v2 ->
             begin match v1 with
               | VClo (vname, varg, venv) ->
                 let newenv = Env.extend venv vname v2 in
                 f varg newenv cont
               | VCloR (g, x, varg, venv) ->
                 let newenv = Env.extend (Env.extend venv g v1) x v2 in
                 f varg newenv cont
               | _ -> failwith (Value.to_string v1 ^ " is not a function")
             end))
  | Nil -> cont (VList([]))
  | Cons (arg1, arg2) ->
    f arg1 env
      (fun v1 -> f arg2 env
          (fun v2 ->
             begin match v2 with
               | VList (l2) -> cont (VList (v1 :: l2))
               | _ -> failwith (Value.to_string v2 ^ " is not a list")
             end))
  | Match (arg1, arg2, name1, name2, arg3) ->
    f arg1 env
      (fun v1 -> begin match v1 with
           | VList ([]) -> f arg2 env (fun v2 -> cont v2)
           | VList (first :: rest) ->
             let env' = Env.extend env name1 first in
             let newenv = Env.extend env' name2 (VList(rest)) in
             f arg3 newenv cont
           | _ -> failwith (Value.to_string v1 ^ " is not a list")
         end)
  | Raise (arg) ->
    f arg env
      (fun v ->
         begin match v with
           | VNumber (n) -> Error (n)
           | _ -> failwith (Value.to_string v ^ " is not a number")
         end)
  | Try (arg1, name, arg2) ->
    let v = f arg1 env (fun x -> Ok (x)) in
    begin match v with
      | Error (n) ->
        let newenv = Env.extend env name (VNumber (n)) in
        f arg2 newenv cont
      | Ok (v) -> cont v
    end
