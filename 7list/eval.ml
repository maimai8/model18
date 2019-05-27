open Syntax
open Value
open Env

(* 実際の計算をする関数 *)
(* Eval.f : Syntax.t -> Value.t *)
let rec f expr env = match expr with
    Number (n) -> VNumber (n)
  | Bool (b) -> VBool (b)
  | Variable (name) ->
    begin
      try
        match Env.get env name with
        | VNumber (n) -> VNumber (n)
        | VBool (b) -> VBool (b)
        | VClo (name, arg, env) -> VClo (name, arg, env)
        | VCloR (fname, argname, arg, env) -> VCloR (fname, argname, arg, env)
        | VList (l) -> VList (l)
      with
      | _ -> failwith ("Unbound variable: " ^ name)
    end
  | Op (arg1, Plus, arg2) ->
    let v1 = f arg1 env in
    let v2 = f arg2 env in
    begin match (v1, v2) with
        (VNumber (n1), VNumber (n2)) -> VNumber (n1 + n2)
      | (_, _) -> failwith ("Bad arguments to +: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
    end
  | Op (arg1, Minus, arg2) ->
    let v1 = f arg1 env in
    let v2 = f arg2 env in
    begin match (v1, v2) with
        (VNumber (n1), VNumber (n2)) -> VNumber (n1 - n2)
      | (_, _) -> failwith ("Bad arguments to -: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
    end
  | Op (arg1, Times, arg2) ->
    let v1 = f arg1 env in
    let v2 = f arg2 env in
    begin match (v1, v2) with
        (VNumber (n1), VNumber (n2)) -> VNumber (n1 * n2)
      | (_, _) -> failwith ("Bad arguments to *: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
    end
  | Op (arg1, Equal, arg2) ->
    let v1 = f arg1 env in
    let v2 = f arg2 env in
    begin match (v1, v2) with
        (VNumber (n1), VNumber (n2)) -> VBool (n1 = n2)
      | (_, _) -> failwith ("Bad arguments to =: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
    end
  | Op (arg1, NotEqual, arg2) ->
    let v1 = f arg1 env in
    let v2 = f arg2 env in
    begin match (v1, v2) with
        (VNumber (n1), VNumber (n2)) -> VBool (n1 <> n2)
      | (_, _) -> failwith ("Bad arguments to <>: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
    end
  | Op (arg1, Less, arg2) ->
    let v1 = f arg1 env in
    let v2 = f arg2 env in
    begin match (v1, v2) with
        (VNumber (n1), VNumber (n2)) -> VBool (n1 < n2)
      | (_, _) -> failwith ("Bad arguments to <: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
    end
  | Op (arg1, OrLess, arg2) ->
    let v1 = f arg1 env in
    let v2 = f arg2 env in
    begin match (v1, v2) with
        (VNumber (n1), VNumber (n2)) -> VBool (n1 <= n2)
      | (_, _) -> failwith ("Bad arguments to <=: " ^
                            Value.to_string v1 ^ ", " ^
                            Value.to_string v2)
    end
  | If (arg1, arg2, arg3) ->
    let v = f arg1 env in
    begin match v with
        VBool (b) -> if b then f arg2 env else f arg3 env
      | _ -> failwith ("Predicate part is not a boolean: " ^ Value.to_string v)
    end
  | Let (name, arg1, arg2) ->
    let v1 = f arg1 env in
    let newenv = Env.extend env name v1 in
    f arg2 newenv
  | LetRec (fname, argname, arg1, arg2) ->
    let newenv = Env.extend env fname (VCloR(fname, argname, arg1, env)) in
    f arg2 newenv
  | Fun (name, arg) -> VClo (name, arg, env)
  | App (arg1, arg2) ->
    let v1 = f arg1 env in
    let v2 = f arg2 env in
    begin match v1 with
      | VClo (vname, varg, venv) ->
        let newenv = Env.extend venv vname v2 in
        f varg newenv
      | VCloR (g, x, varg, venv) ->
        let newenv = Env.extend (Env.extend venv g v1) x v2 in
        f varg newenv
      | _ -> failwith (Value.to_string v1 ^ " is not a function")
    end
  | Nil -> VList([])
  | Cons (arg1, arg2) ->
    let v1 = f arg1 env in
    let v2 = f arg2 env in
    begin match v2 with
      | VList (l2) -> VList (v1 :: l2)
      | _ -> failwith (Value.to_string v2 ^ " is not a list")
    end
  | Match (arg1, arg2, name1, name2, arg3) ->
    let v1 = f arg1 env in
    begin match v1 with
      | VList ([]) -> f arg2 env
      | VList (first :: rest) ->
        let newenv = Env.extend (Env.extend env name1 first) name2 (VList(rest)) in
        f arg3 newenv
      | _ -> failwith (Value.to_string v1 ^ " is not a list")
    end
