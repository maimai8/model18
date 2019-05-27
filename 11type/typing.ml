open Syntax

(*  型を同じにできない場合に起きる例外 *)
exception Unify of Type.t * Type.t

(*  rが型 ty に現れるかをチェックする (occur check) *)
(* occur : Type.t option ref -> Type.t -> bool *)
let rec occur r ty = match ty with
  | Type.TInt -> false
  | Type.TBool -> false
  | Type.TFun (ty1, ty2) -> occur r ty1 || occur r ty2
  | Type.TVar (r') ->
    if r == r' then true
    else begin match !r' with
        None -> false
      | Some (ty') -> occur r ty'
    end
  | Type.TList (l) -> List.exists (occur r) l

(* ty1 = ty2 となるように、型変数への代入をする *)
(* unify : Type.t -> Type.t -> unit *)
let rec unify ty1 ty2 = match (ty1, ty2) with
  | (Type.TInt, Type.TInt) -> ()
  | (Type.TBool, Type.TBool) -> ()
  | (Type.TFun (ty1, ty1'), Type.TFun (ty2, ty2')) ->
    begin
      unify ty1 ty2;
      unify ty1' ty2'
    end
  | (Type.TVar(r1), Type.TVar(r2)) when r1 == r2 -> ()
  | (Type.TVar(r1), _) ->
    begin match !r1 with
        None -> if occur r1 ty2 then raise (Unify (ty1, ty2))
        else r1 := Some (ty2)
      | Some (ty1') -> unify ty1' ty2
    end
  | (_, Type.TVar(r2)) ->
    begin match !r2 with
        None -> if occur r2 ty1 then raise (Unify (ty1, ty2))
        else r2 := Some (ty1)
      | Some (ty2') -> unify ty1 ty2'
    end
  | (Type.TList (l1), Type.TList (l2)) -> List.iter2 unify l1 l2
  | (_, _) -> raise (Unify (ty1, ty2))

(* 型推論 *)
(* g : Syntax.t -> (string, Type.t) Env.t -> Type.t *)
let rec g expr tenv =
  try
    begin match expr with
      | Number (n) -> Type.TInt
      | Bool (b) -> Type.TBool
      | Variable (name) ->
        begin
          try Env.get tenv name with
          | _ -> failwith ("variable 型エラー : " ^ Syntax.to_string expr ^ " に型が付きません") (* Type.gen_type () *)
        end
      | Op (t1, op, t2) ->
        begin match op with
            Plus | Minus | Times | Devide ->
            let ty1 = g t1 tenv in
            let ty2 = g t2 tenv in
            unify ty1 Type.TInt;
            unify ty2 Type.TInt;
            Type.TInt
          | Equal | NotEqual | Less | OrLess ->
            let ty1 = g t1 tenv in
            let ty2 = g t2 tenv in
            unify ty1 Type.TInt;
            unify ty2 Type.TInt;
            Type.TBool
        end
      | If (arg1, arg2, arg3) ->
        let condTy = g arg1 tenv in
        let ty2 = g arg2 tenv in
        let ty3 = g arg3 tenv in
        unify condTy Type.TBool;
        unify ty2 ty3;
        ty2
      | Let (name, arg1, arg2) ->
        let ty1 = g arg1 tenv in
        let tenv' = Env.extend tenv name ty1 in
        g arg2 tenv'
      | LetRec (fname, argname, arg1, arg2) ->
        let tx = Type.gen_type () in
        let ty1' = Type.gen_type () in
        let tf = Type.TFun (tx, ty1') in
        let tenv' = Env.extend tenv fname tf in
        let tenv'' = Env.extend tenv' argname tx in
        let ty1 = g arg1 tenv'' in
        unify ty1 ty1';
        g arg2 tenv'
      | Fun (name, arg1) ->
        let ty1' = Type.gen_type () in
        let tenv' = Env.extend tenv name ty1' in
        let ty2 = g arg1 tenv' in
        Type.TFun (ty1', ty2)
      | App (arg1, arg2) ->
        let ty2' = Type.gen_type () in
        let ty1' = Type.gen_type () in
        let t = Type.TFun (ty2', ty1') in
        let ty1 = g arg1 tenv in
        unify ty1 t;
        let ty2 = g arg2 tenv in
        unify ty2 ty2';
        ty1'
      | Nil -> Type.TList ([Type.gen_type ()])
      | Cons (arg1, arg2) ->
        let ty1 = g arg1 tenv in
        let ty2' = Type.TList ([ty1]) in
        let ty2 = g arg2 tenv in
        unify ty2 ty2';
        ty2
      | Match (arg1, arg2, name1, name2, arg3) ->
        (* let ty1 = g arg1 tenv in *)
        let ty1hd = Type.gen_type () in
        let ty1' = Type.TList ([ty1hd]) in
        let ty1 = g arg1 tenv in
        unify ty1 ty1';
        let ty2 = g arg2 tenv in
        let tenv' = Env.extend (Env.extend tenv name1 ty1hd) name2 ty1 in
        let t = g arg3 tenv' in
        unify ty2 t;
        ty2
      | Raise (arg) ->
        let t = g arg tenv in
        unify t Type.TInt;
        Type.gen_type () (* 何でも良い... *)
      | Try (arg1, name, arg2) ->
        let ty1 = g arg1 tenv in
        let tenv' = Env.extend tenv name Type.TInt in
        let ty2 = g arg2 tenv' in
        unify ty1 ty2;
        ty2
    end
  with
  | Unify (ty1, ty2) -> begin (* unify できなかった *)
      print_endline "式";
      print_string " ";
      Syntax.print expr;
      print_newline ();
      print_endline "を型推論中に型エラーがおきました。";
      print_string " ";
      Type.print ty1;
      print_newline ();
      print_endline "と";
      Type.print ty2;
      print_newline ();
      print_endline "は unify できません。";
      exit 0
    end

(* 型推論の入り口 *)
(* Typing.f Syntax.t -> Type.t *)
let f expr =
  let ty = g expr Env.empty in
  ty
