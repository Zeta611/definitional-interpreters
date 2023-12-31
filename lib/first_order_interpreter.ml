open Base

exception TypeError
exception NameError of string

[@@@warning "-27"]

(** set of expressions of the defined language *)
type exp =
  | Const of const
  | Var of var
  | Appl of appl
  | Lambda of lambda
  | Cond of cond
  | LetRec of let_rec

and const = int
and var = string
and appl = { opr : exp; opnd : exp }
and lambda = { fp : string; body : exp }
and cond = { prem : exp; conc : exp; altr : exp }
and let_rec = { dvar : var; dexp : lambda; rbody : exp }

(** set of value representations of the defined language *)
and value = Int of int | Bool of bool | FunVal of fun_val [@@deriving show]

and fun_val =
  | Closr of { lam : lambda; en : env }
  | Sc
  | Eq1
  | Eq2 of { arg1 : value }

and env =
  | Init
  | Simp of { bvar : var; bval : value; old : env }
  | Rec of { letx : let_rec; old : env }

[@@@warning "+27"]

let succ a = match a with Int n -> Int (n + 1) | _ -> raise TypeError

let equal a b =
  match (a, b) with
  | Int n, Int m -> Bool (Int.equal n m)
  | Bool a, Bool b -> Bool (Bool.equal a b)
  | _ -> raise TypeError

(** Never calls any other recursive function, and can never call itself more times than the length of the searched list.
    Not serious! *)
let rec get e x =
  match e with
  | Init -> (
      match x with
      | "succ" -> FunVal Sc
      | "equal" -> FunVal Eq1
      | x -> raise (NameError x))
  | Simp { bvar; bval; old } -> if String.equal bvar x then bval else get old x
  | Rec { letx; old } ->
      if String.equal letx.dvar x then
        (* essentially rebind `e = Rec { letx; old }` as a cyclic record *)
        FunVal (Closr { lam = letx.dexp; en = e })
      else get old x

(** Serious *)
let rec eval r e =
  match r with
  (* not serious *)
  | Const k -> evcon k
  (* not serious *)
  | Var x -> get e x
  (* The order of application is still dependent to the defining language, in this case, call-by-value from OCaml.
     To CPS-transform, we need four serious operations, in the order of (for the defined language to be CBV):
     1. operator evaluation
     2. operand evaluation
     3. apply 1 to 2
     4. apply c to the continuation *)
  | Appl { opr; opnd } -> apply (eval opr e) (eval opnd e)
  (* not serious *)
  | Lambda l -> FunVal (Closr { lam = l; en = e })
  | Cond { prem; conc; altr } -> (
      (* To CPS-transform, we need three serious operations:
         1. premiss evaluation
         2. conclusion/alternative evaluation
         3. apply c to 2 *)
      match eval prem e with
      | Bool b -> if b then eval conc e else eval altr e
      | _ -> raise TypeError)
  (* serious, but does not contain serious operands *)
  | LetRec r -> eval r.rbody (Rec { letx = r; old = e })

(** Serious *)
and apply f a =
  match f with
  | FunVal f -> (
      match f with
      (* serious, but does not contain serious operands *)
      | Closr { lam; en } ->
          eval lam.body (Simp { bvar = lam.fp; bval = a; old = en })
      (* not serious *)
      | Sc -> succ a
      (* not serious *)
      | Eq1 -> FunVal (Eq2 { arg1 = a })
      (* not serious *)
      | Eq2 f -> equal f.arg1 a)
  | _ -> raise TypeError

and evcon k = Int k

let interpret r = eval r Init
