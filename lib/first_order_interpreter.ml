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

let mk_closr l e = FunVal (Closr { lam = l; en = e })
let mk_sc () = FunVal Sc
let mk_eq1 () = FunVal Eq1
let mk_eq2 a = FunVal (Eq2 { arg1 = a })
let mk_rec r e = Rec { letx = r; old = e }
let mk_simp z a e = Simp { bvar = z; bval = a; old = e }
let mk_init () = Init

(** Never calls any other recursive function, and can never call itself more times than the length of the searched list *)
let rec get e x =
  match e with
  | Init -> (
      match x with
      | "succ" -> mk_sc ()
      | "equal" -> mk_eq1 ()
      | x -> raise (NameError x))
  | Simp { bvar; bval; old } -> if String.equal bvar x then bval else get old x
  | Rec { letx; old } ->
      if String.equal letx.dvar x then
        mk_closr letx.dexp
          e (* essentially rebind `e = Rec { letx; old }` as a cyclic record *)
      else get old x

let rec eval r e =
  match r with
  | Const k -> evcon k
  | Var x -> get e x
  | Appl a -> apply (eval a.opr e) (eval a.opnd e)
  | Lambda l -> mk_closr l e
  | Cond c -> (
      match eval c.prem e with
      | Bool b -> if b then eval c.conc e else eval c.altr e
      | _ -> raise TypeError)
  | LetRec r -> eval r.rbody (mk_rec r e)

and apply f a =
  match f with
  | FunVal f -> (
      match f with
      | Closr { lam; en } -> eval lam.body (mk_simp lam.fp a en)
      | Sc -> succ a
      | Eq1 -> mk_eq2 a
      | Eq2 f -> equal f.arg1 a)
  | _ -> raise TypeError

and evcon k = Int k

let interpret r = eval r (mk_init ())
