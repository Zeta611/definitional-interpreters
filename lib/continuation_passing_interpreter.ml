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

and cont =
  | Fin
  | Evopn of { ap : appl; en : env; next : cont }
  | Apfun of { func : value; next : cont }
  | Branch of { cn : cond; en : env; next : cont }

[@@@warning "+27"]

let succ a = match a with Int n -> Int (n + 1) | _ -> raise TypeError

let equal a b =
  match (a, b) with
  | Int n, Int m -> Bool (Int.equal n m)
  | Bool a, Bool b -> Bool (Bool.equal a b)
  | _ -> raise TypeError

(** Never calls any other recursive function, and can never call itself more times than the length of the searched list *)
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

let rec eval r e c =
  match r with
  | Const k -> cont c (evcon k)
  | Var x -> cont c (get e x)
  (* 1. call eval to evaluate the operator
     2. give this call a continuation that calls eval to evaluate the operand
     3. give this call a continuation that calls apply to apply the operator to the operand
     4. give this call a continuation that calls the continuation c to return the result
     Two continuations produced *)
  | Appl ap -> eval ap.opr e (Evopn { ap; en = e; next = c })
  | Lambda l -> cont c (FunVal (Closr { lam = l; en = e }))
  | Cond cn ->
      (* One continuation produced *)
      eval cn.prem e (Branch { cn; en = e; next = c })
  | LetRec r -> eval r.rbody (Rec { letx = r; old = e }) c

and apply f a c =
  match f with
  | FunVal f -> (
      match f with
      | Closr { lam; en } ->
          eval lam.body (Simp { bvar = lam.fp; bval = a; old = en }) c
      | Sc -> cont c (succ a)
      | Eq1 -> cont c (FunVal (Eq2 { arg1 = a }))
      | Eq2 f -> cont c (equal f.arg1 a))
  | _ -> raise TypeError

and cont c a =
  match c with
  | Fin -> a
  | Evopn { ap; en; next } -> eval ap.opnd en (Apfun { func = a; next })
  | Apfun { func; next } -> apply func a next
  | Branch { cn = { conc; altr; _ }; en; next } -> (
      match a with
      | Bool b -> if b then eval conc en next else eval altr en next
      | _ -> raise TypeError)

and evcon k = Int k

(* One continuation produced *)
let interpret r = eval r Init Fin
