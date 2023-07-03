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
  | Escp of escp

and const = int
and var = string
and appl = { opr : exp; opnd : exp }
and lambda = { fp : string; body : exp }
and cond = { prem : exp; conc : exp; altr : exp }
and let_rec = { dvar : var; dexp : lambda; rbody : exp }
and escp = { escv : var; ebody : exp }

(** set of value representations of the defined language *)
and value = Int of int | Bool of bool | FunVal of fun_val [@@deriving show]

and fun_val = value * cont -> value
(** functions in the defined language is defined using OCaml functions *)

and env = var -> value
(** environments are higher-order *)

and cont = value -> value

[@@@warning "+27"]

let succ a = match a with Int n -> Int (n + 1) | _ -> raise TypeError

let equal a b =
  match (a, b) with
  | Int n, Int m -> Bool (Int.equal n m)
  | Bool a, Bool b -> Bool (Bool.equal a b)
  | _ -> raise TypeError

let ext z a e x = if String.equal x z then a else e x

let rec eval r e c =
  match r with
  | Const k -> c (evcon k)
  | Var x -> c (e x)
  | Appl { opr; opnd } ->
      eval opr e (function
        | FunVal f -> eval opnd e (fun a -> f (a, c))
        | _ -> raise TypeError)
  | Lambda l -> c (evlambda l e)
  | Cond { prem; conc; altr } ->
      eval prem e (function
        | Bool b -> if b then eval conc e c else eval altr e c
        | _ -> raise TypeError)
  | LetRec r ->
      let rec e' x =
        if String.equal x r.dvar then evlambda r.dexp e' else e x
      in
      eval r.rbody e' c
  | Escp { escv; ebody } ->
      eval ebody (ext escv (FunVal (fun (a, _) -> c a)) e) c

and evcon k = Int k
and evlambda { fp; body } e = FunVal (fun (a, c) -> eval body (ext fp a e) c)

let init_env = function
  | "succ" -> FunVal (fun (a, c) -> c (succ a))
  | "equal" -> FunVal (fun (a, c) -> c (FunVal (fun (b, c') -> c' (equal a b))))
  | x -> raise (NameError x)

let interpret r = eval r init_env (fun a -> a)
