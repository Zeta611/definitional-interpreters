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

and fun_val = value -> value
(** functions in the defined language is defined using OCaml functions *)

and env = var -> value
(** environments are higher-order *)

[@@@warning "+27"]

let succ a = match a with Int n -> Int (n + 1) | _ -> raise TypeError

let equal a b =
  match (a, b) with
  | Int n, Int m -> Bool (Int.equal n m)
  | Bool a, Bool b -> Bool (Bool.equal a b)
  | _ -> raise TypeError

(** environment is produced -> Simp *)
let ext z a e x = if String.equal x z then a else e x

let rec eval r e =
  match r with
  | Const k -> evcon k
  | Var x -> e x
  | Appl { opr; opnd } -> (
      match eval opr e with
      (* This is the only application.
         Note that the order of application in the defined language is call-by-value, as it is in OCaml *)
      | FunVal f -> f (eval opnd e)
      | _ -> raise TypeError)
  | Lambda l -> evlambda l e
  | Cond { prem; conc; altr } -> (
      match eval prem e with
      | Bool b -> if b then eval conc e else eval altr e
      | _ -> raise TypeError)
  | LetRec r ->
      let rec e' x =
        (* environment is produced -> Rec *)
        if String.equal x r.dvar then evlambda r.dexp e' else e x
      in
      eval r.rbody e'

and evcon k = Int k

(** lambda expression produced -> Closr *)
and evlambda { fp; body } e = FunVal (fun a -> eval body (ext fp a e))

(*** environment is produced -> Init *)
let init_env = function
  (* lambda expression produced -> Sc *)
  | "succ" -> FunVal (fun a -> succ a)
  (* lambda expression produced twice -> Eq1/2 *)
  | "equal" -> FunVal (fun a -> FunVal (fun b -> equal a b))
  | x -> raise (NameError x)

let interpret r = eval r init_env
