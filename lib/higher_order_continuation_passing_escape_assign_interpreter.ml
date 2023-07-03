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
and value = Int of int | Bool of bool | FunVal of fun_val | Ref of ref
[@@deriving show]

and fun_val = value * mem * cont -> value
(** functions in the defined language is defined using OCaml functions *)

and env = var -> value
(** environments are higher-order *)

and cont = mem * value -> value
and ref = { number : int }
and mem = { count : int; possess : int -> value }

[@@@warning "+27"]

let succ a = match a with Int n -> Int (n + 1) | _ -> raise TypeError

let equal a b =
  match (a, b) with
  | Int n, Int m -> Bool (Int.equal n m)
  | Bool a, Bool b -> Bool (Bool.equal a b)
  | _ -> raise TypeError

let ext z a e x = if String.equal x z then a else e x
let init_mem = { count = 0; possess = (fun _ -> Int 0) }
let nextref m = Ref { number = m.count + 1 }

let augment m a =
  {
    count = m.count + 1;
    possess = (fun n -> if n = m.count + 1 then a else m.possess n);
  }

let update m rf a =
  { m with possess = (fun n -> if n = rf.number then a else m.possess n) }

let lookup m rf = m.possess rf.number

let rec eval r e m c =
  match r with
  | Const k -> c (m, evcon k)
  | Var x -> c (m, e x)
  | Appl { opr; opnd } ->
      eval opr e m (function
        | m', FunVal f -> eval opnd e m' (fun (m'', a) -> f (a, m'', c))
        | _ -> raise TypeError)
  | Lambda l -> c (m, evlambda l e)
  | Cond { prem; conc; altr } ->
      eval prem e m (function
        | m', Bool b -> if b then eval conc e m' c else eval altr e m' c
        | _ -> raise TypeError)
  | LetRec r ->
      let rec e' x =
        (* environment is produced -> Rec *)
        if String.equal x r.dvar then evlambda r.dexp e' else e x
      in
      eval r.rbody e' m c
  | Escp { escv; ebody } ->
      eval ebody (ext escv (FunVal (fun (a, m', _) -> c (m', a))) e) m c

and evcon k = Int k

and evlambda { fp; body } e =
  FunVal (fun (a, m, c) -> eval body (ext fp a e) m c)

let init_env = function
  | "succ" -> FunVal (fun (a, m, c) -> c (m, succ a))
  | "equal" ->
      FunVal
        (fun (a, m, c) -> c (m, FunVal (fun (b, m', c') -> c' (m', equal a b))))
  | "ref" -> FunVal (fun (a, m, c) -> c (augment m a, nextref m))
  | "set" ->
      FunVal
        (fun (rf, m, c) ->
          match rf with
          | Ref rf -> c (m, FunVal (fun (a, m', c') -> c' (update m' rf a, a)))
          | _ -> raise TypeError)
  | "val" ->
      FunVal
        (fun (rf, m, c) ->
          match rf with Ref rf -> c (m, lookup m rf) | _ -> raise TypeError)
  | x -> raise (NameError x)

let interpret r = eval r init_env init_mem (fun (_, a) -> a)
