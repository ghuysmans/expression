(* adapted from "Code reuse through polymorphic variants" by Jacques Garrigue *)

let gensym =
  let n = ref 0 in
  fun () ->
    incr n;
    "_" ^ string_of_int !n

type var = [`Var of string]

exception Unbound of string

let eval_var env (`Var x : var) =
  try
    List.assoc x env
  with Not_found ->
    raise (Unbound x)

type 'a lambda = [
  | `Var of string
  | `Abs of string * 'a
  | `App of 'a * 'a
]

(* TODO compare evaluation strategies *)

let eval_lambda eval_rec env : 'a lambda -> 'a = function
  | #var as v -> eval_var env v
  | `Abs (x, e) ->
    let x' = gensym () in
    `Abs (x', eval_rec ((x, `Var x') :: env) e)
  | `App (f, e) ->
    let e' = eval_rec env e in
    match eval_rec env f with
    | `Abs (x, e) -> eval_rec [x, e'] e
    | f' -> `App (f', e')

let rec eval1 env = eval_lambda eval1 env
let e1 = eval1 ["y", `Var "y"] (`App (`Abs ("x", `Var "x"), `Var "y"))

type 'a expr = [
  | `Var of string
  | `Num of int
  | `Add of 'a * 'a
  | `Mult of 'a * 'a
]

(* FIXME automatic *)
let map_expr (f : _ -> 'a) : 'a expr -> 'a = function
  | #var as v -> v
  | `Num _ as n -> n
  | `Add (e, e') -> `Add (f e, f e')
  | `Mult (e, e') -> `Mult (f e, f e')

let eval_expr eval_rec env (e : 'a expr) : 'a =
  match map_expr (eval_rec env) e with
  | #var as v -> eval_var env v
  | `Add (`Num m, `Num n) -> `Num (m + n)
  | `Mult (`Num m, `Num n) -> `Num (m * n)
  | e -> e

let rec eval2 env = eval_expr eval2 env
let e2 = eval2 ["x", `Var "x"] (`Add (`Mult (`Num 3, `Num 2), `Var "x"))

type 'a lexpr = [
  | `Var of string
  | `Abs of string * 'a
  | `App of 'a * 'a
  | `Num of int
  | `Add of 'a * 'a
  | `Mult of 'a * 'a
]

let eval_lexpr eval_rec env : 'a lexpr -> 'a = function
  | #lambda as x -> eval_lambda eval_rec env x
  | #expr as x -> eval_expr eval_rec env x

let rec eval3 env = eval_lexpr eval3 env
let e3 = eval3 [] (`App (`Abs ("x", `Add (`Var "x", `Var "x")), `Num 2))
let e3' = eval3 ["double", `Abs ("x", `Add (`Var "x", `Var "x"))] (
  `App (`Var "double", `App (`Var "double", `Num 2))
)
