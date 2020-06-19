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

type 'a t = [
  | `Var of string
  | `Abs of string * 'a
  | `App of 'a * 'a
]

(* TODO compare evaluation strategies *)

let eval eval_rec env : 'a t -> 'a = function
  | #var as v -> eval_var env v
  | `Abs (x, e) ->
    let x' = gensym () in
    `Abs (x', eval_rec ((x, `Var x') :: env) e)
  | `App (f, e) ->
    let e' = eval_rec env e in
    match eval_rec env f with
    | `Abs (x, e) -> eval_rec [x, e'] e
    | f' -> `App (f', e')

let rec eval1 env = eval eval1 env
let e1 = eval1 ["y", `Var "y"] (`App (`Abs ("x", `Var "x"), `Var "y"))
