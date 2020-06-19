type 'a t = [
  | `Var of string
  | `Num of int
  | `Add of 'a * 'a
  | `Mult of 'a * 'a
]

(* TODO generate this automatically? *)
let map (f : _ -> 'a) : 'a t -> 'a = function
  | #Lambda.var as v -> v
  | `Num _ as n -> n
  | `Add (e, e') -> `Add (f e, f e')
  | `Mult (e, e') -> `Mult (f e, f e')

let eval eval_rec env (e : 'a t) : 'a =
  match map (eval_rec env) e with
  | #Lambda.var as v -> Lambda.eval_var env v
  | `Add (`Num m, `Num n) -> `Num (m + n)
  | `Mult (`Num m, `Num n) -> `Num (m * n)
  | e -> e

let rec eval2 env = eval eval2 env
let e2 = eval2 ["x", `Var "x"] (`Add (`Mult (`Num 3, `Num 2), `Var "x"))
