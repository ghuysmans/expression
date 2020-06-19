type 'a t = [
  | `Var of string
  | `Abs of string * 'a
  | `App of 'a * 'a
  | `Num of int
  | `Add of 'a * 'a
  | `Mult of 'a * 'a
]

let eval eval_rec env : 'a t -> 'a = function
  | #Lambda.t as x -> Lambda.eval eval_rec env x
  | #Expr.t as x -> Expr.eval eval_rec env x

let rec eval3 env = eval eval3 env
let e3 = eval3 [] (`App (`Abs ("x", `Add (`Var "x", `Var "x")), `Num 2))
let e3' = eval3 ["double", `Abs ("x", `Add (`Var "x", `Var "x"))] (
  `App (`Var "double", `App (`Var "double", `Num 2))
)
