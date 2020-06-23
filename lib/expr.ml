type 'a t = [
  | `Var of string
  | `Num of int
  | `Add of 'a * 'a
  | `Mult of 'a * 'a
  | `Str of string
  | `Cat of 'a * 'a
]

(* TODO generate this automatically? *)
let map (f : _ -> 'a) : 'a t -> 'a = function
  | #Lambda.var as v -> v
  | `Num _ as n -> n
  | `Add (e, e') -> `Add (f e, f e')
  | `Mult (e, e') -> `Mult (f e, f e')
  | `Str _ as s -> s
  | `Cat (e, e') -> `Cat (f e, f e')

let eval eval_rec env (e : 'a t) : 'a =
  match map (eval_rec env) e with
  | #Lambda.var as v -> Lambda.eval_var env v
  | `Add (`Num m, `Num n) -> `Num (m + n)
  | `Mult (`Num m, `Num n) -> `Num (m * n)
  | `Cat (`Str s, `Str s') -> `Str (s ^ s')
  | e -> e

let rec eval2 env = eval eval2 env
let e2 = eval2 ["x", `Var "x"] (`Add (`Mult (`Num 3, `Num 2), `Var "x"))

let pp ppf e =
  let rec aux level ppf e =
    let bin op op_level e e' =
      if level < op_level then Format.fprintf ppf "(";
      let f = aux op_level in
      Format.fprintf ppf "%a%c%a" f e op f e';
      if level < op_level then Format.fprintf ppf ")"
    in
    match e with
    | `Var v -> Format.fprintf ppf "%s" v
    | `Num n -> Format.fprintf ppf "%d" n
    | `Add (e, e') -> bin '+' 5 e e'
    | `Mult (e, e') -> bin '*' 4 e e'
    | `Str s -> Format.fprintf ppf "%S" s
    | `Cat (e, e') -> bin '^' 5 e e'
  in
  aux 5 ppf e
