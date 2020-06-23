open Expression

let () =
  let e = `Mult (`Num 3, `Add (`Num 1, `Mult (`Num 2, `Num 1))) in
  Format.printf "%a@." (Expr.pp ?level:None) e
