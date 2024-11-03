type bool_expr =
  | Var of string
  | Not of bool_expr
  | And of bool_expr * bool_expr
  | Or of bool_expr * bool_expr

let table2 v1 v2 exp = let rec table2_aux = function
  | Var v -> if v = v1 then (true, true, false, false) else (true, false, true, false)
  | Not ex -> let tt, tf, ft, ff = table2_aux ex in
      (not tt, not tf, not ft, not ff)
  | And (ex1, ex2) ->
    let tt1, tf1, ft1, ff1 = table2_aux ex1 in
    let tt2, tf2, ft2, ff2 = table2_aux ex2 in
      (tt1 && tt2, tf1 && tf2, ft1 && ft2, ff1 && ff2)
  | Or (ex1, ex2) ->
    let tt1, tf1, ft1, ff1 = table2_aux ex1 in
    let tt2, tf2, ft2, ff2 = table2_aux ex2 in
      (tt1 || tt2, tf1 || tf2, ft1 || ft2, ff1 || ff2)
in let tt, tf, ft, ff = table2_aux exp
in [(true, true, tt); (true, false, tf); (false, true, ft); (false, false, ff)]

let () =
  assert (table2 "a" "b" (And (Var "a", Or (Var "a", Var "b")))
    = [(true, true, true); (true, false, true); (false, true, false);
    (false, false, false)])