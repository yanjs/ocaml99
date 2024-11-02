let rotate lst n = let rec concat l1 l2 = match l1 with
  | [] -> l2
  | x :: xs -> concat xs (x :: l2)
  in let rec split_aux c accum = function
  | [] -> (accum, [])
  | x :: xs as lst -> if c = 0
    then (accum, lst)
    else split_aux (c - 1) (x :: accum) xs
in let a, b = split_aux (n mod (List.length lst)) [] lst
in concat (List.rev b) (List.rev a)

let () =
  assert (rotate ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"] 3
    = ["d"; "e"; "f"; "g"; "h"; "a"; "b"; "c"])