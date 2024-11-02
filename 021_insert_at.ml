let insert_at x i lst = let rec split_aux c accum = function
  | [] -> (accum, [])
  | x :: xs as lst -> if c = 0
    then (accum, lst)
    else split_aux (c - 1) (x :: accum) xs
in let rec concat l1 l2 = match l1 with
    | [] -> l2
    | x :: xs -> concat xs (x :: l2)
in let a, b = split_aux i [] lst
in concat (x :: a) b

let () =
  assert (insert_at "alfa" 1 ["a"; "b"; "c"; "d"]
    = ["a"; "alfa"; "b"; "c"; "d"])