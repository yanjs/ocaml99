let remove_at i lst = let rec concat l1 l2 = match l1 with
    | [] -> l2
    | x :: xs -> concat xs (x :: l2)
in let rec remove_at_aux i accum = function
  | [] -> accum
  | x :: xs -> if i = 0
    then concat accum xs
    else remove_at_aux (i - 1) (x :: accum) xs
  in remove_at_aux i [] lst

let () =
  assert (remove_at 1 ["a"; "b"; "c"; "d"]
    = ["a"; "c"; "d"])