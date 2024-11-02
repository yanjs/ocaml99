let rev lst = let rec rev_aux prev = function
  | [] -> prev
  | (x :: xs) -> rev_aux (x :: prev) xs
in rev_aux [] lst

let () =
  assert (rev ["a"; "b"; "c"] = ["c"; "b"; "a"])