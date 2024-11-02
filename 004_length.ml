let length lst = let rec length_aux n = function
  | [] -> n
  | x :: xs -> length_aux (n + 1) xs
in length_aux 0 lst

let () =
  assert (length ["a"; "b"; "c"] = 3);
  assert (length [] = 0)