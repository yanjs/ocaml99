let duplicate lst = let rec duplicate_aux accum = function
  | [] -> accum
  | x :: xs -> duplicate_aux (x :: x :: accum) xs
in List.rev (duplicate_aux [] lst)

let () =
  assert (duplicate ["a"; "b"; "c"; "c"; "d"] = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"])