let rec last_two = function
  | x :: y :: [] -> Some (x, y)
  | x :: xs -> last_two xs
  | _ -> None

let () =
  assert (last_two ["a"; "b"; "c"; "d"] = Some ("c", "d"));
  assert (last_two ["a"] = None)