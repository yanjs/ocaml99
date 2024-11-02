let rec last = function
  | [] -> None
  | x :: [] -> Some x
  | x :: xs -> last xs

let () =
  assert (last ["a" ; "b" ; "c" ; "d"] = Some "d");
  assert (last [] = None)