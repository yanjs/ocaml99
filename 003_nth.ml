
let rec nth lst n = match (lst, n) with
  | ((x :: xs), 0) -> Some x
  | ([], n) -> None
  | ((x :: xs) , n) -> nth xs (n - 1)

let () =
  assert (nth ["a"; "b"; "c"; "d"; "e"] 2 = Some "c");
  assert (nth ["a"] 2 = None)