let rec extract n lst = match (n, lst) with
  | (0, _) -> [[]]
  | (n, []) -> []
  | (n, x :: xs) -> (List.map (fun ls -> x::ls) (extract (n - 1) xs)) @ extract n xs

let () =
  assert (extract 2 ["a"; "b"; "c"; "d"]
    = [["a"; "b"]; ["a"; "c"]; ["a"; "d"]; ["b"; "c"]; ["b"; "d"]; ["c"; "d"]])