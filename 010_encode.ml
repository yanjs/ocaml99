let encode lst = let rec encode_aux prev curr lst = match (curr, lst) with
  | (None, []) -> []
  | (None, x :: xs) -> encode_aux prev (Some (1, x)) xs
  | (Some (cy, y), []) -> (cy, y) :: prev
  | (Some (cy, y), x :: xs) -> if x = y
      then encode_aux prev (Some (cy + 1, y)) xs
      else encode_aux ((cy, y) :: prev) (Some (1, x)) xs
in List.rev (encode_aux [] None lst)

let () =
  assert (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
    = [(4, "a"); (1, "b"); (2, "c"); (2, "a"); (1, "d"); (4, "e")])