let rec slice lst i k = let rec take n accum = function
  | [] -> List.rev accum
  | x :: xs -> if n <= 0
    then List.rev accum
    else take (n - 1) (x :: accum) xs
in match (lst, i) with
  | ([], _) -> []
  | (x :: xs, 0) -> take (k + 1) [] lst
  | (x :: xs, i) -> slice xs (i - 1) (k - 1)

let () =
  assert (slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6
    = ["c"; "d"; "e"; "f"; "g"])