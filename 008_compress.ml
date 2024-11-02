let compress lst = let rec compress_aux met = function
  | [] -> met
  | x :: xs -> match met with
    | y :: ys -> if x = y then compress_aux met xs else compress_aux (x :: met) xs
    | [] -> compress_aux [x] xs
in List.rev (compress_aux [] lst)

let () =
  assert (compress ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
    = ["a"; "b"; "c"; "a"; "d"; "e"])
