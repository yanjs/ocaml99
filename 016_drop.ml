let drop lst c = let rec drop_aux c rem accum = function
  | [] -> accum
  | x :: xs -> if rem = 0
      then drop_aux c (c - 1) accum xs
      else drop_aux c (rem - 1) (x :: accum) xs
in List.rev (drop_aux c (c - 1) [] lst)

let () =
  assert (drop ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3
    = ["a"; "b"; "d"; "e"; "g"; "h"; "j"])