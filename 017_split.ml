let split lst c = let rec split_aux c accum = function
  | [] -> (List.rev accum, [])
  | x :: xs as lst -> if c = 0
    then (List.rev accum, lst)
    else split_aux (c - 1) (x :: accum) xs
in split_aux c [] lst

let () =
  assert (split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3
    = (["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"]));
  assert (split ["a"; "b"; "c"; "d"] 5
    = (["a"; "b"; "c"; "d"], []))