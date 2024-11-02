let replicate lst c =
  let rec dupe c x accum =
    if c = 0 then accum else dupe (c - 1) x (x :: accum)
  in
    let rec replicate_aux accum c = function
      | [] -> accum
      | x :: xs -> replicate_aux (dupe c x accum) c xs
    in List.rev (replicate_aux [] c lst)

let () =
  assert (replicate ["a"; "b"; "c"] 3 = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"])