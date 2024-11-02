let range i j = let rec range_aux i j accum =
  if i > j then accum else range_aux (i + 1) j (i :: accum)
in List.rev (range_aux i j [])

let () =
  assert (range 4 9 = [4; 5; 6; 7; 8; 9])