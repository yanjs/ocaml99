let phi m = let rec coprime x y =
  if x = 0 then
    y = 1
  else
    coprime (y mod x) x
  in let rec phi_aux curr accum m =
    if curr >= m then
      accum
    else if coprime curr m then
      phi_aux (curr + 1) (accum + 1) m
    else
      phi_aux (curr + 1) accum m
in if m = 1 then
  1
else
  phi_aux 1 0 m

let () =
  assert (phi 10 = 4)