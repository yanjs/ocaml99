let rec gcd x y =
  if x = 0 then
    y
  else
    gcd (y mod x) x

let () =
  assert (gcd 13 27 = 1);
  assert (gcd 20536 7826 = 2)