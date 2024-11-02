let goldbach x = let is_prime x =
  let rec verify n =
    if n * n > x then
      true
    else if x mod n = 0 then
      false
    else verify (n + 1)
  in if x = 1 then false else verify 2
  in let rec goldbach_aux c =
    if is_prime c && is_prime (x - c) then
      (c, x - c)
    else
      goldbach_aux (c + 1)
in goldbach_aux x 2

let () =
  assert (goldbach 28 = (5, 23))