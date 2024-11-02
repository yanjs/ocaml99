let is_prime x = let rec verify n =
  if n * n > x then
    true
  else if x mod n = 0 then
    false
  else verify (n + 1)
in if x = 1 then false else verify 2

let () =
  assert (is_prime 1 = false);
  assert (is_prime 7 = true);
  assert (is_prime 12 = false);