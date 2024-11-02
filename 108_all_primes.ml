let all_primes lo hi = let is_prime x =
  let rec verify n =
    if n * n > x then
      true
    else if x mod n = 0 then
      false
    else verify (n + 1)
  in if x = 1 then false else verify 2
  in let rec all_primes_aux lo hi accum =
    if lo > hi then
      accum
    else if is_prime lo then
      all_primes_aux (lo + 1) hi (lo :: accum)
    else
      all_primes_aux (lo + 1) hi accum
in all_primes_aux lo hi []

let () =
  assert (List.length (all_primes 2 7920) = 1000)