let goldbach_list lo hi = let is_prime x =
  let rec verify n =
    if n * n > x then
      true
    else if x mod n = 0 then
      false
    else verify (n + 1)
  in if x = 1 then false else verify 2
  in let rec goldbach_aux x c =
    if is_prime c && is_prime (x - c) then
      (c, x - c)
    else
      goldbach_aux x (c + 1)
  in let rec even_range_aux lo hi accum =
    if lo > hi then
      accum
    else if hi mod 2 = 0 then
      even_range_aux lo (hi - 2) (hi :: accum)
    else
      even_range_aux lo (hi - 1) accum
in List.map (fun e -> (e, goldbach_aux e 2)) (even_range_aux lo hi [])

let () =
  assert (goldbach_list 9 20 = [(10, (3, 7)); (12, (5, 7)); (14, (3, 11)); (16, (3, 13)); (18, (5, 13));
    (20, (3, 17))]);
  Printf.printf "%d cases that primes > 50 in 2..3000\n"
    (List.length (List.filter
                  (fun (e, (p1, p2)) -> p1 > 50)
                  (goldbach_list 2 3000)))