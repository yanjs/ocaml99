let timeit f a =
  let t_start = Unix.gettimeofday() in
  f a;
  let t_end = Unix.gettimeofday() in
  t_end -. t_start

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

let phi_improved m = let rec verify p mult accum rem =
  if rem = 1 then
    ((p, mult) :: accum)
  else if rem mod p = 0 then
    verify p (mult + 1) accum (rem / p)
  else if mult > 0 then
    verify (p + 1) 0 ((p, mult) :: accum) rem
  else
    verify (p + 1) 0 accum rem
in let factors = List.rev (verify 2 0 [] m)
in let rec pow a x =
  if x = 0 then
    1
  else if x mod 2 = 0 then
    let h = pow a (x / 2) in h * h
  else
    a * pow a (x - 1)
in List.fold_left (fun a (p, mult) -> a * (p - 1) * (pow p (mult - 1))) 1 factors

let () =
  let time_elapsed1 = timeit phi 10090 in
  Printf.printf "timeit phi 10090\nTime: %fs \n" time_elapsed1;
  let time_elapsed2 = timeit phi_improved 10090 in
  Printf.printf "timeit phi_improved 10090\nTime: %fs \n" time_elapsed2;
