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
  assert (phi_improved 10 = 4);
  assert (phi_improved 13 = 12)