let factors x = let rec verify p mult accum rem =
  if rem = 1 then
    ((p, mult) :: accum)
  else if rem mod p = 0 then
    verify p (mult + 1) accum (rem / p)
  else if mult > 0 then
    verify (p + 1) 0 ((p, mult) :: accum) rem
  else
    verify (p + 1) 0 accum rem
in List.rev (verify 2 0 [] x)

let () =
  assert (factors 315 = [(3, 2); (5, 1); (7, 1)])