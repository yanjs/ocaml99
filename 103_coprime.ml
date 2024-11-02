let rec coprime x y =
  if x = 0 then
    y = 1
  else
    coprime (y mod x) x

let () =
  assert (coprime 13 27);
  assert (coprime 20536 7826 = false)