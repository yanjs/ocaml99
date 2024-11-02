type 'a rle =
  | One of 'a
  | Many of int * 'a

let decode lst = let rec decode_aux result = function
  | [] -> result
  | (One x) :: xs -> decode_aux (x :: result) xs
  | (Many (0, x)) :: xs -> decode_aux result xs
  | (Many (c, x)) :: xs -> decode_aux (x :: result) ((Many (c - 1, x)) :: xs)
in List.rev (decode_aux [] lst)

let () =
  assert (decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")]
    = ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"])