type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode lst =
  let c_rle count x = if count = 1 then (One x) else (Many (count, x)) in
  let rec encode_aux prev count = function
  | [] -> []
  | [x] -> c_rle (count + 1) x :: prev
  | x :: x1 :: xs -> if x = x1
    then encode_aux prev (count + 1) (x1 :: xs)
    else encode_aux ((c_rle (count + 1) x) :: prev) 0 (x1 :: xs)
in List.rev (encode_aux [] 0 lst)

let () =
  assert (encode ["a";"a";"a";"a";"b";"c";"c";"a";"a";"d";"e";"e";"e";"e"]
    = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
    Many (4, "e")])