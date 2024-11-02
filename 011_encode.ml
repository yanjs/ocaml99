type 'a rle =
  | One of 'a
  | Many of int * 'a

let encode lst = let rec encode_aux prev count = function
  | [] -> []
  | [x] -> if count = 0 then ((One x) :: prev) else ((Many (count + 1, x)) :: prev)
  | x :: x1 :: xs -> if x = x1
      then encode_aux prev (count + 1) (x1 :: xs)
      else if count = 0
        then encode_aux ((One x) :: prev) 0 (x1 :: xs)
        else encode_aux ((Many (count + 1, x)) :: prev) 0 (x1 :: xs)
in List.rev (encode_aux [] 0 lst)

let () =
  assert (encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"]
    = [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
    Many (4, "e")])