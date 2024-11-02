let permutation lst =
  Random.init 0;
  let rec split_aux c accum = function
    | [] -> (accum, [])
    | x :: xs as lst -> if c = 0
      then (accum, lst)
      else split_aux (c - 1) (x :: accum) xs
  in let rec concat l1 l2 = match l1 with
    | [] -> l2
    | x :: xs -> concat xs (x :: l2)
  in let rec rand_select_aux lst c accum =
    if c = 0 then accum else
      let a, b = split_aux (Random.int (List.length lst)) [] lst
      in match b with
        | [] -> raise (Failure "Should not happen")
        | (x :: xs) -> let new_lst = concat a xs
                      in rand_select_aux new_lst (c - 1) (x :: accum)
in rand_select_aux lst (List.length lst) []

let () =
  let print_string_list lst =
    List.iter (Printf.printf "%s ") lst;
    print_newline ()
  in
  let result = permutation ["a"; "b"; "c"; "d"; "e"; "f"]
  in print_string_list result