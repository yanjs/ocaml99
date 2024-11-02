let rec lotto_select n m =
  Random.init 0;
  match n with
    | 0 -> []
    | n -> (Random.int m + 1) :: lotto_select (n - 1) m


let () =
  let print_string_list lst =
    List.iter (Printf.printf "%d ") lst; print_newline ()
  in
  let result = lotto_select 6 49
  in print_string_list result