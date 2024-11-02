let rec group lst grps =
  if List.for_all (fun x -> x = 0) grps
    then [(List.map (fun _ -> []) grps)]
    else match lst with
      | [] -> []
      | x :: xs -> (
        let to_ith f i =
          fun j l -> if i = j then f l else l in
        let cons_to_ith = to_ith (fun l -> x :: l) in
        let sub1_to_ith = to_ith (fun l -> l - 1) in
        List.fold_left (@) []
        (List.mapi
          (fun i _ -> List.map
                            (fun m -> List.mapi (cons_to_ith i) m)
                            (group xs (List.mapi
                                        (sub1_to_ith i)
                                        grps)))
          grps)
      ) @ group xs grps

let () =
  let print_string_list lst =
    Printf.printf "[";
    List.iteri (fun i s ->
      if i > 0 then Printf.printf ", ";
      Printf.printf "\"%s\"" s
    ) lst;
    Printf.printf "]"
  in
  let print_string_list_list lst =
    Printf.printf "[";
    List.iteri (fun i sublist ->
      if i > 0 then Printf.printf "; ";
      print_string_list sublist
    ) lst;
    Printf.printf "]"
  in
  let print_string_list_list_list lst =
    Printf.printf "[";
    List.iteri (fun i subsublist ->
      if i > 0 then Printf.printf "; ";
      print_string_list_list subsublist
    ) lst;
    Printf.printf "]\n"
  in
  print_string_list_list_list (group ["a"; "b"; "c"; "d"] [2; 1])