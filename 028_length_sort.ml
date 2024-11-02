let rec quick_sort key = function
  | [] -> []
  | x :: xs -> quick_sort key (List.filter (fun y -> key y < key x) xs)
    @ [x]
    @ quick_sort key (List.filter (fun y -> key x <= key y) xs)

let length_sort lst = quick_sort List.length lst

let frequency_sort lst = let to_freq l =
  List.length (List.filter (fun ls -> List.length l = List.length ls) lst)
in quick_sort to_freq lst

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
    Printf.printf "]\n"
  in
  let lst = [["a"; "b"; "c"]; ["d"; "e"]; ["f"; "g"; "h"]; ["d"; "e"];
    ["i"; "j"; "k"; "l"]; ["m"; "n"]; ["o"]]
  in
  print_string_list_list (length_sort lst);
  print_string_list_list (frequency_sort lst)
