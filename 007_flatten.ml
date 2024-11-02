type 'a node =
  | One of 'a 
  | Many of 'a node list

let flatten lst = let rec flatten_aux met = function
  | [] -> met
  | One x :: xs -> flatten_aux (x :: met) xs
  | Many x :: xs -> flatten_aux (flatten_aux met x) xs
in List.rev (flatten_aux [] lst)

let () =
  assert (flatten [One "a"; Many [One "b"; Many [One "c" ;One "d"]; One "e"]]
    = ["a"; "b"; "c"; "d"; "e"])
