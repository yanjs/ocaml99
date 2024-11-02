let pack lst = let rec pack_aux prev curr = function
  | [] -> if curr = [] then [] else (curr :: prev)
  | x :: xs -> match curr with
    | [] -> pack_aux prev [x] xs
    | y :: ys -> if x = y
      then pack_aux prev (x :: curr) xs
      else pack_aux (curr :: prev) [x] xs
in List.rev (pack_aux [] [] lst)

let () =
  assert (pack ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "d"; "e"; "e"; "e"; "e"]
    = [["a"; "a"; "a"; "a"]; ["b"]; ["c"; "c"]; ["a"; "a"]; ["d"; "d"];
    ["e"; "e"; "e"; "e"]])