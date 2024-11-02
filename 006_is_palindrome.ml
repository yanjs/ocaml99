let is_palindrome lst = lst = List.rev lst

let () =
  assert (is_palindrome ["x"; "a"; "m"; "a"; "x"] = true);
  assert (is_palindrome ["a"; "b"] = false)