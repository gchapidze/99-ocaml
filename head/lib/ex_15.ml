(* # replicate ["a"; "b"; "c"] 3;;
- : string list = ["a"; "a"; "a"; "b"; "b"; "b"; "c"; "c"; "c"] *)

let rec replicate ls i =
  let rec append_helper c e = 
    if c = 0 then [] else e :: append_helper(c - 1)(e) in
  let rec append l = 
    match l with
    | [] -> []
    | x :: xs -> append_helper(i)(x) @ append xs in
    append ls;;