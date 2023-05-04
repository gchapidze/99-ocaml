(* #  decode [Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d"; Many (4, "e")];;
- : string list =
["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"] *)

let decode ls = 
  let rec append i e =
    if i = 0 then [] else e :: append(i - 1)(e)
  in
  let rec iter_and_decode l = 
    match l with
    | [] -> []
    | Many(i, e) :: xs ->  append(i)(e) @ iter_and_decode(xs) 
    | One e :: xs -> e :: iter_and_decode(xs)
  in
  iter_and_decode ls;;
