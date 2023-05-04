type 'a rle =
  | One of 'a
  | Many of int * 'a;;

(* # encode ["a"; "a"; "a"; "a"; "b"; "c"; "c"; "a"; "a"; "d"; "e"; "e"; "e"; "e"];;
- : string rle list =
[Many (4, "a"); One "b"; Many (2, "c"); Many (2, "a"); One "d";
 Many (4, "e")] *)

let encode ls = 
  let rec mod_encode l acc_i acc_j = 
    match l with
    | [] -> acc_j
    | [x] -> 
      (if List.length acc_i = 0 then One x :: acc_j else Many(List.length acc_i + 1, x) :: acc_j)
    | x :: y :: xs ->
      if x = y then mod_encode(y :: xs)(x :: acc_i)(acc_j)
      else mod_encode(y :: xs)([])
      (if List.length acc_i = 0 then One x :: acc_j else Many(List.length acc_i + 1, x) :: acc_j)
    in
    mod_encode(ls)([])([]);;
