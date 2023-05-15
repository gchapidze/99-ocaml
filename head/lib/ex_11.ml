type 'a rle =
  | One of 'a
  | Many of int * 'a;;

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
