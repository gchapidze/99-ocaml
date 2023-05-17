let pack ls = 
  let rec iter_and_pack l acc_i acc_j = 
    match l with
    | [] -> acc_j
    | x :: [] -> (x :: acc_i) :: acc_j 
    | x :: y :: xs -> 
    if x = y then iter_and_pack(y :: xs)(x :: acc_i)(acc_j)
    else iter_and_pack(y :: xs)([])((x :: acc_i) :: acc_j)
  in
  List.rev @@ iter_and_pack(ls)([])([]);;
