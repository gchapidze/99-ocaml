let is_prime n =
    let n = max n (-n) in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
    in
      is_not_divisor 2;;

let goldbach n =
  if n mod 2 = 1 then raise (invalid_arg "not even") 
  else
  let rec find i =
    if is_prime i && is_prime (n - i) then (i, (n - i))
    else if i >= n then failwith "I am genius!"
    else
      find (succ i)
  in
  find 3;;

let goldbach_list l h =
  let rec cons low high acc =
    if low > high then acc
    else
    try
      let (f, s) = goldbach low in
      cons (low + 1) high ((low, (f, s)) :: acc)
    with
      invalid_arg -> cons(low + 1)(high)(acc) in
  List.rev @@ cons l h [];;
