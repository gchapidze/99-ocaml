let is_prime n =
    let n = max n (-n) in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
    in
      is_not_divisor 2;;

let goldbach n =
  if n mod 2 = 1 then failwith "Not even"
  else
  let rec find i =
    if is_prime i && is_prime (n - i) then (i, (n - i))
    else if i >= n then failwith "I am genius!"
    else
      find (succ i)
  in
  find 3;;
