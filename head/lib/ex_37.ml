let is_prime n =
    let n = max n (-n) in
    let rec is_not_divisor d =
      d * d > n || (n mod d <> 0 && is_not_divisor (d + 1))
    in
      is_not_divisor 2;;

let rec all_primes l h =
  let rec accumulate low high acc = 
    if is_prime low then accumulate (low + 1) high (low :: acc)
    else if low > high then acc
    else accumulate(low + 1) h acc
  in
  List.rev @@ accumulate l h [];;

all_primes 2 50;;
