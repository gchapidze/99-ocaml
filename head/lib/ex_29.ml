
let is_prime n =
  let s = 2 in
  let n = if n < 0 then Int.neg n
          else n in
  let rec helper s =
    if s < n then if n mod s = 0 then false
                  else helper(s + 1)
    else
      true
  in
  helper s;;
                   
