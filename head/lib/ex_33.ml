
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

let factors n =
  let rec helper num divisor rlist =
    if num = 1 then rlist
    else
      if (is_prime divisor) && (num mod divisor = 0) then
        helper(num / divisor)(2)(divisor :: rlist)
      else
        helper(num)(divisor + 1)(rlist)
  in
  List.sort(Int.compare)(helper n 2 []);;
