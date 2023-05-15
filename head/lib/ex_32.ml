
let coprime x y =
  let rec gcd x y =
    if x > y then gcd(x - y)(y)
    else if y > x then gcd(x)(y - x)
    else x
  in
  if (gcd x y) = 1 then true
  else
    false;;

let phi n =
  let s = 1 in
  let rec helper num c =
    if num >= n then c
    else
      if coprime n num then helper(num + 1)(c + 1)
      else helper(num + 1)(c)
  in
  helper s 0;;
