let coprime x y =
  let rec gcd x y =
    if x > y then gcd(x - y)(y)
    else if y > x then gcd(x)(y - x)
    else x
  in
  if (gcd x y) = 1 then true
  else
    false;;
