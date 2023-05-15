
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
  let facs = List.sort(Int.compare)(helper n 2 [])
  in
  let rec append n acc = function
    | [] -> acc
    | x :: [] -> let (v, n) = List.hd acc in if v = x then (v, (n + 1)) :: List.tl acc
                                             else (x, 1) :: acc 
    | x :: xs -> if x = List.hd xs then append(n + 1)(acc)(xs)
                 else append(1)((x, n) :: acc)(xs)
  in
  List.rev @@ append 1 [] facs;;
