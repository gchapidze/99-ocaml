let palindrome ls =
  let ll = List.length ls - 1 in 
  let rec accumulate l c =
    match l with
    | [] -> true
    | x :: xs -> 
      if x <> List.nth(ls)(ll - c) then false 
      else if c = (ll / 2) then true
      else accumulate(xs)(c + 1)
  in
  accumulate ls 0;;