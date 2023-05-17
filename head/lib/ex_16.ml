let drop ls i = 
  let rec drop_helper l c =
    match l with
    | [] -> []
    | x :: xs -> 
      if c mod i <> 0 then x :: drop_helper(xs)(c + 1) 
      else drop_helper(xs)(c + 1) in
      drop_helper(ls)(1);;
