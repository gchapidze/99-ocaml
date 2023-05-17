let encode ls = 
  let rec encoder l (c, _) = 
    match l with
    | [] -> []
    | [x] -> (c + 1, x) :: []
    | x :: y :: xs -> 
      if x = y then encoder(y :: xs)(c + 1, x) 
      else (c + 1, x) :: encoder(y :: xs)(0, "")
  in
  encoder ls (0, "");;
