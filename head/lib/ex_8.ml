let rec compress ls = 
  match ls with
  | [] -> []
  | [x] -> x :: []
  | x :: y :: xs -> if x = y then compress(y :: xs) else x :: compress(y :: xs);;
