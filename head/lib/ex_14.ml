let rec duplicate ls = 
  match ls with
  | [] -> []
  | x :: xs -> x :: x :: duplicate(xs);;
