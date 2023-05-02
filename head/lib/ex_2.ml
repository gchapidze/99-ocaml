let rec last_two ls = 
  match ls with
  | [] -> None
  | x :: y :: [] -> Some (x :: y :: [])
  | _ :: xs -> last_two xs;;