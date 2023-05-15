
exception Failure of string;;

let nth ls n =
  let rec iter counter l = 
    match l with
    | [] -> raise (Failure "List is empty")
    | [x] -> if n = 0 then x else raise (Failure "Index out of bounds")
    | x :: xs -> if counter = n then x else iter(counter + 1)(xs) 
  in
  iter(0)(ls);;
