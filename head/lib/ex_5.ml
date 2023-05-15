
let rev ls =
  let rec reverse l acc = 
    match l with 
    | [] -> acc
    | x :: [] -> x :: acc
    | x :: xs -> reverse(xs)(x :: acc)
  in
  reverse ls [];;
