
let remove_at i ls = 
  let rec remove l acc index = 
    match l with
    | [] -> acc
    | x :: xs -> if index = i then List.rev acc @ xs else remove(xs)(x :: acc)(index + 1)
  in
  remove ls [] 0;;
