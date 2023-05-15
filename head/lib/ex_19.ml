
let rotate ls i = 
  let rec rotator_helper l acc index = 
    match l with
    | [] -> acc
    | x :: xs -> 
      if index < i then rotator_helper(xs)(x :: acc)(index + 1)
      else l @ List.rev acc
    in
    rotator_helper ls [] 0;;
