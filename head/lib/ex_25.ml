
let remove_at ls i = 
  let rec remover l acc j = 
    match l with
    | [] -> acc
    | x :: xs -> if i = j then remover(xs)(acc)(j + 1) else remover(xs)(x :: acc)(j + 1)
  in
  List.rev @@ remover ls [] 0;;


let permutation ls = 
  let rec generate l acc = 
    if List.length l = 0 then acc 
    else 
      let index = Random.int(List.length l) in 
      generate(remove_at l index)(List.nth l index :: acc)
  in
  generate ls [];; 
