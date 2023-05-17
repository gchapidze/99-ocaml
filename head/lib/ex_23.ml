let rand_select (ls: 'a list) (len: int): 'a list = 
  let min_bound = Random.int (List.length ls) - len
in
let max_bound = min_bound + len 
in
  let rec append l min = 
    match l with
    | [] -> []
    | x :: xs -> 
      if min < max_bound then x :: append(xs)(min + 1)
      else []
    in
    if len < 0 || len >= List.length ls then [] else append(ls)(min_bound);;
