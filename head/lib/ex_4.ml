
let length ls = 
  let rec count l c = 
    match l with
    | [] -> c
    | _ :: xs -> count(xs)(c + 1)
  in
count ls 0;;
