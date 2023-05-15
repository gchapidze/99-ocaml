
let range i j = 
    let rec append a b =  
      if a > b then []
      else a :: append(a + 1)(b)
    in
    if i > j then List.rev (append j i) else append i j;;  
