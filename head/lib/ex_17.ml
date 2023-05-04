(* # split ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 3;;
- : string list * string list =
(["a"; "b"; "c"], ["d"; "e"; "f"; "g"; "h"; "i"; "j"])
# split ["a"; "b"; "c"; "d"] 5;;
- : string list * string list = (["a"; "b"; "c"; "d"], []) *)

let split ls i = 
  let rec splitter l first second c = 
    match l with
    | [] -> List.rev first :: List.rev second :: []
    | x :: xs -> 
      if c < i then splitter(xs)(x :: first)(second)(c + 1)
      else splitter(xs)(first)(x :: second)(c + 1)
  in
  splitter ls [] [] 0;;