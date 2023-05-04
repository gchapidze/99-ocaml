(* # duplicate ["a"; "b"; "c"; "c"; "d"];;
- : string list = ["a"; "a"; "b"; "b"; "c"; "c"; "c"; "c"; "d"; "d"] *)

let rec duplicate ls = 
  match ls with
  | [] -> []
  | x :: xs -> x :: x :: duplicate(xs);;