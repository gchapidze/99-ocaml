(* # insert_at "alfa" 1 ["a"; "b"; "c"; "d"];;
- : string list = ["a"; "alfa"; "b"; "c"; "d"] *)

let insert_at ~(node: 'a) ~(index: int) ~(list: 'a list) = 
  let rec iteri l i = 
    match l with
    | [] -> []
    | x :: xs -> if i = index then node :: x :: iteri(xs)(i + 1) else x :: iteri(xs)(i + 1)
  in
  iteri list 0;;
