(* # slice ["a"; "b"; "c"; "d"; "e"; "f"; "g"; "h"; "i"; "j"] 2 6;;
- : string list = ["c"; "d"; "e"; "f"; "g"] *)

(* let slice ls i j = 
  List.filteri(fun index _ -> if index >= i && index <= j then true else false)ls;; *)

let slice ls i j = 
  let rec iteri l _i acc = 
    match l with
    | [] -> acc
    | x :: xs -> 
      if _i >= i && _i <= j then iteri(xs)(_i + 1)(x :: acc)
      else iteri(xs)(_i + 1)(acc)
    in
    List.rev @@ iteri ls 0 [];;