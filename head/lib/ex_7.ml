type 'a node =
  | One of 'a 
  | Many of 'a node list;;

(* [One "a"; Many [One "b"; Many [One "c"; One "d"]; One "e"]] *)
let flatten ls = 
  let rec flat l acc = 
    match l with
    | [] -> acc
    | One x :: xs -> flat(xs)(x :: acc) 
    | Many x :: xs -> flat(xs)(flat(x)(acc))
  in
  List.rev @@ flat(ls)([]);;