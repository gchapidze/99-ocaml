let length_sort list =
  List.sort(fun ls1 ls2 -> Int.compare (List.length ls1)(List.length ls2))list;;

let frequency_sort list =
  let mapped () = List.map(
                      fun ls -> (ls, List.fold_left(fun acc l ->
                                        if List.length l = List.length ls then acc + 1 else acc) 0 list))
                    list in
  let sorted () = List.sort(fun (_, v1) (_, v2) -> Int.compare v1 v2) @@ mapped () in
  List.map(fun (ls, _) -> ls) @@ sorted ();;
