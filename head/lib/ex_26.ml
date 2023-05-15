
let res = ref [];;

let rec extract n ls =
  let hd =
    let upper = n in
    let rec helper c l =
      if c < 1 then []
      else List.hd l :: helper(c - 1)(List.tl l)
    in
    helper(upper - 1)(ls)
  in
  let rec append j =
    if j < (List.length ls) then
      (
        res := (hd @ [List.nth ls j]) :: !res; append(j + 1)
      )
    else if (List.length ls < n) then ignore(res := List.rev !res)
    else
      extract(n)(List.tl ls)
  in
  append (n - 1);;
