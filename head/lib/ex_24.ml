(* # lotto_select 6 49;;
- : int list = [20; 28; 45; 16; 24; 38] *)

let rec lotto_select len high = 
  if len <= 1 then []
  else Random.int high :: lotto_select(len - 1)(high);;