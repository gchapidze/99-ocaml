let rec lotto_select len high = 
  if len <= 1 then []
  else Random.int high :: lotto_select(len - 1)(high);;
