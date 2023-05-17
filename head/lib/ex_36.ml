let timeit f arg =
  let s = Sys.time () in
  f arg;
  let e = Sys.time () -. s in
  e;;
