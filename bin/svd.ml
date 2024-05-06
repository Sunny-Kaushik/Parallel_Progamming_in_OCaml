let _ =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  match arg with
  | "parallel" ->
      print_endline "Running parallel SVD";
      Algos.Svd.parallel ()
  | _ ->
      print_endline "Running normal SVD";
      Algos.Svd.normal ()
