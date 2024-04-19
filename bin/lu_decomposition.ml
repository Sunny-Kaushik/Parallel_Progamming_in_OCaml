let _ =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  match arg with
  | "parallel" ->
      print_endline "Running parallel LU_Decomposition";
      Algos.Lu_decomposition.parallel ()
  | _ ->
      print_endline "Running normal LU_Decomposition";
      Algos.Lu_decomposition.normal ()
