let _ =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  match arg with
  | "parallel" ->
      print_endline "Running parallel nbody";
      Algos.NBody_Parallel.main ()
  | _ ->
      print_endline "Running normal nbody";
      Algos.NBody_Sequential.main ()
