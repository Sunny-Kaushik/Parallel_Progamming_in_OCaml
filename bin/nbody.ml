let _ =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  match arg with
  | "write_optimized" ->
      print_endline "Running write optimized nbody";
      Algos.NBody_Write_Optimized.main ()
  | "parallel" ->
      print_endline "Running parallel nbody";
      Algos.NBody_Parallel.main ()
  | _ ->
      print_endline "Running normal nbody";
      Algos.NBody_Sequential.main ()
