let _ =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  match arg with
  | "write_optimized" ->
      print_endline "Running write optimized mat_mul";
      Algos.Mat_mul.parallel_write_optimized ()
  | "parallel" ->
      print_endline "Running parallel mat_mul";
      Algos.Mat_mul.parallel ()
  | _ ->
      print_endline "Running normal mat_mul";
      Algos.Mat_mul.normal ()
