let _ =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  match arg with
  | "parallel" ->
      print_endline "Running parallel fibonacci";
      Algos.Fibonacci.parallel ()
  | _ ->
      print_endline "Running normal fibonacci";
      Algos.Fibonacci.normal ()
