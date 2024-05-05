let _ =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  match arg with
  | "parallel" ->
      print_endline "Running parallel imperative quicksort";
      Algos.Quicksort_Imperative.parallel ()
  | _ ->
      print_endline "Running normal imperative quicksort";
      Algos.Quicksort_Imperative.normal ()
