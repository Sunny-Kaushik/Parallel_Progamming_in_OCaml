let _ =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  match arg with
  | "parallel" ->
      print_endline "Running parallel functional mergesort";
      Algos.Mergesort_Functional.parallel ()
  | _ ->
      print_endline "Running normal functional mergesort";
      Algos.Mergesort_Functional.normal ()
