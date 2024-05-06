let _ =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  match arg with
  | "parallel" ->
      print_endline "Running parallel Qr_decomp Gram";
      Algos.Qr_decomp_gram.parallel ()
  | _ ->
      print_endline "Running normal Qr_decomp Gram";
      Algos.Qr_decomp_gram.normal ()
