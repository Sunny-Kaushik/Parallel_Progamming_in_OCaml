let _ =
  let arg = if Array.length Sys.argv > 1 then Sys.argv.(1) else "" in
  match arg with
  | "parallel" ->
      print_endline "Running parallel height of binary tree";
      Algos.Btree_Height.parallel ()
  | _ ->
      print_endline "Running normal height of binary tree";
      Algos.Btree_Height.normal ()
