open Domainslib

let get_determinant_bareiss_normal vect =
  let dim = Array.length vect in
  if dim <= 0 then 0.0
  else
    let sign = ref 1.0 in
    let rec swap_rows m k =
      if m = dim then ()
      else if vect.(m).(k) <> 0. then (
        let temp = vect.(m) in
        vect.(m) <- vect.(k);
        vect.(k) <- temp;
        sign := -. !sign)
      else swap_rows (m + 1) k
    in
    try
      for k = 0 to dim - 2 do
        if vect.(k).(k) = 0. then (
          let m = ref (k + 1) in
          swap_rows !m k;
          if !m = dim then
            (* No entries != 0 found in column k -> det = 0 *)
            raise Exit);
        for i = k + 1 to dim - 1 do
          for j = k + 1 to dim - 1 do
            vect.(i).(j) <-
              (vect.(k).(k) *. vect.(i).(j)) -. (vect.(i).(k) *. vect.(k).(j));
            if k <> 0 then vect.(i).(j) <- vect.(i).(j) /. vect.(k - 1).(k - 1)
          done
        done
      done;
      !sign *. vect.(dim - 1).(dim - 1)
    with Exit -> 0.0

let get_determinant_bareiss_parallel (pool : Task.pool) vect =
  let dim = Array.length vect in
  if dim <= 0 then 0.0
  else
    let sign = ref 1.0 in
    let rec swap_rows m k =
      if m = dim then ()
      else if vect.(m).(k) <> 0. then (
        let temp = vect.(m) in
        vect.(m) <- vect.(k);
        vect.(k) <- temp;
        sign := -. !sign)
      else swap_rows (m + 1) k
    in
    Task.parallel_for pool ~start:0 ~finish:(dim - 2) ~body:(fun k ->
        if vect.(k).(k) = 0. then (
          let m = ref (k + 1) in
          swap_rows !m k;
          if !m = dim then
            (* No entries != 0 found in column k -> det = 0 *)
            raise Exit);
        for i = k + 1 to dim - 1 do
          for j = k + 1 to dim - 1 do
            vect.(i).(j) <-
              (vect.(k).(k) *. vect.(i).(j)) -. (vect.(i).(k) *. vect.(k).(j));
            if k <> 0 then vect.(i).(j) <- vect.(i).(j) /. vect.(k - 1).(k - 1)
          done
        done);
    try !sign *. vect.(dim - 1).(dim - 1) with Exit -> 0.0

let generate_random_matrix size =
  let m = Array.make_matrix size size 0. in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      m.(i).(j) <- Random.float 0.2
    done
  done;
  m

let size = 1024

let normal matrix =
  (* let matrix = generate_random_matrix size in *)
  let det = get_determinant_bareiss_normal matrix in
  Printf.printf "Determinant: %f\n" det;
  ()

let parallel matrix =
  let n_domains = 8 in
  (* let matrix = generate_random_matrix size in *)
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let det =
    Task.run pool (fun () -> get_determinant_bareiss_parallel pool matrix)
  in
  Task.teardown_pool pool;
  Printf.printf "Determinant: %f\n" det;
  ()

let main () =
  let matrix = generate_random_matrix size in
  let matrix_copy = Array.map Array.copy matrix in
  normal matrix;
  parallel matrix_copy
