open Domainslib

let generate_random_matrix size =
  let m = Array.make_matrix size size 0.0 in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      m.(i).(j) <-
        Random.float 10.0 (* Generate random floats between 0.0 and 1000.0 *)
    done
  done;
  m

let print_float_value f =
  print_float f;
  print_newline ()

let swap arr i j =
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp

(* Perform LU decomposition with partial pivoting *)
let lu_decomposition_with_pivoting matrix =
  let n = Array.length matrix in
  let lu = Array.copy matrix in
  let p = Array.init n (fun i -> i) in
  let swaps = ref 0 in
  for k = 0 to n - 2 do
    let max_index = ref k in
    let max_value = ref (abs_float lu.(k).(k)) in
    for i = k + 1 to n - 1 do
      let abs_val = abs_float lu.(i).(k) in
      if abs_val > !max_value then (
        max_index := i;
        max_value := abs_val)
    done;
    if !max_index <> k then (
      swap lu !max_index k;
      swap p !max_index k;
      swaps := !swaps + 1);
    for i = k + 1 to n - 1 do
      let factor = lu.(i).(k) /. lu.(k).(k) in
      lu.(i).(k) <- factor;
      for j = k + 1 to n - 1 do
        lu.(i).(j) <- lu.(i).(j) -. (factor *. lu.(k).(j))
      done
    done
  done;
  (lu, !swaps)

(* Calculate determinant from LU decomposition *)
let determinant_from_lu lu swaps =
  let n = Array.length lu in
  let det_sign = if swaps mod 2 = 0 then 1.0 else -1.0 in
  let det = ref det_sign in
  for i = 0 to n - 1 do
    det := !det *. lu.(i).(i)
  done;
  !det

let determinant_lu_pivot matrix =
  let lu, swaps = lu_decomposition_with_pivoting matrix in
  determinant_from_lu lu swaps

let lu_decomposition_with_pivoting_parallel pool matrix =
  let n = Array.length matrix in
  let lu = Array.copy matrix in
  let p = Array.init n (fun i -> i) in
  let swaps = ref 0 in

  for k = 0 to n - 2 do
    let max_index = ref k in
    let max_value = ref (abs_float lu.(k).(k)) in

    (* Parallelize finding the maximum value and its index *)
    Task.parallel_for pool ~start:(k + 1) ~finish:(n - 1) ~body:(fun i ->
        let abs_val = abs_float lu.(i).(k) in
        if abs_val > !max_value then (
          max_index := i;
          max_value := abs_val));

    if !max_index <> k then (
      swap lu !max_index k;
      swap p !max_index k;
      swaps := !swaps + 1);

    (* Parallelize LU decomposition *)
    Task.parallel_for pool ~start:(k + 1) ~finish:(n - 1) ~body:(fun i ->
        let factor = lu.(i).(k) /. lu.(k).(k) in
        lu.(i).(k) <- factor;
        for j = k + 1 to n - 1 do
          lu.(i).(j) <- lu.(i).(j) -. (factor *. lu.(k).(j))
        done)
  done;
  (lu, !swaps)

(* Calculate determinant from LU decomposition *)
let determinant_from_lu_parallel pool lu swaps =
  let n = Array.length lu in
  let det_sign = if swaps mod 2 = 0 then 1.0 else -1.0 in
  let det = ref det_sign in
  Task.parallel_for pool ~start:0 ~finish:(n - 1) ~body:(fun i ->
      det := !det *. lu.(i).(i));
  !det

let determinant_lu_pivot_parallel pool matrix =
  let lu, swaps = lu_decomposition_with_pivoting_parallel pool matrix in
  determinant_from_lu_parallel pool lu swaps

let size = 100

let main () =
  let m = generate_random_matrix size in
  let mcopy = Array.copy m in
  let res = determinant_lu_pivot m in
  Printf.printf "Res normal: %f\n" res;

  let n_domains = 8 in
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let res =
    Task.run pool (fun () -> determinant_lu_pivot_parallel pool mcopy)
  in
  Task.teardown_pool pool;
  Printf.printf "Res parallel: %f\n" res

let normal () =
  let m = generate_random_matrix size in
  let res = determinant_lu_pivot m in
  print_float_value res

let parallel () =
  let n_domains = 8 in
  let m = generate_random_matrix size in
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let res = Task.run pool (fun () -> determinant_lu_pivot_parallel pool m) in
  Task.teardown_pool pool;
  print_float_value res
