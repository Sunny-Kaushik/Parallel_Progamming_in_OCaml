open Domainslib

let generate_random_matrix size =
  let m = Array.make_matrix size size 0 in
  for i = 0 to size - 1 do
    for j = 0 to size - 1 do
      m.(i).(j) <-
        Random.int 1000 (* Generate random integers between 0 and 999 *)
    done
  done;
  m

let print_matrix mat =
  Array.iter
    (fun row ->
      Array.iter (fun elem -> Printf.printf "%d " elem) row;
      print_newline ())
    mat

let matrix_mult n a b =
  let result = Array.make_matrix n n 0 in
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      for k = 0 to n - 1 do
        result.(i).(j) <- result.(i).(j) + (a.(i).(k) * b.(k).(j))
      done
    done
  done;
  result

let matrix_mult_parallel pool n a b =
  let result = Array.make_matrix n n 0 in
  Task.parallel_for pool ~start:0 ~finish:(n - 1) ~body:(fun i ->
      for k = 0 to n - 1 do
        for j = 0 to n - 1 do
          result.(i).(j) <- result.(i).(j) + (a.(i).(k) * b.(k).(j))
        done
      done);
  result

let size = 1024

let normal () =
  let m = generate_random_matrix size and n = generate_random_matrix size in
  let res = matrix_mult size m n in
  print_matrix res

let parallel () =
  let n_domains = 8 in
  let m = generate_random_matrix size and n = generate_random_matrix size in
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let res = Task.run pool (fun () -> matrix_mult_parallel pool size m n) in
  Task.teardown_pool pool;
  print_matrix res
