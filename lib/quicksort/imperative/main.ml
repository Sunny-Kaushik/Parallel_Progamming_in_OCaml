open Domainslib

let generate_random_array size =
  let arr = Array.make size 0 in
  for i = 0 to size - 1 do
    arr.(i) <-
      Random.int 100000 (* Generate random integers between 0 and 999 *)
  done;
  arr

let swap arr i j =
  let temp = arr.(i) in
  arr.(i) <- arr.(j);
  arr.(j) <- temp

let quick_sort arr =
  let partition arr low high =
    let pivot = arr.(high) in

    let rec partition_helper arr i j =
      if j >= high then i
      else if arr.(j) < pivot then (
        swap arr (i + 1) j;
        partition_helper arr (i + 1) (j + 1))
      else partition_helper arr i (j + 1)
    in

    let new_i = partition_helper arr (low - 1) low in
    swap arr (new_i + 1) high;
    new_i + 1
  in

  let rec quick_sort_helper arr low high =
    if low < high then (
      let pi = partition arr low high in
      quick_sort_helper arr low (pi - 1);
      quick_sort_helper arr (pi + 1) high)
  in

  let n = Array.length arr in
  quick_sort_helper arr 0 (n - 1)

let quick_sort_parallel pool arr =
  let partition arr low high =
    let pivot = arr.(high) in

    let rec partition_helper arr i j =
      if j >= high then i
      else if arr.(j) < pivot then (
        swap arr (i + 1) j;
        partition_helper arr (i + 1) (j + 1))
      else partition_helper arr i (j + 1)
    in

    let new_i = partition_helper arr (low - 1) low in
    swap arr (new_i + 1) high;
    new_i + 1
  in

  let rec quick_sort_helper_parallel pool arr low high =
    if low < high then (
      let pi = partition arr low high in
      let left_task =
        Task.async pool (fun () ->
            quick_sort_helper_parallel pool arr low (pi - 1))
      in
      let right_task =
        Task.async pool (fun () ->
            quick_sort_helper_parallel pool arr (pi + 1) high)
      in
      Task.await pool left_task;
      Task.await pool right_task)
  in

  let n = Array.length arr in
  quick_sort_helper_parallel pool arr 0 (n - 1)

let size = 1000000

let normal () =
  let () =
    let arr = generate_random_array size in
    quick_sort arr;
    Printf.printf "Sorted array: %s\n"
      (Array.to_list arr |> List.map string_of_int |> String.concat ", ")
  in
  ()

let parallel () =
  let n_domains = 8 in
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let arr = generate_random_array size in
  Task.run pool (fun () -> quick_sort_parallel pool arr);
  Task.teardown_pool pool;
  Printf.printf "Sorted array: %s\n"
    (Array.to_list arr |> List.map string_of_int |> String.concat ", ")
