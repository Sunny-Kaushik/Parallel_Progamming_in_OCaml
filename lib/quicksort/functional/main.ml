open Domainslib

let rec generate_random_list size =
  if size <= 0 then [] else Random.int 1000000 :: generate_random_list (size - 1)

let print_int_list lst =
  List.iter
    (fun x ->
      print_int x;
      print_string " ")
    lst;
  print_newline ()

let rec partition pivot = function
  | [] -> ([], [])
  | x :: xs ->
      let smaller, larger = partition pivot xs in
      if x < pivot then (x :: smaller, larger) else (smaller, x :: larger)

let rec quick_sort = function
  | [] -> []
  | pivot :: rest ->
      let smaller, larger = partition pivot rest in
      quick_sort smaller @ [ pivot ] @ quick_sort larger

let rec quick_sort_parallel pool = function
  | [] -> []
  | pivot :: rest ->
      let smaller, larger = partition pivot rest in
      let left_task =
        Task.async pool (fun () -> quick_sort_parallel pool smaller)
      in
      let right_task =
        Task.async pool (fun () -> quick_sort_parallel pool larger)
      in
      let sorted_smaller = Task.await pool left_task in
      let sorted_larger = Task.await pool right_task in
      sorted_smaller @ [ pivot ] @ sorted_larger

let size = 100000

let normal () =
  let arr = generate_random_list size in
  let sorted_arr = quick_sort arr in
  Printf.printf "Sorted array: %s\n"
    (List.map string_of_int sorted_arr |> String.concat ", ")

let parallel () =
  let n_domains = 8 in
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let lst = generate_random_list size in
  let sorted_lst = Task.run pool (fun () -> quick_sort_parallel pool lst) in
  Task.teardown_pool pool;
  print_int_list sorted_lst
