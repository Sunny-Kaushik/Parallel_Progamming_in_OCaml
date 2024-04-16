open Domainslib

let print_arr lst =
  Array.iter
    (fun xi ->
      print_int xi;
      print_string " ")
    lst;
  print_newline ()

let rec merge_sort arr =
  let merge left right =
    let merged = Array.make (Array.length left + Array.length right) 0 in
    let rec merge_helper i j k =
      if i < Array.length left && j < Array.length right then
        if left.(i) < right.(j) then (
          merged.(k) <- left.(i);
          merge_helper (i + 1) j (k + 1))
        else (
          merged.(k) <- right.(j);
          merge_helper i (j + 1) (k + 1))
      else if i < Array.length left then (
        merged.(k) <- left.(i);
        merge_helper (i + 1) j (k + 1))
      else if j < Array.length right then (
        merged.(k) <- right.(j);
        merge_helper i (j + 1) (k + 1))
    in
    merge_helper 0 0 0;
    merged
  in
  let split arr =
    let mid = Array.length arr / 2 in
    let left = Array.sub arr 0 mid in
    let right = Array.sub arr mid (Array.length arr - mid) in
    (left, right)
  in
  if Array.length arr <= 1 then arr
  else
    let left, right = split arr in
    let sorted_left_task = merge_sort left in
    let sorted_right_task = merge_sort right in
    merge sorted_left_task sorted_right_task

let rec merge_sort_parallel pool arr =
  let merge pool left right =
    let merged = Array.make (Array.length left + Array.length right) 0 in
    let rec merge_helper pool i j k =
      if i < Array.length left && j < Array.length right then
        if left.(i) < right.(j) then (
          merged.(k) <- left.(i);
          merge_helper pool (i + 1) j (k + 1))
        else (
          merged.(k) <- right.(j);
          merge_helper pool i (j + 1) (k + 1))
      else if i < Array.length left then (
        merged.(k) <- left.(i);
        merge_helper pool (i + 1) j (k + 1))
      else if j < Array.length right then (
        merged.(k) <- right.(j);
        merge_helper pool i (j + 1) (k + 1))
    in
    merge_helper pool 0 0 0;
    merged
  in
  let split arr =
    let mid = Array.length arr / 2 in
    let left = Array.sub arr 0 mid in
    let right = Array.sub arr mid (Array.length arr - mid) in
    (left, right)
  in
  if Array.length arr <= 1 then arr
  else
    let left, right = split arr in
    let sorted_left_task =
      Task.async pool (fun () -> merge_sort_parallel pool left)
    in
    let sorted_right_task =
      Task.async pool (fun () -> merge_sort_parallel pool right)
    in
    let sorted_left = Task.await pool sorted_left_task in
    let sorted_right = Task.await pool sorted_right_task in
    merge pool sorted_left sorted_right

let lst = [| 7; 6; 5; 4; 10; 1 |]

let normal() =
    let sorted_lst = merge_sort lst in
    print_arr sorted_lst


let parallel () =
  let n_domains = 8 in
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let sorted_lst = Task.run pool (fun () -> merge_sort_parallel pool lst) in
  Task.teardown_pool pool;
  print_arr sorted_lst
