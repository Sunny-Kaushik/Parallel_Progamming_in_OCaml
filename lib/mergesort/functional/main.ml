open Domainslib

let rec merge_sort_parallel pool lst =
  let rec merge pool left right =
    match left, right with
    | [], right -> right
    | left, [] -> left
    | h1::t1, h2::t2 ->
        if h1 < h2 then h1 :: merge pool t1 right
        else h2 :: merge pool left t2
  in
  let split lst =
    let rec split_acc lst acc1 acc2 =
      match lst with
      | [] -> (acc1, acc2)
      | [x] -> (x::acc1, acc2)
      | x1::x2::tl -> split_acc tl (x1::acc1) (x2::acc2)
    in
    match lst with
    | [] -> ([], [])
    | _::[] -> (lst, [])
    | _ -> let left, right = split_acc lst [] [] in
           (List.rev left, List.rev right)
  in
  match lst with
  | [] -> []
  | [x] -> [x]
  | _ ->
      let left, right = split lst in
      let sorted_left_task = Task.async pool (fun () -> merge_sort_parallel pool left) in
      let sorted_right_task = Task.async pool (fun () -> merge_sort_parallel pool right) in
      let sorted_left = Task.await pool sorted_left_task in
      let sorted_right = Task.await pool sorted_right_task in
      (merge pool sorted_left sorted_right)


let print_int_list lst =
List.iter (fun x -> print_int x; print_string " ") lst;
print_newline () 
let main() =
  let lst = [4; 2; 5; 1; 6; 3] in
  let pool = Task.setup_pool ~num_domains:4 () in
  let sorted_lst = Task.run pool (fun () -> merge_sort_parallel pool lst) in
  Task.teardown_pool pool;
  print_int_list sorted_lst ;;
