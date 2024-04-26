open Domainslib

let () = Random.init 42

type 'a bin_tree = BTEmpty | BTNonEmpty of 'a * 'a bin_tree * 'a bin_tree

let rec random_bin_tree height =
  if height <= 0 then BTEmpty
  else
    let value = Random.int 100 in
    let left_height = Random.int height in
    let right_height = height - 1 - left_height in
    BTNonEmpty (value, random_bin_tree left_height, random_bin_tree right_height)

let rec random_bin_tree_parallel (first_time : bool) (pool : Task.pool) height =
  if height <= 0 then BTEmpty
  else if first_time then
    let value = Random.int 100 in
    let left_height = Random.int height in
    let right_height = height - 1 - left_height in
    let left =
      Task.async pool (fun _ -> random_bin_tree_parallel false pool left_height)
    in
    let right =
      Task.async pool (fun _ ->
          random_bin_tree_parallel false pool right_height)
    in
    BTNonEmpty (value, Task.await pool left, Task.await pool right)
  else
    let value = Random.int 100 in
    let left_height = Random.int height in
    let right_height = height - 1 - left_height in
    let left = Task.async pool (fun _ -> random_bin_tree left_height) in
    let right = Task.async pool (fun _ -> random_bin_tree right_height) in
    BTNonEmpty (value, Task.await pool left, Task.await pool right)

(* let value = Random.int 100 in
   let left_height = Random.int height in
   let right_height = height - 1 - left_height in
   let left = Task.async pool (fun _ -> random_bin_tree left_height) in
   let right = Task.async pool (fun _ -> random_bin_tree right_height) in
   BTNonEmpty (value, Task.await pool left, Task.await pool right) *)

let rec height_bin_tree_normal = function
  | BTEmpty -> 0
  | BTNonEmpty (_, left, right) ->
      1 + max (height_bin_tree_normal left) (height_bin_tree_normal right)

let height_bin_tree_parallel (pool : Task.pool) = function
  | BTEmpty -> 0
  | BTNonEmpty (_, left, right) ->
      let left_height =
        Task.async pool (fun _ -> height_bin_tree_normal left)
      in
      let right_height =
        Task.async pool (fun _ -> height_bin_tree_normal right)
      in
      1 + max (Task.await pool left_height) (Task.await pool right_height)

let height = 67108864

let normal () =
  let tree = random_bin_tree height in
  print_endline "Tree generated";
  let h = height_bin_tree_normal tree in
  Printf.printf "Height of tree: %d\n" h

let parallel () =
  let n_domains = 2 in
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let tree =
    Task.run pool (fun () -> random_bin_tree_parallel true pool height)
  in
  let h = Task.run pool (fun () -> height_bin_tree_parallel pool tree) in
  Task.teardown_pool pool;
  Printf.printf "Height of tree: %d\n" h
