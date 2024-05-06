open Domainslib


let dot_product v1 v2 =
  let sum = ref 0.0 in
  Array.iter2 (fun x y -> sum := !sum +. x *. y) v1 v2;
  !sum

let vector_subtract v1 v2 =
  Array.map2 (fun x y -> x -. y) v1 v2

let scalar_multiply scalar v =
  Array.map (fun x -> scalar *. x) v

let vector_norm v =
  sqrt (dot_product v v)

let normalize v =
  scalar_multiply (1.0 /. vector_norm v) v

let gram_schmidt a =
  let m, n = Array.length a, Array.length a.(0) in
  let q = Array.make_matrix m n 0.0 in
  let r = Array.make_matrix n n 0.0 in
  for i = 0 to n - 1 do
    let ai = Array.init m (fun j -> a.(j).(i)) in
    let u = ref ai in
    for j = 0 to i - 1 do
      let qj = Array.init m (fun k -> q.(k).(j)) in
      let rij = dot_product ai qj in
      r.(j).(i) <- rij;
      let projected = scalar_multiply rij qj in
      u := vector_subtract !u projected
    done;
    let ui_norm = vector_norm !u in
    r.(i).(i) <- ui_norm;
    let qi = normalize !u in
    Array.iteri (fun j _ -> q.(j).(i) <- qi.(j)) qi
  done;
  q, r


(* Parallel Gram-Schmidt Orthogonalization *)
let gram_schmidt_parallel pool a =
  let m, n = Array.length a, Array.length a.(0) in
  let q = Array.make_matrix m n 0.0 in
  let r = Array.make_matrix n n 0.0 in
  Task.parallel_for pool ~start:0 ~finish:(n - 1) ~body:(fun i ->
    let ai = Array.init m (fun j -> a.(j).(i)) in
    let u = ref ai in
    for j = 0 to i - 1 do
      let qj = Array.init m (fun k -> q.(k).(j)) in
      let rij = dot_product ai qj in
      r.(j).(i) <- rij;
      let projected = scalar_multiply rij qj in
      u := vector_subtract !u projected
    done;
    let ui_norm = vector_norm !u in
    r.(i).(i) <- ui_norm;
    let qi = normalize !u in
    Array.iteri (fun j _ -> q.(j).(i) <- qi.(j)) qi
  );
  q, r


let print_matrix mat =
  Array.iter
    (fun row ->
      Array.iter (fun elem -> Printf.printf "%.2f " elem) row;
      print_newline ())
    mat

let generate_random_matrix rows cols =
  let matrix = Array.make_matrix rows cols 0.0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      matrix.(i).(j) <- Random.float 1.0  (* Generate a random float between 0 and 1 *)
    done
  done;
  matrix


let normal () =
  let a = generate_random_matrix 500 500 in
  let q, r = gram_schmidt a in
  print_matrix q;
  print_newline ();
  print_matrix r

let parallel () =
  let n_domains = 8 in
  let a = generate_random_matrix 500 500 in
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let q, r = Task.run pool (fun () -> gram_schmidt_parallel pool a) in
  Task.teardown_pool pool;
  print_matrix q;
  print_newline ();
  print_matrix r