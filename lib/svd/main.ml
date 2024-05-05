open Domainslib

(* QR Decomposition from QR_decomposition module, includes normal and parallel
   open qr_decomp *)

let vector_subtract v1 v2 = Array.map2 (fun x y -> x -. y) v1 v2

let dot_product v1 v2 =
  let sum = ref 0.0 in
  Array.iter2 (fun x y -> sum := !sum +. (x *. y)) v1 v2;
  !sum

let scalar_multiply scalar v = Array.map (fun x -> scalar *. x) v
let vector_norm v = sqrt (dot_product v v)
let normalize v = scalar_multiply (1.0 /. vector_norm v) v

(* Function to print a matrix for debugging *)
let print_matrix m =
  Array.iter
    (fun row ->
      Array.iter (fun element -> Printf.printf "%.2f " element) row;
      print_endline "")
    m

(* Matrix multiplication *)
let matrix_mult a b =
  let a_rows = Array.length a in
  let b_cols = Array.length b.(0) in
  let b_rows = Array.length b in
  Array.init a_rows (fun i ->
      Array.init b_cols (fun j ->
          let sum = ref 0.0 in
          for k = 0 to b_rows - 1 do
            sum := !sum +. (a.(i).(k) *. b.(k).(j))
          done;
          !sum))


let matrix_mult_parallel pool a b =
  let a_rows = Array.length a in
  let b_cols = Array.length b.(0) in
  let b_rows = Array.length b in
  let result = Array.make_matrix a_rows b_cols 0.0 in
  Task.parallel_for pool ~start:0 ~finish:(8 - 1) ~body:(fun i ->
      for j = 0 to b_cols - 1 do
        for k = 0 to b_rows - 1 do
          result.(i).(j) <- result.(i).(j) +. (a.(i).(k) *. b.(k).(j))
        done
      done);
  result

(* QR Decomposition *)
let gram_schmidt a =
  let m, n = (Array.length a, Array.length a.(0)) in
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
  (q, r)

let gram_schmidt_parallel pool a =
  let m, n = (Array.length a, Array.length a.(0)) in
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
  (q, r)

(* Singular Value Decomposition - Normal *)
let svd a =
  let at =
    Array.init
      (Array.length a.(0))
      (fun i -> Array.init (Array.length a) (fun j -> a.(j).(i)))
  in
  let ata = matrix_mult at a in
  let q, _ = gram_schmidt ata in
  let u, s, v_t = (q, ata, q) in
  (u, s, v_t)

(* Singular Value Decomposition - Parallel *)
let svd_parallel pool a =
  let at =
    Array.init
      (Array.length a.(0))
      (fun i -> Array.init (Array.length a) (fun j -> a.(j).(i)))
  in
  let ata = matrix_mult_parallel pool at a in
  let q_r_promise = Task.async pool (fun () -> gram_schmidt_parallel pool ata) in
  let q, _ = Task.await pool q_r_promise in
  (q, ata, q)

let generate_random_matrix rows cols =
  let matrix = Array.make_matrix rows cols 0.0 in
  for i = 0 to rows - 1 do
    for j = 0 to cols - 1 do
      matrix.(i).(j) <- Random.float 1.0 
    done
  done;
  matrix

let a = generate_random_matrix 500 500

(* Example usage *)
let normal () =
  (* Normal SVD computation *)
  let u, s, v_t = svd a in
  print_endline "Normal SVD Computation:";
  print_endline "Matrix U (left singular vectors):";
  print_matrix u;
  print_endline "Matrix S (singular values, simplified as A^T * A):";
  print_matrix s;
  print_endline "Matrix V^T (right singular vectors transposed, simplified):";
  print_matrix v_t

let parallel () =
  let n_domains = 8 in
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let u_p, s_p, v_t_p = Task.run pool (fun () -> svd_parallel pool a) in
  print_endline "Parallel SVD Computation:";
  print_endline "Matrix U (left singular vectors):";
  print_matrix u_p;
  print_endline "Matrix S (singular values, simplified as A^T * A):";
  print_matrix s_p;
  print_endline "Matrix V^T (right singular vectors transposed, simplified):";
  print_matrix v_t_p;
  Task.teardown_pool pool
