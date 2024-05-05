open Domainslib

type complex = { re : float; im : float }

let complex_add x y = { re = x.re +. y.re; im = x.im +. y.im }
let complex_sub x y = { re = x.re -. y.re; im = x.im -. y.im }
let complex_mul x y = {
  re = (x.re *. y.re) -. (x.im *. y.im);
  im = (x.re *. y.im) +. (x.im *. y.re);
}

let complex_exp theta = {
  re = cos theta;
  im = sin theta;
}

(* Recursive FFT function *)
let rec fft a =
  let n = Array.length a in
  if n = 1 then a
  else begin
    let even = fft (Array.init (n / 2) (fun i -> a.(2 * i))) in
    let odd = fft (Array.init (n / 2) (fun i -> a.(2 * i + 1))) in
    let t = Array.make n {re = 0.0; im = 0.0} in
    for k = 0 to (n / 2) - 1 do
      let e = complex_exp (-.2.0 *. Float.pi *. float_of_int k /. float_of_int n) in
      let ce = complex_mul e odd.(k) in
      t.(k) <- complex_add even.(k) ce;
      t.(k + n / 2) <- complex_sub even.(k) ce;
    done;
    t
  end

(* Parallel FFT function using Domainslib.Task *)
let rec fft_parallel pool a =
  let n = Array.length a in
  if n = 1 then a
  else begin
    let even = Task.async pool (fun () -> fft_parallel pool (Array.init (n / 2) (fun i -> a.(2 * i)))) in
    let odd = Task.async pool (fun () -> fft_parallel pool (Array.init (n / 2) (fun i -> a.(2 * i + 1)))) in
    let evens = Task.await pool even in
    let odds = Task.await pool odd in
    let t = Array.make n {re = 0.0; im = 0.0} in
    Task.parallel_for pool ~start:0 ~finish:((n / 2) - 1) ~body:(fun k ->
      let e = complex_exp (-.2.0 *. Float.pi *. float_of_int k /. float_of_int n) in
      let ce = complex_mul e odds.(k) in
      t.(k) <- complex_add evens.(k) ce;
      t.(k + n / 2) <- complex_sub evens.(k) ce;
    );
    t
  end

(* Function to generate random complex numbers *)
let generate_random_complex_array size =
  Array.init size (fun _ -> { re = Float.of_int (Random.int 1000000); im = 0.0 })

let a = generate_random_complex_array 100

(* Normal execution *)
let normal () =
  
  let res = fft a in
  Array.iteri (fun i c -> if i < 10 then Printf.printf "Seq: %d: (%.2f, %.2f)\n" i c.re c.im) res

(* Parallel execution *)
let parallel() =
  let n_domains = 8 in
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let res = Task.run pool (fun () -> fft_parallel pool a) in
  Task.teardown_pool pool;
  Array.iteri (fun i c -> if i < 10 then Printf.printf "Par: %d: (%.2f, %.2f)\n" i c.re c.im) res