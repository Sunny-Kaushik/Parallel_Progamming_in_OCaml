open Domainslib

let rec fib n = if n < 2 then 1 else fib (n - 1) + fib (n - 2)

let rec fib_parallel (pool : Task.pool) n =
  if n <= 40 then fib n
  else
    let a = Task.async pool (fun _ -> fib_parallel pool (n - 1)) in
    let b = Task.async pool (fun _ -> fib_parallel pool (n - 2)) in
    Task.await pool a + Task.await pool b

let normal () =
  let n = 42 in
  let res = fib n in
  Printf.printf "fib(%d) = %d\n" n res

let parallel () =
  let n = 42 in
  let n_domains = 8 in
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let res = Task.run pool (fun () -> fib_parallel pool n) in
  Task.teardown_pool pool;
  Printf.printf "fib(%d) = %d\n" n res
