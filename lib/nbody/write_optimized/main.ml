open Domainslib

(*
  Split array of planet_pos & planet_vec to reduce cache contention between the two arrays.
  planet_pos are read frequently and written to infrequently.
  planet_vec are written to frequently and read infrequently.

  Nbodies: 4096 (2^12)
  So updated a total of 4096 * 4096 = 16777216 times
*)

type planet_pos = {
  mutable x : float;
  mutable y : float;
  mutable z : float;
  mass : float;
}

type planet_vec = { mutable vx : float; mutable vy : float; mutable vz : float }

let advance pool n_domains n_bodies bodies_pos bodies_vec dt =
  Task.parallel_for pool ~chunk_size:(n_bodies / n_domains) ~start:0
    ~finish:(n_bodies - 1) ~body:(fun i ->
      let bp = bodies_pos.(i) in
      let bv = bodies_vec.(i) in
      let vx, vy, vz = (ref bv.vx, ref bv.vy, ref bv.vz) in
      for j = 0 to n_bodies - 1 do
        let bp' = bodies_pos.(j) in
        if i != j then (
          let dx = bp.x -. bp'.x
          and dy = bp.y -. bp'.y
          and dz = bp.z -. bp'.z in
          let dist2 = (dx *. dx) +. (dy *. dy) +. (dz *. dz) in
          let mag = dt /. (dist2 *. sqrt dist2) in
          let mass = bp'.mass in
          vx := !vx -. (dx *. mass *. mag);
          vy := !vy -. (dy *. mass *. mag);
          vz := !vz -. (dz *. mass *. mag))
      done;
      bv.vx <- !vx;
      bv.vy <- !vy;
      bv.vz <- !vz);

  let update bodies dt =
    let n_bodies = Array.length bodies in
    Task.parallel_for pool ~chunk_size:(n_bodies / n_domains) ~start:0
      ~finish:(n_bodies - 1) ~body:(fun i ->
        let bp = bodies.(i) in
        let bv = bodies_vec.(i) in
        bp.x <- bp.x +. (dt *. bv.vx);
        bp.y <- bp.y +. (dt *. bv.vy);
        bp.z <- bp.z +. (dt *. bv.vz))
  in
  update bodies_pos dt

let energy bodies_pos bodies_vec =
  let e = ref 0. in
  for i = 0 to Array.length bodies_pos - 1 do
    let bp = bodies_pos.(i) in
    let bv = bodies_vec.(i) in
    e :=
      !e
      +. 0.5 *. bp.mass
         *. ((bv.vx *. bv.vx) +. (bv.vy *. bv.vy) +. (bv.vz *. bv.vz));
    for j = i + 1 to Array.length bodies_pos - 1 do
      let bp' = bodies_pos.(j) in
      let dx = bp.x -. bp'.x and dy = bp.y -. bp'.y and dz = bp.z -. bp'.z in
      let distance = sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz)) in
      e := !e -. (bp.mass *. bp'.mass /. distance)
    done
  done;
  !e

let pi = 3.141592653589793
let solar_mass = 4. *. pi *. pi
let days_per_year = 365.24

let offset_momentum bodies_pos bodies_vec =
  let px = ref 0. and py = ref 0. and pz = ref 0. in
  for i = 0 to Array.length bodies_pos - 1 do
    px := !px +. (bodies_vec.(i).vx *. bodies_pos.(i).mass);
    py := !py +. (bodies_vec.(i).vy *. bodies_pos.(i).mass);
    pz := !pz +. (bodies_vec.(i).vz *. bodies_pos.(i).mass)
  done;
  bodies_vec.(0).vx <- -. !px /. solar_mass;
  bodies_vec.(0).vy <- -. !py /. solar_mass;
  bodies_vec.(0).vz <- -. !pz /. solar_mass

let initialize_bodies num_bodies =
  ( Array.init num_bodies (fun _ ->
        {
          x = Random.float 10.;
          y = Random.float 10.;
          z = Random.float 10.;
          mass = Random.float 10. *. solar_mass;
        }),
    Array.init num_bodies (fun _ ->
        {
          vx = Random.float 5. *. days_per_year;
          vy = Random.float 4. *. days_per_year;
          vz = Random.float 5. *. days_per_year;
        }) )

let main () =
  let n = 256 in
  let n_bodies = 4096 in
  let n_domains = 8 in
  let pool = Task.setup_pool ~num_domains:(n_domains - 1) () in
  let bodies_pos, bodies_vec = initialize_bodies n_bodies in

  (* dt -> used to calculate changes in velocity & position of bodies over time *)
  let dt = 0.01 in

  (* makes sure total momentum of the system is zero *)
  (* help simplify our simulation by making the center of mass stationary *)
  offset_momentum bodies_pos bodies_vec;
  Printf.printf "%.9f\n" (energy bodies_pos bodies_vec);
  for _i = 1 to n do
    Task.run pool (fun () ->
        advance pool n_domains n_bodies bodies_pos bodies_vec dt)
  done;
  Printf.printf "%.9f\n" (energy bodies_pos bodies_vec);
  Task.teardown_pool pool
