type planet = {
  mutable x : float;
  mutable y : float;
  mutable z : float;
  mutable vx : float;
  mutable vy : float;
  mutable vz : float;
  mass : float;
}

let advance bodies n_bodies dt =
  for i = 0 to n_bodies - 1 do
    let b = bodies.(i) in
    for j = i + 1 to n_bodies - 1 do
      let b' = bodies.(j) in
      (* ith body is own & consider wrt to jth body *)
      let dx = b.x -. b'.x and dy = b.y -. b'.y and dz = b.z -. b'.z in
      let dist2 = (dx *. dx) +. (dy *. dy) +. (dz *. dz) in
      (* find magnitude of gravitational force b/w b & b' using distance b/w them *)
      let mag = dt /. (dist2 *. sqrt dist2) in

      (* update velocity of b wrt b' *)
      b.vx <- b.vx -. (dx *. b'.mass *. mag);
      b.vy <- b.vy -. (dy *. b'.mass *. mag);
      b.vz <- b.vz -. (dz *. b'.mass *. mag);

      (* update velocity of b' wrt b *)
      b'.vx <- b'.vx +. (dx *. b.mass *. mag);
      b'.vy <- b'.vy +. (dy *. b.mass *. mag);
      b'.vz <- b'.vz +. (dz *. b.mass *. mag)
    done
  done

let update bodies dt =
  let n_bodies = Array.length bodies in
  for i = 0 to n_bodies - 1 do
    let b = bodies.(i) in
    (* update position of body 'b' based on it's current velocity *)
    b.x <- b.x +. (dt *. b.vx);
    b.y <- b.y +. (dt *. b.vy);
    b.z <- b.z +. (dt *. b.vz)
  done

let energy bodies =
  let e = ref 0. in
  for i = 0 to Array.length bodies - 1 do
    let b = bodies.(i) in
    e :=
      !e
      +. (0.5 *. b.mass *. ((b.vx *. b.vx) +. (b.vy *. b.vy) +. (b.vz *. b.vz)));
    for j = i + 1 to Array.length bodies - 1 do
      let b' = bodies.(j) in
      let dx = b.x -. b'.x and dy = b.y -. b'.y and dz = b.z -. b'.z in
      let distance = sqrt ((dx *. dx) +. (dy *. dy) +. (dz *. dz)) in
      e := !e -. (b.mass *. b'.mass /. distance)
    done
  done;
  !e

let pi = 3.141592653589793
let solar_mass = 4. *. pi *. pi
let days_per_year = 365.24

let offset_momentum bodies =
  let px = ref 0. and py = ref 0. and pz = ref 0. in
  for i = 0 to Array.length bodies - 1 do
    px := !px +. (bodies.(i).vx *. bodies.(i).mass);
    py := !py +. (bodies.(i).vy *. bodies.(i).mass);
    pz := !pz +. (bodies.(i).vz *. bodies.(i).mass)
  done;
  bodies.(0).vx <- -. !px /. solar_mass;
  bodies.(0).vy <- -. !py /. solar_mass;
  bodies.(0).vz <- -. !pz /. solar_mass

let initialize_bodies num_bodies =
  Array.init num_bodies (fun _ ->
      {
        x = Random.float 10.;
        y = Random.float 10.;
        z = Random.float 10.;
        vx = Random.float 5. *. days_per_year;
        vy = Random.float 4. *. days_per_year;
        vz = Random.float 5. *. days_per_year;
        mass = Random.float 10. *. solar_mass;
      })

let main () =
  let n = 256 in
  let num_bodies = 4096 in
  let bodies = initialize_bodies num_bodies in
  
  (* dt -> used to calculate changes in velocity & position of bodies over time *)
  let dt = 0.01 in
  
  (* makes sure total momentum of the system is zero *)
  (* help simplify our simulation by making the center of mass stationary *)
  offset_momentum bodies;
  Printf.printf "%.9f\n" (energy bodies);
  for _i = 1 to n do
    advance bodies num_bodies dt;
    update bodies dt
  done;
  Printf.printf "%.9f\n" (energy bodies)
