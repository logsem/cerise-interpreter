type t = Int of Z.t | Inf

exception InfiniteZException of string

let of_int (n : int) : t = Int (Z.of_int n)
let of_z (z : Z.t) : t = Int z
let to_string (zi : t) =
  match zi with
  | Inf -> "∞"
  | Int z -> Z.to_string z

let add (zi1 : t) (zi2 : t) : t =
  match zi1, zi2 with
  | Int z1, Int z2 -> Int Z.(z1 + z2)
  | _, _ -> Inf
let (+) = add
let z_add (z1 : Z.t) (zi2 : t) : t = (Int z1) + zi2
let add_z (zi1 : t) (z2 : Z.t)  : t = zi1 + (Int z2)

let sub (zi1 : t) (zi2 : t) : t =
  match zi1, zi2 with
  | Int z1, Int z2 -> Int Z.(z1 - z2)
  | Inf, Inf -> Int Z.zero
  | Inf, _ -> Inf
  | _, Inf ->
    raise @@
    InfiniteZException
      (Printf.sprintf "Substraction \"%s - ∞\"  not allowed." (to_string zi1))
let (-) = sub
let z_sub (z1 : Z.t) (zi2 : t) : t = (Int z1) + zi2
let sub_z (zi1 : t) (z2 : Z.t)  : t = zi1 + (Int z2)

let min (zi1 : t) (zi2 : t) : t =
  match zi1, zi2 with
  | Int z1, Int z2 -> Int (Z.min z1 z2)
  | Inf, Inf -> Inf
  | Inf, _ -> zi2
  |  _, Inf -> zi1
let z_min (z1 : Z.t) (zi2 : t) : t = min (Int z1) zi2
let min_z (zi1 : t) (z2 : Z.t)  : t = min zi1 (Int z2)

let eq (zi1 : t) (zi2 : t) : bool =
  match zi1, zi2 with
  | Int z1, Int z2 -> z1 = z2
  | Inf, Inf -> true
  |  _, _ -> false
let (=) = eq
let z_eq (z1 : Z.t) (zi2 : t) : bool = (Int z1) = zi2
let eq_z (zi1 : t) (z2 : Z.t)  : bool = zi1 = (Int z2)

let lt (zi1 : t) (zi2 : t) : bool =
  match zi1, zi2 with
  | Int z1, Int z2 -> z1 < z2
  | Inf, _ -> false
  |  _, Inf -> true
let (<) = lt
let z_lt (z1 : Z.t) (zi2 : t) : bool = (Int z1) < zi2
let lt_z (zi1 : t) (z2 : Z.t)  : bool = zi1 < (Int z2)

let leq (zi1 : t) (zi2 : t) : bool =
  match zi1, zi2 with
  | Int z1, Int z2 -> z1 <= z2
  |  _, Inf -> true
  | Inf, _ -> false
let (<=) = leq
let z_leq (z1 : Z.t) (zi2 : t) : bool = (Int z1) <= zi2
let leq_z (zi1 : t) (z2 : Z.t)  : bool = zi1 <= (Int z2)

let gt (zi1 : t) (zi2 : t) : bool =
  match zi1, zi2 with
  | Int z1, Int z2 -> z1 > z2
  |  _, Inf -> false
  | Inf, _ -> true
let (>) = gt
let z_gt (z1 : Z.t) (zi2 : t) : bool = (Int z1) > zi2
let gt_z (zi1 : t) (z2 : Z.t)  : bool = zi1 > (Int z2)

let geq (zi1 : t) (zi2 : t) : bool =
  match zi1, zi2 with
  | Int z1, Int z2 -> z1 >= z2
  | Inf, _ -> true
  |  _, Inf -> false
let (>=) = geq
let z_geq (z1 : Z.t) (zi2 : t) : bool = (Int z1) >= zi2
let geq_z (zi1 : t) (z2 : Z.t)  : bool = zi1 >= (Int z2)
