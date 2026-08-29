(* GENERATED FILE -- DO NOT EDIT. *)


type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

type nat =
| O
| S of nat

(** val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option **)

let option_map f = function
| Some a -> Some (f a)
| None -> None

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b

(** val fst : ('a1 * 'a2) -> 'a1 **)

let fst = function
| (x, _) -> x

(** val snd : ('a1 * 'a2) -> 'a2 **)

let snd = function
| (_, y) -> y

type comparison =
| Eq
| Lt
| Gt

(** val compOpp : comparison -> comparison **)

let compOpp = function
| Eq -> Eq
| Lt -> Gt
| Gt -> Lt

(** val id : __ -> __ **)

let id x =
  x

module Coq__1 = struct
 (** val add : nat -> nat -> nat **)

 let rec add n0 m =
   match n0 with
   | O -> m
   | S p -> S (add p m)
end
include Coq__1

type positive =
| XI of positive
| XO of positive
| XH

type n =
| N0
| Npos of positive

type z =
| Z0
| Zpos of positive
| Zneg of positive

module Nat =
 struct
  (** val eq_dec : nat -> nat -> bool **)

  let rec eq_dec n0 m =
    match n0 with
    | O -> (match m with
            | O -> true
            | S _ -> false)
    | S n1 -> (match m with
               | O -> false
               | S n2 -> eq_dec n1 n2)
 end

module Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val add : positive -> positive -> positive **)

  let rec add x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XO p ->
      (match y with
       | XI q -> XI (add p q)
       | XO q -> XO (add p q)
       | XH -> XI p)
    | XH -> (match y with
             | XI q -> XO (succ q)
             | XO q -> XI q
             | XH -> XO XH)

  (** val add_carry : positive -> positive -> positive **)

  and add_carry x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> XI (add_carry p q)
       | XO q -> XO (add_carry p q)
       | XH -> XI (succ p))
    | XO p ->
      (match y with
       | XI q -> XO (add_carry p q)
       | XO q -> XI (add p q)
       | XH -> XO (succ p))
    | XH ->
      (match y with
       | XI q -> XI (succ q)
       | XO q -> XO (succ q)
       | XH -> XI XH)

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH

  (** val pred_N : positive -> n **)

  let pred_N = function
  | XI p -> Npos (XO p)
  | XO p -> Npos (pred_double p)
  | XH -> N0

  (** val mul : positive -> positive -> positive **)

  let rec mul x y =
    match x with
    | XI p -> add y (XO (mul p y))
    | XO p -> XO (mul p y)
    | XH -> y

  (** val iter : ('a1 -> 'a1) -> 'a1 -> positive -> 'a1 **)

  let rec iter f x = function
  | XI n' -> f (iter f (iter f x n') n')
  | XO n' -> iter f (iter f x n') n'
  | XH -> f x

  (** val div2 : positive -> positive **)

  let div2 = function
  | XI p0 -> p0
  | XO p0 -> p0
  | XH -> XH

  (** val div2_up : positive -> positive **)

  let div2_up = function
  | XI p0 -> succ p0
  | XO p0 -> p0
  | XH -> XH

  (** val compare_cont : comparison -> positive -> positive -> comparison **)

  let rec compare_cont r x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> compare_cont r p q
       | XO q -> compare_cont Gt p q
       | XH -> Gt)
    | XO p ->
      (match y with
       | XI q -> compare_cont Lt p q
       | XO q -> compare_cont r p q
       | XH -> Gt)
    | XH -> (match y with
             | XH -> r
             | _ -> Lt)

  (** val compare : positive -> positive -> comparison **)

  let compare =
    compare_cont Eq

  (** val eqb : positive -> positive -> bool **)

  let rec eqb p q =
    match p with
    | XI p0 -> (match q with
                | XI q0 -> eqb p0 q0
                | _ -> false)
    | XO p0 -> (match q with
                | XO q0 -> eqb p0 q0
                | _ -> false)
    | XH -> (match q with
             | XH -> true
             | _ -> false)

  (** val coq_Nsucc_double : n -> n **)

  let coq_Nsucc_double = function
  | N0 -> Npos XH
  | Npos p -> Npos (XI p)

  (** val coq_Ndouble : n -> n **)

  let coq_Ndouble = function
  | N0 -> N0
  | Npos p -> Npos (XO p)

  (** val coq_lor : positive -> positive -> positive **)

  let rec coq_lor p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> XI (coq_lor p0 q0)
       | XO q0 -> XI (coq_lor p0 q0)
       | XH -> p)
    | XO p0 ->
      (match q with
       | XI q0 -> XI (coq_lor p0 q0)
       | XO q0 -> XO (coq_lor p0 q0)
       | XH -> XI p0)
    | XH -> (match q with
             | XO q0 -> XI q0
             | _ -> q)

  (** val coq_land : positive -> positive -> n **)

  let rec coq_land p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> coq_Nsucc_double (coq_land p0 q0)
       | XO q0 -> coq_Ndouble (coq_land p0 q0)
       | XH -> Npos XH)
    | XO p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (coq_land p0 q0)
       | XO q0 -> coq_Ndouble (coq_land p0 q0)
       | XH -> N0)
    | XH -> (match q with
             | XO _ -> N0
             | _ -> Npos XH)

  (** val ldiff : positive -> positive -> n **)

  let rec ldiff p q =
    match p with
    | XI p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (ldiff p0 q0)
       | XO q0 -> coq_Nsucc_double (ldiff p0 q0)
       | XH -> Npos (XO p0))
    | XO p0 ->
      (match q with
       | XI q0 -> coq_Ndouble (ldiff p0 q0)
       | XO q0 -> coq_Ndouble (ldiff p0 q0)
       | XH -> Npos p)
    | XH -> (match q with
             | XO _ -> Npos XH
             | _ -> N0)

  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1 **)

  let rec iter_op op p a =
    match p with
    | XI p0 -> op a (iter_op op p0 (op a a))
    | XO p0 -> iter_op op p0 (op a a)
    | XH -> a

  (** val to_nat : positive -> nat **)

  let to_nat x =
    iter_op Coq__1.add x (S O)

  (** val of_succ_nat : nat -> positive **)

  let rec of_succ_nat = function
  | O -> XH
  | S x -> succ (of_succ_nat x)
 end

module Coq_Pos =
 struct
  (** val succ : positive -> positive **)

  let rec succ = function
  | XI p -> XO (succ p)
  | XO p -> XI p
  | XH -> XO XH

  (** val pred_double : positive -> positive **)

  let rec pred_double = function
  | XI p -> XI (XO p)
  | XO p -> XI (pred_double p)
  | XH -> XH

  (** val pred : positive -> positive **)

  let pred = function
  | XI p -> XO p
  | XO p -> pred_double p
  | XH -> XH

  (** val eq_dec : positive -> positive -> bool **)

  let rec eq_dec p x0 =
    match p with
    | XI p0 -> (match x0 with
                | XI p1 -> eq_dec p0 p1
                | _ -> false)
    | XO p0 -> (match x0 with
                | XO p1 -> eq_dec p0 p1
                | _ -> false)
    | XH -> (match x0 with
             | XH -> true
             | _ -> false)
 end

module N =
 struct
  (** val succ_pos : n -> positive **)

  let succ_pos = function
  | N0 -> XH
  | Npos p -> Pos.succ p

  (** val coq_lor : n -> n -> n **)

  let coq_lor n0 m =
    match n0 with
    | N0 -> m
    | Npos p -> (match m with
                 | N0 -> n0
                 | Npos q -> Npos (Pos.coq_lor p q))

  (** val coq_land : n -> n -> n **)

  let coq_land n0 m =
    match n0 with
    | N0 -> N0
    | Npos p -> (match m with
                 | N0 -> N0
                 | Npos q -> Pos.coq_land p q)

  (** val ldiff : n -> n -> n **)

  let ldiff n0 m =
    match n0 with
    | N0 -> N0
    | Npos p -> (match m with
                 | N0 -> n0
                 | Npos q -> Pos.ldiff p q)
 end

module Coq_N =
 struct
  (** val to_nat : n -> nat **)

  let to_nat = function
  | N0 -> O
  | Npos p -> Pos.to_nat p

  (** val of_nat : nat -> n **)

  let of_nat = function
  | O -> N0
  | S n' -> Npos (Pos.of_succ_nat n')

  (** val eq_dec : n -> n -> bool **)

  let eq_dec n0 m =
    match n0 with
    | N0 -> (match m with
             | N0 -> true
             | Npos _ -> false)
    | Npos p -> (match m with
                 | N0 -> false
                 | Npos p0 -> Coq_Pos.eq_dec p p0)
 end

module Z =
 struct
  (** val double : z -> z **)

  let double = function
  | Z0 -> Z0
  | Zpos p -> Zpos (XO p)
  | Zneg p -> Zneg (XO p)

  (** val succ_double : z -> z **)

  let succ_double = function
  | Z0 -> Zpos XH
  | Zpos p -> Zpos (XI p)
  | Zneg p -> Zneg (Pos.pred_double p)

  (** val pred_double : z -> z **)

  let pred_double = function
  | Z0 -> Zneg XH
  | Zpos p -> Zpos (Pos.pred_double p)
  | Zneg p -> Zneg (XI p)

  (** val pos_sub : positive -> positive -> z **)

  let rec pos_sub x y =
    match x with
    | XI p ->
      (match y with
       | XI q -> double (pos_sub p q)
       | XO q -> succ_double (pos_sub p q)
       | XH -> Zpos (XO p))
    | XO p ->
      (match y with
       | XI q -> pred_double (pos_sub p q)
       | XO q -> double (pos_sub p q)
       | XH -> Zpos (Pos.pred_double p))
    | XH ->
      (match y with
       | XI q -> Zneg (XO q)
       | XO q -> Zneg (Pos.pred_double q)
       | XH -> Z0)

  (** val add : z -> z -> z **)

  let add x y =
    match x with
    | Z0 -> y
    | Zpos x' ->
      (match y with
       | Z0 -> x
       | Zpos y' -> Zpos (Pos.add x' y')
       | Zneg y' -> pos_sub x' y')
    | Zneg x' ->
      (match y with
       | Z0 -> x
       | Zpos y' -> pos_sub y' x'
       | Zneg y' -> Zneg (Pos.add x' y'))

  (** val opp : z -> z **)

  let opp = function
  | Z0 -> Z0
  | Zpos x0 -> Zneg x0
  | Zneg x0 -> Zpos x0

  (** val sub : z -> z -> z **)

  let sub m n0 =
    add m (opp n0)

  (** val mul : z -> z -> z **)

  let mul x y =
    match x with
    | Z0 -> Z0
    | Zpos x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zpos (Pos.mul x' y')
       | Zneg y' -> Zneg (Pos.mul x' y'))
    | Zneg x' ->
      (match y with
       | Z0 -> Z0
       | Zpos y' -> Zneg (Pos.mul x' y')
       | Zneg y' -> Zpos (Pos.mul x' y'))

  (** val compare : z -> z -> comparison **)

  let compare x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> Eq
             | Zpos _ -> Lt
             | Zneg _ -> Gt)
    | Zpos x' -> (match y with
                  | Zpos y' -> Pos.compare x' y'
                  | _ -> Gt)
    | Zneg x' ->
      (match y with
       | Zneg y' -> compOpp (Pos.compare x' y')
       | _ -> Lt)

  (** val leb : z -> z -> bool **)

  let leb x y =
    match compare x y with
    | Gt -> false
    | _ -> true

  (** val ltb : z -> z -> bool **)

  let ltb x y =
    match compare x y with
    | Lt -> true
    | _ -> false

  (** val eqb : z -> z -> bool **)

  let eqb x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> true
             | _ -> false)
    | Zpos p -> (match y with
                 | Zpos q -> Pos.eqb p q
                 | _ -> false)
    | Zneg p -> (match y with
                 | Zneg q -> Pos.eqb p q
                 | _ -> false)

  (** val of_N : n -> z **)

  let of_N = function
  | N0 -> Z0
  | Npos p -> Zpos p

  (** val div2 : z -> z **)

  let div2 = function
  | Z0 -> Z0
  | Zpos p -> (match p with
               | XH -> Z0
               | _ -> Zpos (Pos.div2 p))
  | Zneg p -> Zneg (Pos.div2_up p)

  (** val shiftl : z -> z -> z **)

  let shiftl a = function
  | Z0 -> a
  | Zpos p -> Pos.iter (mul (Zpos (XO XH))) a p
  | Zneg p -> Pos.iter div2 a p

  (** val shiftr : z -> z -> z **)

  let shiftr a n0 =
    shiftl a (opp n0)

  (** val coq_lor : z -> z -> z **)

  let coq_lor a b =
    match a with
    | Z0 -> b
    | Zpos a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> Zpos (Pos.coq_lor a0 b0)
       | Zneg b0 -> Zneg (N.succ_pos (N.ldiff (Pos.pred_N b0) (Npos a0))))
    | Zneg a0 ->
      (match b with
       | Z0 -> a
       | Zpos b0 -> Zneg (N.succ_pos (N.ldiff (Pos.pred_N a0) (Npos b0)))
       | Zneg b0 ->
         Zneg (N.succ_pos (N.coq_land (Pos.pred_N a0) (Pos.pred_N b0))))

  (** val coq_land : z -> z -> z **)

  let coq_land a b =
    match a with
    | Z0 -> Z0
    | Zpos a0 ->
      (match b with
       | Z0 -> Z0
       | Zpos b0 -> of_N (Pos.coq_land a0 b0)
       | Zneg b0 -> of_N (N.ldiff (Npos a0) (Pos.pred_N b0)))
    | Zneg a0 ->
      (match b with
       | Z0 -> Z0
       | Zpos b0 -> of_N (N.ldiff (Npos b0) (Pos.pred_N a0))
       | Zneg b0 ->
         Zneg (N.succ_pos (N.coq_lor (Pos.pred_N a0) (Pos.pred_N b0))))

  (** val eq_dec : z -> z -> bool **)

  let eq_dec x y =
    match x with
    | Z0 -> (match y with
             | Z0 -> true
             | _ -> false)
    | Zpos p -> (match y with
                 | Zpos p0 -> Coq_Pos.eq_dec p p0
                 | _ -> false)
    | Zneg p -> (match y with
                 | Zneg p0 -> Coq_Pos.eq_dec p p0
                 | _ -> false)

  (** val b2z : bool -> z **)

  let b2z = function
  | true -> Zpos XH
  | false -> Z0
 end

(** val le_lt_dec : nat -> nat -> bool **)

let rec le_lt_dec n0 m =
  match n0 with
  | O -> true
  | S n1 -> (match m with
             | O -> false
             | S n2 -> le_lt_dec n1 n2)

(** val le_gt_dec : nat -> nat -> bool **)

let le_gt_dec =
  le_lt_dec

(** val le_dec : nat -> nat -> bool **)

let le_dec =
  le_gt_dec

(** val z_lt_dec : z -> z -> bool **)

let z_lt_dec x y =
  match Z.compare x y with
  | Lt -> true
  | _ -> false

(** val z_le_dec : z -> z -> bool **)

let z_le_dec x y =
  match Z.compare x y with
  | Gt -> false
  | _ -> true

type decision = bool

(** val decide : decision -> bool **)

let decide decision0 =
  decision0

type ('a, 'b) relDecision = 'a -> 'b -> decision

(** val decide_rel : ('a1, 'a2) relDecision -> 'a1 -> 'a2 -> decision **)

let decide_rel relDecision0 =
  relDecision0

type 'a empty = 'a

(** val empty0 : 'a1 empty -> 'a1 **)

let empty0 empty1 =
  empty1

type 'm mBind = __ -> __ -> (__ -> 'm) -> 'm -> 'm

(** val mbind : 'a1 mBind -> ('a2 -> 'a1) -> 'a1 -> 'a1 **)

let mbind mBind0 x x0 =
  Obj.magic mBind0 __ __ x x0

type 'm fMap = __ -> __ -> (__ -> __) -> 'm -> 'm

(** val fmap : 'a1 fMap -> ('a2 -> 'a3) -> 'a1 -> 'a1 **)

let fmap fMap0 x x0 =
  Obj.magic fMap0 __ __ x x0

type ('k, 'a, 'm) lookup = 'k -> 'm -> 'a option

(** val lookup0 : ('a1, 'a2, 'a3) lookup -> 'a1 -> 'a3 -> 'a2 option **)

let lookup0 lookup1 =
  lookup1

type ('k, 'a, 'm) insert = 'k -> 'a -> 'm -> 'm

(** val insert0 : ('a1, 'a2, 'a3) insert -> 'a1 -> 'a2 -> 'a3 -> 'a3 **)

let insert0 insert1 =
  insert1

type ('k, 'a, 'm) partialAlter = ('a option -> 'a option) -> 'k -> 'm -> 'm

(** val partial_alter :
    ('a1, 'a2, 'a3) partialAlter -> ('a2 option -> 'a2 option) -> 'a1 -> 'a3
    -> 'a3 **)

let partial_alter partialAlter0 =
  partialAlter0

(** val and_dec : decision -> decision -> decision **)

let and_dec p_dec q_dec =
  if p_dec then q_dec else false

(** val bool_eq_dec : (bool, bool) relDecision **)

let bool_eq_dec x y =
  if x then if y then true else false else if y then false else true

(** val unit_eq_dec : (unit, unit) relDecision **)

let unit_eq_dec _ _ =
  true

(** val option_bind : (__ -> __ option) -> __ option -> __ option **)

let option_bind f = function
| Some x -> f x
| None -> None

(** val option_fmap : (__ -> __) -> __ option -> __ option **)

let option_fmap =
  option_map

module Coq_Nat =
 struct
  (** val eq_dec : (nat, nat) relDecision **)

  let eq_dec =
    Nat.eq_dec

  (** val le_dec : (nat, nat) relDecision **)

  let le_dec =
    le_dec
 end

module Coq0_Pos =
 struct
  (** val eq_dec : (positive, positive) relDecision **)

  let eq_dec =
    Coq_Pos.eq_dec

  (** val reverse_go : positive -> positive -> positive **)

  let rec reverse_go p1 = function
  | XI p3 -> reverse_go (XI p1) p3
  | XO p3 -> reverse_go (XO p1) p3
  | XH -> p1

  (** val reverse : positive -> positive **)

  let reverse =
    reverse_go XH
 end

module Coq0_N =
 struct
  (** val eq_dec : (n, n) relDecision **)

  let eq_dec =
    Coq_N.eq_dec
 end

module Coq_Z =
 struct
  (** val eq_dec : (z, z) relDecision **)

  let eq_dec =
    Z.eq_dec

  (** val le_dec : (z, z) relDecision **)

  let le_dec =
    z_le_dec

  (** val lt_dec : (z, z) relDecision **)

  let lt_dec =
    z_lt_dec
 end

type 'a countable = { encode : ('a -> positive);
                      decode : (positive -> 'a option) }

(** val unit_countable : unit countable **)

let unit_countable =
  { encode = (fun _ -> XH); decode = (fun _ -> Some ()) }

(** val option_countable :
    ('a1, 'a1) relDecision -> 'a1 countable -> 'a1 option countable **)

let option_countable _ h =
  { encode = (fun o ->
    match o with
    | Some x -> Coq_Pos.succ (h.encode x)
    | None -> XH); decode = (fun p ->
    if decide (decide_rel Coq0_Pos.eq_dec p XH)
    then Some None
    else fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x -> Some x)
           ((Obj.magic h).decode (Coq_Pos.pred p))) }

(** val sum_countable :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision -> 'a2
    countable -> ('a1, 'a2) sum countable **)

let sum_countable _ h _ h0 =
  { encode = (fun xy ->
    match xy with
    | Inl x -> XO (h.encode x)
    | Inr y -> XI (h0.encode y)); decode = (fun p ->
    match p with
    | XI p0 ->
      let p1 = Obj.magic p0 in
      Obj.magic fmap (fun _ _ -> option_fmap) (fun x -> Inr x) (h0.decode p1)
    | XO p0 ->
      let p1 = Obj.magic p0 in
      Obj.magic fmap (fun _ _ -> option_fmap) (fun x -> Inl x) (h.decode p1)
    | XH -> None) }

(** val n_countable : n countable **)

let n_countable =
  { encode = (fun x -> match x with
                       | N0 -> XH
                       | Npos p -> Coq_Pos.succ p);
    decode = (fun p ->
    if decide (decide_rel Coq0_Pos.eq_dec p XH)
    then Some N0
    else Some (Npos (Coq_Pos.pred p))) }

(** val z_countable : z countable **)

let z_countable =
  { encode = (fun x ->
    match x with
    | Z0 -> XH
    | Zpos p -> XO p
    | Zneg p -> XI p); decode = (fun p -> Some
    (match p with
     | XI p0 -> Zneg p0
     | XO p0 -> Zpos p0
     | XH -> Z0)) }

(** val nat_countable : nat countable **)

let nat_countable =
  { encode = (fun x -> n_countable.encode (Coq_N.of_nat x)); decode =
    (fun p ->
    fmap (Obj.magic (fun _ _ -> option_fmap)) Coq_N.to_nat
      ((Obj.magic n_countable).decode p)) }

type ('k, 'a, 'm) mapFold = __ -> ('k -> 'a -> __ -> __) -> __ -> 'm -> __

(** val map_fold :
    ('a1, 'a2, 'a3) mapFold -> ('a1 -> 'a2 -> 'a4 -> 'a4) -> 'a4 -> 'a3 -> 'a4 **)

let map_fold mapFold0 x x0 x1 =
  Obj.magic mapFold0 __ x x0 x1

(** val map_insert :
    ('a1, 'a2, 'a3) partialAlter -> ('a1, 'a2, 'a3) insert **)

let map_insert h i x =
  partial_alter h (fun _ -> Some x) i

(** val map_to_list : ('a1, 'a2, 'a3) mapFold -> 'a3 -> ('a1 * 'a2) list **)

let map_to_list h =
  map_fold h (fun i x x0 -> (i, x) :: x0) []

type 'a gmap_dep_ne =
| GNode001 of 'a gmap_dep_ne
| GNode010 of 'a
| GNode011 of 'a * 'a gmap_dep_ne
| GNode100 of 'a gmap_dep_ne
| GNode101 of 'a gmap_dep_ne * 'a gmap_dep_ne
| GNode110 of 'a gmap_dep_ne * 'a
| GNode111 of 'a gmap_dep_ne * 'a * 'a gmap_dep_ne

type 'a gmap_dep =
| GEmpty
| GNodes of 'a gmap_dep_ne

type ('k, 'a) gmap = { gmap_car : 'a gmap_dep }

(** val gmap_dep_ne_case :
    'a1 gmap_dep_ne -> ('a1 gmap_dep -> (__ * 'a1) option -> 'a1 gmap_dep ->
    'a2) -> 'a2 **)

let gmap_dep_ne_case t f =
  match t with
  | GNode001 r -> f GEmpty None (GNodes r)
  | GNode010 x -> f GEmpty (Some (__, x)) GEmpty
  | GNode011 (x, r) -> f GEmpty (Some (__, x)) (GNodes r)
  | GNode100 l -> f (GNodes l) None GEmpty
  | GNode101 (l, r) -> f (GNodes l) None (GNodes r)
  | GNode110 (l, x) -> f (GNodes l) (Some (__, x)) GEmpty
  | GNode111 (l, x, r) -> f (GNodes l) (Some (__, x)) (GNodes r)

(** val gmap_dep_ne_lookup : positive -> 'a1 gmap_dep_ne -> 'a1 option **)

let rec gmap_dep_ne_lookup i = function
| GNode001 r -> (match i with
                 | XI i0 -> gmap_dep_ne_lookup i0 r
                 | _ -> None)
| GNode010 x -> (match i with
                 | XH -> Some x
                 | _ -> None)
| GNode011 (x, r) ->
  (match i with
   | XI i0 -> gmap_dep_ne_lookup i0 r
   | XO _ -> None
   | XH -> Some x)
| GNode100 l -> (match i with
                 | XO i0 -> gmap_dep_ne_lookup i0 l
                 | _ -> None)
| GNode101 (l, r) ->
  (match i with
   | XI i0 -> gmap_dep_ne_lookup i0 r
   | XO i0 -> gmap_dep_ne_lookup i0 l
   | XH -> None)
| GNode110 (l, x) ->
  (match i with
   | XI _ -> None
   | XO i0 -> gmap_dep_ne_lookup i0 l
   | XH -> Some x)
| GNode111 (l, x, r) ->
  (match i with
   | XI i0 -> gmap_dep_ne_lookup i0 r
   | XO i0 -> gmap_dep_ne_lookup i0 l
   | XH -> Some x)

(** val gmap_dep_lookup : positive -> 'a1 gmap_dep -> 'a1 option **)

let gmap_dep_lookup i = function
| GEmpty -> None
| GNodes t -> gmap_dep_ne_lookup i t

(** val gmap_lookup :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2, ('a1, 'a2) gmap)
    lookup **)

let gmap_lookup _ h k mt =
  gmap_dep_lookup (h.encode k) mt.gmap_car

(** val gmap_empty :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2) gmap empty **)

let gmap_empty _ _ =
  { gmap_car = GEmpty }

(** val gmap_dep_ne_singleton : positive -> 'a1 -> 'a1 gmap_dep_ne **)

let rec gmap_dep_ne_singleton i x =
  match i with
  | XI i0 -> GNode001 (gmap_dep_ne_singleton i0 x)
  | XO i0 -> GNode100 (gmap_dep_ne_singleton i0 x)
  | XH -> GNode010 x

(** val gmap_partial_alter_aux :
    (positive -> __ -> 'a1 gmap_dep_ne -> 'a1 gmap_dep) -> ('a1 option -> 'a1
    option) -> positive -> 'a1 gmap_dep -> 'a1 gmap_dep **)

let gmap_partial_alter_aux go f i = function
| GEmpty ->
  (match f None with
   | Some x -> GNodes (gmap_dep_ne_singleton i x)
   | None -> GEmpty)
| GNodes t -> go i __ t

(** val gmap_dep_ne_partial_alter :
    ('a1 option -> 'a1 option) -> positive -> 'a1 gmap_dep_ne -> 'a1 gmap_dep **)

let rec gmap_dep_ne_partial_alter f i = function
| GNode001 r ->
  (match i with
   | XI i0 ->
     (match gmap_dep_ne_partial_alter f i0 r with
      | GEmpty -> GEmpty
      | GNodes r0 -> GNodes (GNode001 r0))
   | XO i0 ->
     (match f None with
      | Some x0 ->
        let l = gmap_dep_ne_singleton i0 x0 in GNodes (GNode101 (l, r))
      | None -> GNodes (GNode001 r))
   | XH ->
     (match f None with
      | Some a ->
        let p0 = (__, a) in let (_, x0) = p0 in GNodes (GNode011 (x0, r))
      | None -> GNodes (GNode001 r)))
| GNode010 x0 ->
  (match i with
   | XI i0 ->
     (match f None with
      | Some x1 ->
        let r = gmap_dep_ne_singleton i0 x1 in GNodes (GNode011 (x0, r))
      | None -> GNodes (GNode010 x0))
   | XO i0 ->
     (match f None with
      | Some x1 ->
        let l = gmap_dep_ne_singleton i0 x1 in GNodes (GNode110 (l, x0))
      | None -> GNodes (GNode010 x0))
   | XH ->
     (match f (Some x0) with
      | Some a -> let p0 = (__, a) in let (_, x1) = p0 in GNodes (GNode010 x1)
      | None -> GEmpty))
| GNode011 (x0, r) ->
  (match i with
   | XI i0 ->
     (match gmap_dep_ne_partial_alter f i0 r with
      | GEmpty -> GNodes (GNode010 x0)
      | GNodes r0 -> GNodes (GNode011 (x0, r0)))
   | XO i0 ->
     (match f None with
      | Some x1 ->
        let l = gmap_dep_ne_singleton i0 x1 in GNodes (GNode111 (l, x0, r))
      | None -> GNodes (GNode011 (x0, r)))
   | XH ->
     (match f (Some x0) with
      | Some a ->
        let p0 = (__, a) in let (_, x1) = p0 in GNodes (GNode011 (x1, r))
      | None -> GNodes (GNode001 r)))
| GNode100 l ->
  (match i with
   | XI i0 ->
     (match f None with
      | Some x0 ->
        let r = gmap_dep_ne_singleton i0 x0 in GNodes (GNode101 (l, r))
      | None -> GNodes (GNode100 l))
   | XO i0 ->
     (match gmap_dep_ne_partial_alter f i0 l with
      | GEmpty -> GEmpty
      | GNodes l0 -> GNodes (GNode100 l0))
   | XH ->
     (match f None with
      | Some a ->
        let p0 = (__, a) in let (_, x0) = p0 in GNodes (GNode110 (l, x0))
      | None -> GNodes (GNode100 l)))
| GNode101 (l, r) ->
  (match i with
   | XI i0 ->
     (match gmap_dep_ne_partial_alter f i0 r with
      | GEmpty -> GNodes (GNode100 l)
      | GNodes r0 -> GNodes (GNode101 (l, r0)))
   | XO i0 ->
     (match gmap_dep_ne_partial_alter f i0 l with
      | GEmpty -> GNodes (GNode001 r)
      | GNodes l0 -> GNodes (GNode101 (l0, r)))
   | XH ->
     (match f None with
      | Some a ->
        let p0 = (__, a) in let (_, x0) = p0 in GNodes (GNode111 (l, x0, r))
      | None -> GNodes (GNode101 (l, r))))
| GNode110 (l, x0) ->
  (match i with
   | XI i0 ->
     (match f None with
      | Some x1 ->
        let r = gmap_dep_ne_singleton i0 x1 in GNodes (GNode111 (l, x0, r))
      | None -> GNodes (GNode110 (l, x0)))
   | XO i0 ->
     (match gmap_dep_ne_partial_alter f i0 l with
      | GEmpty -> GNodes (GNode010 x0)
      | GNodes l0 -> GNodes (GNode110 (l0, x0)))
   | XH ->
     (match f (Some x0) with
      | Some a ->
        let p0 = (__, a) in let (_, x1) = p0 in GNodes (GNode110 (l, x1))
      | None -> GNodes (GNode100 l)))
| GNode111 (l, x0, r) ->
  (match i with
   | XI i0 ->
     (match gmap_dep_ne_partial_alter f i0 r with
      | GEmpty -> GNodes (GNode110 (l, x0))
      | GNodes r0 -> GNodes (GNode111 (l, x0, r0)))
   | XO i0 ->
     (match gmap_dep_ne_partial_alter f i0 l with
      | GEmpty -> GNodes (GNode011 (x0, r))
      | GNodes l0 -> GNodes (GNode111 (l0, x0, r)))
   | XH ->
     (match f (Some x0) with
      | Some a ->
        let p0 = (__, a) in let (_, x1) = p0 in GNodes (GNode111 (l, x1, r))
      | None -> GNodes (GNode101 (l, r))))

(** val gmap_dep_partial_alter :
    ('a1 option -> 'a1 option) -> positive -> 'a1 gmap_dep -> 'a1 gmap_dep **)

let gmap_dep_partial_alter f i x =
  gmap_partial_alter_aux (fun x0 _ -> gmap_dep_ne_partial_alter f x0) f i x

(** val gmap_partial_alter :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2, ('a1, 'a2) gmap)
    partialAlter **)

let gmap_partial_alter _ h f k pat =
  let { gmap_car = mt } = pat in
  { gmap_car = (gmap_dep_partial_alter f (h.encode k) mt) }

(** val gmap_fold_aux :
    (positive -> 'a2 -> 'a1 gmap_dep_ne -> 'a2) -> positive -> 'a2 -> 'a1
    gmap_dep -> 'a2 **)

let gmap_fold_aux go i y = function
| GEmpty -> y
| GNodes t -> go i y t

(** val gmap_dep_ne_fold :
    (positive -> 'a1 -> 'a2 -> 'a2) -> positive -> 'a2 -> 'a1 gmap_dep_ne ->
    'a2 **)

let rec gmap_dep_ne_fold f x x0 x1 =
  gmap_dep_ne_case x1 (fun ml mx mr ->
    gmap_fold_aux (fun x2 x3 x4 -> gmap_dep_ne_fold f x2 x3 x4) (XI x)
      (gmap_fold_aux (fun x2 x3 x4 -> gmap_dep_ne_fold f x2 x3 x4) (XO x)
        (match mx with
         | Some p0 -> let (_, x2) = p0 in f (Coq0_Pos.reverse x) x2 x0
         | None -> x0)
        ml)
      mr)

(** val gmap_dep_fold :
    (positive -> 'a1 -> 'a2 -> 'a2) -> positive -> 'a2 -> 'a1 gmap_dep -> 'a2 **)

let gmap_dep_fold f =
  gmap_fold_aux (gmap_dep_ne_fold f)

(** val gmap_fold :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1 -> 'a2 -> __ -> __) -> __
    -> ('a1, 'a2) gmap -> __ **)

let gmap_fold _ h f y pat =
  let { gmap_car = mt } = pat in
  gmap_dep_fold (fun i x ->
    match h.decode i with
    | Some k -> f k x
    | None -> id) XH y mt

module Coq_finz =
 struct
  type finz =
  | FinZ of z

  (** val to_z : z -> finz -> z **)

  let to_z _ = function
  | FinZ f0 -> f0

  (** val of_z : z -> z -> finz option **)

  let of_z finz_bound z0 =
    let d = Coq_Z.lt_dec z0 finz_bound in
    if d
    then let d0 = Coq_Z.le_dec Z0 z0 in if d0 then Some (FinZ z0) else None
    else None

  (** val leb : z -> finz -> finz -> bool **)

  let leb finz_bound f1 f2 =
    Z.leb (to_z finz_bound f1) (to_z finz_bound f2)

  (** val ltb : z -> finz -> finz -> bool **)

  let ltb finz_bound f1 f2 =
    Z.ltb (to_z finz_bound f1) (to_z finz_bound f2)

  (** val incr : z -> finz -> z -> finz option **)

  let incr finz_bound f off =
    let z0 = Z.add (to_z finz_bound f) off in
    let filtered_var = Coq_Z.lt_dec z0 finz_bound in
    if filtered_var
    then let filtered_var0 = Coq_Z.le_dec Z0 z0 in
         if filtered_var0 then Some (FinZ z0) else None
    else None
 end

(** val finz_eq_dec : z -> (Coq_finz.finz, Coq_finz.finz) relDecision **)

let finz_eq_dec _ x y =
  let Coq_finz.FinZ z0 = x in let Coq_finz.FinZ z1 = y in Coq_Z.eq_dec z0 z1

(** val finz_countable : z -> Coq_finz.finz countable **)

let finz_countable finz_bound =
  { encode = (fun r -> z_countable.encode (Coq_finz.to_z finz_bound r));
    decode = (fun n0 ->
    match z_countable.decode n0 with
    | Some z0 -> Coq_finz.of_z finz_bound z0
    | None -> None) }

(** val memNum : z **)

let memNum =
  Zpos (XO (XO (XO (XO (XO (XO (XO (XI (XO (XO (XI (XO (XO (XO (XO (XI (XO
    (XI (XI (XI XH))))))))))))))))))))

(** val withinBounds :
    z -> Coq_finz.finz -> Coq_finz.finz -> Coq_finz.finz -> bool **)

let withinBounds z0 b e a =
  (&&) (Coq_finz.leb z0 b a) (Coq_finz.ltb z0 a e)

(** val isWithin :
    z -> Coq_finz.finz -> Coq_finz.finz -> Coq_finz.finz -> Coq_finz.finz ->
    bool **)

let isWithin z0 n1 n2 b e =
  (&&) (Coq_finz.leb z0 b n1) (Coq_finz.leb z0 n2 e)

(** val oNum : z **)

let oNum =
  Zpos (XO (XO (XO (XO (XO (XO (XO (XI (XO (XO (XI (XO (XO (XO (XO (XI (XO
    (XI (XI (XI XH))))))))))))))))))))

(** val regNum : nat **)

let regNum =
  S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S (S
    (S (S (S (S (S (S O))))))))))))))))))))))))))))))

type regName =
| PC
| R of nat

type sRegName =
| MTDC

(** val reg_eq_dec : (regName, regName) relDecision **)

let reg_eq_dec r1 r2 =
  match r1 with
  | PC -> (match r2 with
           | PC -> true
           | R _ -> false)
  | R n0 -> (match r2 with
             | PC -> false
             | R n1 -> Coq_Nat.eq_dec n0 n1)

(** val n_to_regname : nat -> regName option **)

let n_to_regname n0 =
  let filtered_var = Coq_Nat.le_dec n0 regNum in
  if filtered_var then Some (R n0) else None

(** val reg_countable : regName countable **)

let reg_countable =
  { encode = (fun r ->
    (sum_countable unit_eq_dec unit_countable Coq_Nat.eq_dec nat_countable).encode
      (match r with
       | PC -> Inl ()
       | R n0 -> Inr n0));
    decode = (fun n0 ->
    match (sum_countable unit_eq_dec unit_countable Coq_Nat.eq_dec
            nat_countable).decode n0 with
    | Some y -> (match y with
                 | Inl _ -> Some PC
                 | Inr n1 -> n_to_regname n1)
    | None -> None) }

(** val sreg_eq_dec : (sRegName, sRegName) relDecision **)

let sreg_eq_dec _ _ =
  true

(** val sreg_countable : sRegName countable **)

let sreg_countable =
  { encode = (fun _ ->
    (option_countable unit_eq_dec unit_countable).encode (Some ())); decode =
    (fun n0 ->
    match unit_countable.decode n0 with
    | Some _ -> Some MTDC
    | None -> None) }

(** val cnull : regName **)

let cnull =
  R O

type rXperm =
| Orx
| R0
| X
| XSR

type wperm =
| Ow
| W
| WL

type dLperm =
| LG
| DL

type dROperm =
| LM
| DRO

type perm =
| BPerm of rXperm * wperm * dLperm * dROperm

type locality =
| Global
| Local

type sealPerms = bool * bool

(** val permit_seal : sealPerms -> bool **)

let permit_seal =
  fst

(** val permit_unseal : sealPerms -> bool **)

let permit_unseal =
  snd

(** val executeAllowed : perm -> bool **)

let executeAllowed = function
| BPerm (rx, _, _, _) -> (match rx with
                          | Orx -> false
                          | R0 -> false
                          | _ -> true)

(** val readAllowed : perm -> bool **)

let readAllowed = function
| BPerm (rx, _, _, _) -> (match rx with
                          | Orx -> false
                          | _ -> true)

(** val writeAllowed : perm -> bool **)

let writeAllowed = function
| BPerm (_, w, _, _) -> (match w with
                         | Ow -> false
                         | _ -> true)

(** val has_sreg_access : perm -> bool **)

let has_sreg_access = function
| BPerm (rx, _, _, _) -> (match rx with
                          | XSR -> true
                          | _ -> false)

(** val isWL : perm -> bool **)

let isWL = function
| BPerm (_, w, _, _) -> (match w with
                         | WL -> true
                         | _ -> false)

(** val isDL : perm -> bool **)

let isDL = function
| BPerm (_, _, dl, _) -> (match dl with
                          | LG -> false
                          | DL -> true)

(** val isDRO : perm -> bool **)

let isDRO = function
| BPerm (_, _, _, dro) -> (match dro with
                           | LM -> false
                           | DRO -> true)

(** val isLocal : locality -> bool **)

let isLocal = function
| Global -> false
| Local -> true

(** val rXPermFlowsTo : rXperm -> rXperm -> bool **)

let rXPermFlowsTo rx1 rx2 =
  match rx1 with
  | Orx -> true
  | R0 -> (match rx2 with
           | Orx -> false
           | _ -> true)
  | X -> (match rx2 with
          | Orx -> false
          | R0 -> false
          | _ -> true)
  | XSR -> (match rx2 with
            | XSR -> true
            | _ -> false)

(** val wPermFlowsTo : wperm -> wperm -> bool **)

let wPermFlowsTo w1 w2 =
  match w1 with
  | Ow -> true
  | W -> (match w2 with
          | Ow -> false
          | _ -> true)
  | WL -> (match w2 with
           | WL -> true
           | _ -> false)

(** val dLPermFlowsTo : dLperm -> dLperm -> bool **)

let dLPermFlowsTo dl1 dl2 =
  match dl1 with
  | LG -> (match dl2 with
           | LG -> true
           | DL -> false)
  | DL -> true

(** val dROPermFlowsTo : dROperm -> dROperm -> bool **)

let dROPermFlowsTo dro1 dro2 =
  match dro1 with
  | LM -> (match dro2 with
           | LM -> true
           | DRO -> false)
  | DRO -> true

(** val permFlowsTo : perm -> perm -> bool **)

let permFlowsTo p1 p2 =
  let BPerm (rx1, w1, dl1, dro1) = p1 in
  let BPerm (rx2, w2, dl2, dro2) = p2 in
  (&&)
    ((&&) ((&&) (rXPermFlowsTo rx1 rx2) (wPermFlowsTo w1 w2))
      (dLPermFlowsTo dl1 dl2))
    (dROPermFlowsTo dro1 dro2)

(** val localityFlowsTo : locality -> locality -> bool **)

let localityFlowsTo l1 l2 =
  match l1 with
  | Global -> (match l2 with
               | Global -> true
               | Local -> false)
  | Local -> true

(** val sealPermFlowsTo : sealPerms -> sealPerms -> bool **)

let sealPermFlowsTo s1 s2 =
  (&&) (if permit_seal s1 then permit_seal s2 else true)
    (if permit_unseal s1 then permit_unseal s2 else true)

type sealable =
| SCap of perm * locality * Coq_finz.finz * Coq_finz.finz * Coq_finz.finz
| SSealRange of sealPerms * locality * Coq_finz.finz * Coq_finz.finz
   * Coq_finz.finz

type word =
| WInt of z
| WSealable of sealable
| WSentry of perm * locality * Coq_finz.finz * Coq_finz.finz * Coq_finz.finz
| WSealed of Coq_finz.finz * sealable

(** val isLocalSealable : sealable -> bool **)

let isLocalSealable = function
| SCap (_, l, _, _, _) -> isLocal l
| SSealRange (_, l, _, _, _) -> isLocal l

(** val isLocalWord : word -> bool **)

let isLocalWord = function
| WInt _ -> false
| WSealable sb -> isLocalSealable sb
| WSentry (_, l, _, _, _) -> isLocal l
| WSealed (_, sb) -> isLocalSealable sb

(** val canStore : perm -> word -> bool **)

let canStore p w =
  if isLocalWord w then isWL p else writeAllowed p

(** val updatePcPerm : word -> word **)

let updatePcPerm w = match w with
| WSentry (p, g, b, e, a) -> WSealable (SCap (p, g, b, e, a))
| _ -> w

(** val nonZero : word -> bool **)

let nonZero = function
| WInt n0 -> negb (Z.eqb n0 Z0)
| _ -> true

(** val deeplocal_perm : perm -> perm **)

let deeplocal_perm = function
| BPerm (rx, w, _, dro) -> BPerm (rx, w, DL, dro)

(** val deeplocal_sb : sealable -> sealable **)

let deeplocal_sb = function
| SCap (p, g, b, e, a) -> SCap ((deeplocal_perm p), g, b, e, a)
| SSealRange (sp, g, b, e, a) -> SSealRange (sp, g, b, e, a)

(** val deeplocal : word -> word **)

let deeplocal w = match w with
| WSealable sb -> WSealable (deeplocal_sb sb)
| _ -> w

(** val borrow_sb : sealable -> sealable **)

let borrow_sb = function
| SCap (p, _, b, e, a) -> SCap (p, Local, b, e, a)
| SSealRange (sp, _, b, e, a) -> SSealRange (sp, Local, b, e, a)

(** val borrow : word -> word **)

let borrow w = match w with
| WInt _ -> w
| WSealable sb -> WSealable (borrow_sb sb)
| WSentry (p, _, b, e, a) -> WSentry (p, Local, b, e, a)
| WSealed (ot, sb) -> WSealed (ot, (borrow_sb sb))

(** val readonly_perm : perm -> perm **)

let readonly_perm = function
| BPerm (rx, _, dl, _) -> BPerm (rx, Ow, dl, DRO)

(** val readonly_sb : sealable -> sealable **)

let readonly_sb sb = match sb with
| SCap (p, g, b, e, a) -> SCap ((readonly_perm p), g, b, e, a)
| SSealRange (_, _, _, _, _) -> sb

(** val readonly : word -> word **)

let readonly w = match w with
| WSealable sb -> WSealable (readonly_sb sb)
| _ -> w

(** val load_word : perm -> word -> word **)

let load_word p w =
  let borrow_w = if isDL p then deeplocal (borrow w) else w in
  if isDRO p then readonly borrow_w else borrow_w

type mem = (Coq_finz.finz, word) gmap

type reg = (regName, word) gmap

type sReg = (sRegName, word) gmap

(** val lookup_reg : regName -> reg -> word option **)

let lookup_reg r regs =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun w -> Some
    (if decide (decide_rel reg_eq_dec r cnull) then WInt Z0 else w))
    (lookup0 (gmap_lookup reg_eq_dec reg_countable) r regs)

(** val insert_reg : regName -> word -> reg -> reg **)

let insert_reg r w regs =
  let w' = if decide (decide_rel reg_eq_dec r cnull) then WInt Z0 else w in
  insert0 (map_insert (gmap_partial_alter reg_eq_dec reg_countable)) r w' regs

type instr =
| Jmp of (z, regName) sum
| Jnz of (z, regName) sum * regName
| Jalr of regName * regName
| Mov of regName * (z, regName) sum
| Load of regName * regName
| Store of regName * (z, regName) sum
| Lt0 of regName * (z, regName) sum * (z, regName) sum
| Add of regName * (z, regName) sum * (z, regName) sum
| Sub of regName * (z, regName) sum * (z, regName) sum
| Mul of regName * (z, regName) sum * (z, regName) sum
| LAnd of regName * (z, regName) sum * (z, regName) sum
| LOr of regName * (z, regName) sum * (z, regName) sum
| LShiftL of regName * (z, regName) sum * (z, regName) sum
| LShiftR of regName * (z, regName) sum * (z, regName) sum
| Lea of regName * (z, regName) sum
| Restrict of regName * (z, regName) sum
| Subseg of regName * (z, regName) sum * (z, regName) sum
| GetB of regName * regName
| GetE of regName * regName
| GetA of regName * regName
| GetP of regName * regName
| GetL of regName * regName
| GetWType of regName * regName
| GetOType of regName * regName
| Seal of regName * regName * regName
| UnSeal of regName * regName * regName
| ReadSR of regName * sRegName
| WriteSR of sRegName * regName
| Fail
| Halt

type machineParameters = { decodeInstr : (z -> instr);
                           encodeInstr : (instr -> z);
                           encodePerm : (perm -> z);
                           decodePerm : (z -> perm);
                           encodeLoc : (locality -> z);
                           decodePermPair : (z -> perm * locality);
                           encodePermPair : ((perm * locality) -> z);
                           encodeSealPerms : (sealPerms -> z);
                           decodeSealPerms : (z -> sealPerms);
                           decodeSealPermPair : (z -> sealPerms * locality);
                           encodeSealPermPair : ((sealPerms * locality) -> z);
                           encodeWordType : (word -> z);
                           decodeWordType : (z -> word) }

(** val decodeInstrW : machineParameters -> word -> instr **)

let decodeInstrW h = function
| WInt z0 -> h.decodeInstr z0
| _ -> Fail

type execConf = (reg * sReg) * mem

type confFlag =
| Executable
| Halted
| Failed
| NextI

type conf = confFlag * execConf

(** val reg0 : execConf -> reg **)

let reg0 _UU03d5_ =
  fst (fst _UU03d5_)

(** val sreg : execConf -> sReg **)

let sreg _UU03d5_ =
  snd (fst _UU03d5_)

(** val mem0 : execConf -> mem **)

let mem0 =
  snd

(** val update_reg : execConf -> regName -> word -> execConf **)

let update_reg _UU03c6_ r w =
  (((insert_reg r w (reg0 _UU03c6_)), (sreg _UU03c6_)), (mem0 _UU03c6_))

(** val update_sreg : execConf -> sRegName -> word -> execConf **)

let update_sreg _UU03c6_ sr w =
  (((reg0 _UU03c6_),
    (insert0 (map_insert (gmap_partial_alter sreg_eq_dec sreg_countable)) sr
      w (sreg _UU03c6_))),
    (mem0 _UU03c6_))

(** val update_mem : execConf -> Coq_finz.finz -> word -> execConf **)

let update_mem _UU03c6_ a w =
  (((reg0 _UU03c6_), (sreg _UU03c6_)),
    (insert0
      (map_insert
        (gmap_partial_alter (finz_eq_dec memNum) (finz_countable memNum)))
      a w (mem0 _UU03c6_)))

(** val updatePC_gen : execConf -> z -> conf option **)

let updatePC_gen _UU03c6_ imm =
  match lookup0 (gmap_lookup reg_eq_dec reg_countable) PC (reg0 _UU03c6_) with
  | Some y ->
    (match y with
     | WSealable sb ->
       (match sb with
        | SCap (p, g, b, e, a) ->
          (match Coq_finz.incr memNum a imm with
           | Some a' ->
             let _UU03c6_' =
               update_reg _UU03c6_ PC (WSealable (SCap (p, g, b, e, a')))
             in
             Some (NextI, _UU03c6_')
           | None -> None)
        | SSealRange (_, _, _, _, _) -> None)
     | _ -> None)
  | None -> None

(** val updatePC : execConf -> conf option **)

let updatePC _UU03c6_ =
  updatePC_gen _UU03c6_ (Zpos XH)

(** val z_of_argument : reg -> (z, regName) sum -> z option **)

let z_of_argument regs = function
| Inl z0 -> Some z0
| Inr r ->
  (match lookup_reg r regs with
   | Some w -> (match w with
                | WInt z0 -> Some z0
                | _ -> None)
   | None -> None)

(** val word_of_argument : reg -> (z, regName) sum -> word option **)

let word_of_argument regs = function
| Inl n0 -> Some (WInt n0)
| Inr r -> lookup_reg r regs

(** val addr_of_argument : reg -> (z, regName) sum -> Coq_finz.finz option **)

let addr_of_argument regs src =
  match z_of_argument regs src with
  | Some n0 -> Coq_finz.of_z memNum n0
  | None -> None

(** val otype_of_argument :
    reg -> (z, regName) sum -> Coq_finz.finz option **)

let otype_of_argument regs src =
  match z_of_argument regs src with
  | Some n0 -> Coq_finz.of_z oNum n0
  | None -> None

(** val exec_opt :
    machineParameters -> instr -> perm -> execConf -> conf option **)

let exec_opt h i plevel _UU03c6_ =
  match i with
  | Jmp rimm ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun imm ->
      updatePC_gen _UU03c6_ imm)
      (Obj.magic z_of_argument (reg0 _UU03c6_) rimm)
  | Jnz (rimm, rcond) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun wcond ->
      if nonZero wcond
      then mbind (Obj.magic (fun _ _ -> option_bind)) (fun imm ->
             updatePC_gen _UU03c6_ imm)
             (Obj.magic z_of_argument (reg0 _UU03c6_) rimm)
      else updatePC _UU03c6_) (Obj.magic lookup_reg rcond (reg0 _UU03c6_))
  | Jalr (rdst, rsrc) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun wrsrc ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun wpc ->
        match wpc with
        | WSealable sb ->
          (match sb with
           | SCap (p, g, b, e, a) ->
             (match Coq_finz.incr memNum a (Zpos XH) with
              | Some a' ->
                let _UU03c6__next =
                  update_reg _UU03c6_ PC (updatePcPerm wrsrc)
                in
                let _UU03c6__dst =
                  update_reg _UU03c6__next rdst (WSentry (p, g, b, e, a'))
                in
                Some (NextI, _UU03c6__dst)
              | None -> None)
           | SSealRange (_, _, _, _, _) -> None)
        | _ -> None)
        (lookup0 (Obj.magic gmap_lookup reg_eq_dec reg_countable) PC
          (reg0 _UU03c6_)))
      (Obj.magic lookup_reg rsrc (reg0 _UU03c6_))
  | Mov (dst, _UU03c1_) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun tomov ->
      updatePC (update_reg _UU03c6_ dst tomov))
      (Obj.magic word_of_argument (reg0 _UU03c6_) _UU03c1_)
  | Load (dst, src) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun wsrc ->
      match wsrc with
      | WSealable sb ->
        (match sb with
         | SCap (p, _, b, e, a) ->
           if (&&) (readAllowed p) (withinBounds memNum b e a)
           then mbind (Obj.magic (fun _ _ -> option_bind)) (fun asrc ->
                  updatePC (update_reg _UU03c6_ dst (load_word p asrc)))
                  (lookup0
                    (Obj.magic gmap_lookup (finz_eq_dec memNum)
                      (finz_countable memNum))
                    a (mem0 _UU03c6_))
           else None
         | SSealRange (_, _, _, _, _) -> None)
      | _ -> None) (Obj.magic lookup_reg src (reg0 _UU03c6_))
  | Store (dst, _UU03c1_) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun tostore ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun wdst ->
        match wdst with
        | WSealable sb ->
          (match sb with
           | SCap (p, _, b, e, a) ->
             if (&&) ((&&) (writeAllowed p) (withinBounds memNum b e a))
                  (canStore p tostore)
             then updatePC (update_mem _UU03c6_ a tostore)
             else None
           | SSealRange (_, _, _, _, _) -> None)
        | _ -> None) (Obj.magic lookup_reg dst (reg0 _UU03c6_)))
      (Obj.magic word_of_argument (reg0 _UU03c6_) _UU03c1_)
  | Lt0 (dst, _UU03c1_1, _UU03c1_2) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun n1 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun n2 ->
        updatePC (update_reg _UU03c6_ dst (WInt (Z.b2z (Z.ltb n1 n2)))))
        (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_2))
      (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_1)
  | Add (dst, _UU03c1_1, _UU03c1_2) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun n1 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun n2 ->
        updatePC (update_reg _UU03c6_ dst (WInt (Z.add n1 n2))))
        (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_2))
      (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_1)
  | Sub (dst, _UU03c1_1, _UU03c1_2) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun n1 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun n2 ->
        updatePC (update_reg _UU03c6_ dst (WInt (Z.sub n1 n2))))
        (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_2))
      (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_1)
  | Mul (dst, _UU03c1_1, _UU03c1_2) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun n1 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun n2 ->
        updatePC (update_reg _UU03c6_ dst (WInt (Z.mul n1 n2))))
        (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_2))
      (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_1)
  | LAnd (dst, _UU03c1_1, _UU03c1_2) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun n1 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun n2 ->
        updatePC (update_reg _UU03c6_ dst (WInt (Z.coq_land n1 n2))))
        (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_2))
      (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_1)
  | LOr (dst, _UU03c1_1, _UU03c1_2) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun n1 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun n2 ->
        updatePC (update_reg _UU03c6_ dst (WInt (Z.coq_lor n1 n2))))
        (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_2))
      (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_1)
  | LShiftL (dst, _UU03c1_1, _UU03c1_2) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun n1 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun n2 ->
        updatePC (update_reg _UU03c6_ dst (WInt (Z.shiftl n1 n2))))
        (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_2))
      (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_1)
  | LShiftR (dst, _UU03c1_1, _UU03c1_2) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun n1 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun n2 ->
        updatePC (update_reg _UU03c6_ dst (WInt (Z.shiftr n1 n2))))
        (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_2))
      (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_1)
  | Lea (dst, _UU03c1_) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun n0 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun wdst ->
        match wdst with
        | WSealable sb ->
          (match sb with
           | SCap (p, g, b, e, a) ->
             (match Coq_finz.incr memNum a n0 with
              | Some a' ->
                updatePC
                  (update_reg _UU03c6_ dst (WSealable (SCap (p, g, b, e,
                    a'))))
              | None -> None)
           | SSealRange (p, g, b, e, a) ->
             (match Coq_finz.incr oNum a n0 with
              | Some a' ->
                updatePC
                  (update_reg _UU03c6_ dst (WSealable (SSealRange (p, g, b,
                    e, a'))))
              | None -> None))
        | _ -> None) (Obj.magic lookup_reg dst (reg0 _UU03c6_)))
      (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_)
  | Restrict (dst, _UU03c1_) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun n0 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun wdst ->
        match wdst with
        | WSealable sb ->
          (match sb with
           | SCap (p, g, b, e, a) ->
             let (p', g') = h.decodePermPair n0 in
             if (&&) (permFlowsTo p' p) (localityFlowsTo g' g)
             then updatePC
                    (update_reg _UU03c6_ dst (WSealable (SCap (p', g', b, e,
                      a))))
             else None
           | SSealRange (p, g, b, e, a) ->
             let (p', g') = h.decodeSealPermPair n0 in
             if (&&) (sealPermFlowsTo p' p) (localityFlowsTo g' g)
             then updatePC
                    (update_reg _UU03c6_ dst (WSealable (SSealRange (p', g',
                      b, e, a))))
             else None)
        | _ -> None) (Obj.magic lookup_reg dst (reg0 _UU03c6_)))
      (Obj.magic z_of_argument (reg0 _UU03c6_) _UU03c1_)
  | Subseg (dst, _UU03c1_1, _UU03c1_2) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun wdst ->
      match wdst with
      | WSealable sb ->
        (match sb with
         | SCap (p, g, b, e, a) ->
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun a1 ->
             mbind (Obj.magic (fun _ _ -> option_bind)) (fun a2 ->
               if isWithin memNum a1 a2 b e
               then updatePC
                      (update_reg _UU03c6_ dst (WSealable (SCap (p, g, a1,
                        a2, a))))
               else None)
               (Obj.magic addr_of_argument (reg0 _UU03c6_) _UU03c1_2))
             (Obj.magic addr_of_argument (reg0 _UU03c6_) _UU03c1_1)
         | SSealRange (p, g, b, e, a) ->
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun o1 ->
             mbind (Obj.magic (fun _ _ -> option_bind)) (fun o2 ->
               if isWithin oNum o1 o2 b e
               then updatePC
                      (update_reg _UU03c6_ dst (WSealable (SSealRange (p, g,
                        o1, o2, a))))
               else None)
               (Obj.magic otype_of_argument (reg0 _UU03c6_) _UU03c1_2))
             (Obj.magic otype_of_argument (reg0 _UU03c6_) _UU03c1_1))
      | _ -> None) (Obj.magic lookup_reg dst (reg0 _UU03c6_))
  | GetB (dst, r) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun wr ->
      match wr with
      | WSealable sb ->
        (match sb with
         | SCap (_, _, b, _, _) ->
           updatePC (update_reg _UU03c6_ dst (WInt (Coq_finz.to_z memNum b)))
         | SSealRange (_, _, b, _, _) ->
           updatePC (update_reg _UU03c6_ dst (WInt (Coq_finz.to_z oNum b))))
      | WSentry (_, _, b, _, _) ->
        updatePC (update_reg _UU03c6_ dst (WInt (Coq_finz.to_z memNum b)))
      | _ -> None) (Obj.magic lookup_reg r (reg0 _UU03c6_))
  | GetE (dst, r) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun wr ->
      match wr with
      | WSealable sb ->
        (match sb with
         | SCap (_, _, _, e, _) ->
           updatePC (update_reg _UU03c6_ dst (WInt (Coq_finz.to_z memNum e)))
         | SSealRange (_, _, _, e, _) ->
           updatePC (update_reg _UU03c6_ dst (WInt (Coq_finz.to_z oNum e))))
      | WSentry (_, _, _, e, _) ->
        updatePC (update_reg _UU03c6_ dst (WInt (Coq_finz.to_z memNum e)))
      | _ -> None) (Obj.magic lookup_reg r (reg0 _UU03c6_))
  | GetA (dst, r) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun wr ->
      match wr with
      | WSealable sb ->
        (match sb with
         | SCap (_, _, _, _, a) ->
           updatePC (update_reg _UU03c6_ dst (WInt (Coq_finz.to_z memNum a)))
         | SSealRange (_, _, _, _, a) ->
           updatePC (update_reg _UU03c6_ dst (WInt (Coq_finz.to_z oNum a))))
      | WSentry (_, _, _, _, a) ->
        updatePC (update_reg _UU03c6_ dst (WInt (Coq_finz.to_z memNum a)))
      | _ -> None) (Obj.magic lookup_reg r (reg0 _UU03c6_))
  | GetP (dst, r) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun wr ->
      match wr with
      | WSealable sb ->
        (match sb with
         | SCap (p, _, _, _, _) ->
           updatePC (update_reg _UU03c6_ dst (WInt (h.encodePerm p)))
         | SSealRange (p, _, _, _, _) ->
           updatePC (update_reg _UU03c6_ dst (WInt (h.encodeSealPerms p))))
      | WSentry (p, _, _, _, _) ->
        updatePC (update_reg _UU03c6_ dst (WInt (h.encodePerm p)))
      | _ -> None) (Obj.magic lookup_reg r (reg0 _UU03c6_))
  | GetL (dst, r) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun wr ->
      match wr with
      | WSealable sb ->
        (match sb with
         | SCap (_, l, _, _, _) ->
           updatePC (update_reg _UU03c6_ dst (WInt (h.encodeLoc l)))
         | SSealRange (_, l, _, _, _) ->
           updatePC (update_reg _UU03c6_ dst (WInt (h.encodeLoc l))))
      | WSentry (_, l, _, _, _) ->
        updatePC (update_reg _UU03c6_ dst (WInt (h.encodeLoc l)))
      | _ -> None) (Obj.magic lookup_reg r (reg0 _UU03c6_))
  | GetWType (dst, r) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun wr ->
      updatePC (update_reg _UU03c6_ dst (WInt (h.encodeWordType wr))))
      (Obj.magic lookup_reg r (reg0 _UU03c6_))
  | GetOType (dst, r) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun wr ->
      match wr with
      | WSealed (o, _) ->
        updatePC (update_reg _UU03c6_ dst (WInt (Coq_finz.to_z oNum o)))
      | _ -> updatePC (update_reg _UU03c6_ dst (WInt (Zneg XH))))
      (Obj.magic lookup_reg r (reg0 _UU03c6_))
  | Seal (dst, r1, r2) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun wr1 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun wr2 ->
        match wr1 with
        | WSealable sb0 ->
          (match sb0 with
           | SCap (_, _, _, _, _) -> None
           | SSealRange (p, _, b, e, a) ->
             (match wr2 with
              | WSealable sb ->
                if (&&) (permit_seal p) (withinBounds oNum b e a)
                then updatePC (update_reg _UU03c6_ dst (WSealed (a, sb)))
                else None
              | _ -> None))
        | _ -> None) (Obj.magic lookup_reg r2 (reg0 _UU03c6_)))
      (Obj.magic lookup_reg r1 (reg0 _UU03c6_))
  | UnSeal (dst, r1, r2) ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun wr1 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun wr2 ->
        match wr1 with
        | WSealable sb0 ->
          (match sb0 with
           | SCap (_, _, _, _, _) -> None
           | SSealRange (p, _, b, e, a) ->
             (match wr2 with
              | WSealed (a', sb) ->
                if decide
                     (and_dec (decide_rel bool_eq_dec (permit_unseal p) true)
                       (and_dec
                         (decide_rel bool_eq_dec (withinBounds oNum b e a)
                           true)
                         (decide_rel (finz_eq_dec oNum) a' a)))
                then updatePC (update_reg _UU03c6_ dst (WSealable sb))
                else None
              | _ -> None))
        | _ -> None) (Obj.magic lookup_reg r2 (reg0 _UU03c6_)))
      (Obj.magic lookup_reg r1 (reg0 _UU03c6_))
  | ReadSR (dst, src) ->
    if has_sreg_access plevel
    then mbind (Obj.magic (fun _ _ -> option_bind)) (fun tomov ->
           updatePC (update_reg _UU03c6_ dst tomov))
           (lookup0 (Obj.magic gmap_lookup sreg_eq_dec sreg_countable) src
             (sreg _UU03c6_))
    else None
  | WriteSR (dst, src) ->
    if has_sreg_access plevel
    then mbind (Obj.magic (fun _ _ -> option_bind)) (fun tomov ->
           updatePC (update_sreg _UU03c6_ dst tomov))
           (Obj.magic lookup_reg src (reg0 _UU03c6_))
    else None
  | Fail -> Some (Failed, _UU03c6_)
  | Halt -> Some (Halted, _UU03c6_)

(** val exec : machineParameters -> instr -> perm -> execConf -> conf **)

let exec h i plevel _UU03c6_ =
  match exec_opt h i plevel _UU03c6_ with
  | Some conf0 -> conf0
  | None -> (Failed, _UU03c6_)

(** val isCorrectPCb : word -> bool **)

let isCorrectPCb = function
| WSealable sb ->
  (match sb with
   | SCap (p, _, b, e, a) ->
     (&&) ((&&) (Coq_finz.leb memNum b a) (Coq_finz.ltb memNum a e))
       (executeAllowed p)
   | SSealRange (_, _, _, _, _) -> false)
| _ -> false

(** val machine_step : machineParameters -> conf -> conf option **)

let machine_step h = function
| (c0, phi) ->
  (match c0 with
   | Executable ->
     let (p, m) = phi in
     let (r, _) = p in
     (match lookup0 (gmap_lookup reg_eq_dec reg_countable) PC r with
      | Some pc ->
        if isCorrectPCb pc
        then (match pc with
              | WSealable sb ->
                (match sb with
                 | SCap (p0, _, _, _, a) ->
                   (match lookup0
                            (gmap_lookup (finz_eq_dec memNum)
                              (finz_countable memNum))
                            a m with
                    | Some wa -> Some (exec h (decodeInstrW h wa) p0 phi)
                    | None -> Some (Failed, phi))
                 | SSealRange (_, _, _, _, _) -> Some (Failed, phi))
              | _ -> Some (Failed, phi))
        else Some (Failed, phi)
      | None -> Some (Failed, phi))
   | _ -> None)

(** val reg_empty : reg **)

let reg_empty =
  empty0 (gmap_empty reg_eq_dec reg_countable)

(** val sreg_empty : sReg **)

let sreg_empty =
  empty0 (gmap_empty sreg_eq_dec sreg_countable)

(** val mem_empty : mem **)

let mem_empty =
  empty0 (gmap_empty (finz_eq_dec memNum) (finz_countable memNum))

(** val reg_lookup : reg -> regName -> word option **)

let reg_lookup rs r =
  lookup0 (gmap_lookup reg_eq_dec reg_countable) r rs

(** val sreg_lookup : sReg -> sRegName -> word option **)

let sreg_lookup srs sr =
  lookup0 (gmap_lookup sreg_eq_dec sreg_countable) sr srs

(** val mem_lookup : mem -> Coq_finz.finz -> word option **)

let mem_lookup m a =
  lookup0 (gmap_lookup (finz_eq_dec memNum) (finz_countable memNum)) a m

(** val reg_insert : reg -> regName -> word -> reg **)

let reg_insert rs r w =
  insert0 (map_insert (gmap_partial_alter reg_eq_dec reg_countable)) r w rs

(** val sreg_insert : sReg -> sRegName -> word -> sReg **)

let sreg_insert srs sr w =
  insert0 (map_insert (gmap_partial_alter sreg_eq_dec sreg_countable)) sr w
    srs

(** val mem_insert : mem -> Coq_finz.finz -> word -> mem **)

let mem_insert m a w =
  insert0
    (map_insert
      (gmap_partial_alter (finz_eq_dec memNum) (finz_countable memNum)))
    a w m

(** val reg_elements : reg -> (regName * word) list **)

let reg_elements rs =
  map_to_list (fun _ -> gmap_fold reg_eq_dec reg_countable) rs

(** val sreg_elements : sReg -> (sRegName * word) list **)

let sreg_elements srs =
  map_to_list (fun _ -> gmap_fold sreg_eq_dec sreg_countable) srs

(** val mem_elements : mem -> (Coq_finz.finz * word) list **)

let mem_elements m =
  map_to_list (fun _ ->
    gmap_fold (finz_eq_dec memNum) (finz_countable memNum)) m
