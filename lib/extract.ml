
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

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

(** val uncurry : ('a1 -> 'a2 -> 'a3) -> ('a1 * 'a2) -> 'a3 **)

let uncurry f = function
| (x, y) -> f x y

(** val prod_curry_subdef : ('a1 -> 'a2 -> 'a3) -> ('a1 * 'a2) -> 'a3 **)

let prod_curry_subdef =
  uncurry

(** val length : 'a1 list -> Big_int_Z.big_int **)

let rec length = function
| [] -> Big_int_Z.zero_big_int
| _ :: l' -> Big_int_Z.succ_big_int (length l')

(** val app : 'a1 list -> 'a1 list -> 'a1 list **)

let rec app l m =
  match l with
  | [] -> m
  | a :: l1 -> a :: (app l1 m)

(** val id : __ -> __ **)

let id x =
  x

module Coq__1 = struct
 (** val add : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)
 let rec add = Big_int_Z.add_big_int
end
include Coq__1

(** val mul : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

let rec mul = Big_int_Z.mult_big_int

(** val sub : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

let rec sub = (fun n m -> Big_int_Z.max_big_int Big_int_Z.zero_big_int
  (Big_int_Z.sub_big_int n m))

(** val compose : ('a2 -> 'a3) -> ('a1 -> 'a2) -> 'a1 -> 'a3 **)

let compose g f x =
  g (f x)

(** val flip : ('a1 -> 'a2 -> 'a3) -> 'a2 -> 'a1 -> 'a3 **)

let flip f x y =
  f y x

module Nat =
 struct
  (** val leb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let rec leb = Big_int_Z.le_big_int

  (** val ltb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let ltb n0 m =
    leb (Big_int_Z.succ_big_int n0) m

  (** val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let rec eq_dec = Big_int_Z.eq_big_int
 end

module Pos =
 struct
  (** val succ : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec succ = Big_int_Z.succ_big_int

  (** val add :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec add = Big_int_Z.add_big_int

  (** val add_carry :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  and add_carry x y =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q ->
        (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        (add_carry p q))
        (fun q -> Big_int_Z.mult_int_big_int 2 (add_carry p q))
        (fun _ ->
        (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        (succ p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q -> Big_int_Z.mult_int_big_int 2 (add_carry p q))
        (fun q ->
        (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        (add p q))
        (fun _ -> Big_int_Z.mult_int_big_int 2 (succ p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q ->
        (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        (succ q))
        (fun q -> Big_int_Z.mult_int_big_int 2 (succ q))
        (fun _ ->
        (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int)
        y)
      x

  (** val pred_double : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec pred_double x =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (Big_int_Z.mult_int_big_int 2 p))
      (fun p ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (pred_double p))
      (fun _ -> Big_int_Z.unit_big_int)
      x

  (** val pred : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let pred = (fun n -> Big_int_Z.max_big_int Big_int_Z.unit_big_int
  (Big_int_Z.pred_big_int n))

  (** val iter_op : ('a1 -> 'a1 -> 'a1) -> Big_int_Z.big_int -> 'a1 -> 'a1 **)

  let rec iter_op op p a =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p0 -> op a (iter_op op p0 (op a a)))
      (fun p0 -> iter_op op p0 (op a a))
      (fun _ -> a)
      p

  (** val to_nat : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let to_nat x =
    iter_op Coq__1.add x (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)

  (** val of_succ_nat : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec of_succ_nat n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> Big_int_Z.unit_big_int)
      (fun x -> succ (of_succ_nat x))
      n0

  (** val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let rec eq_dec p x0 =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun p1 -> eq_dec p0 p1)
        (fun _ -> false)
        (fun _ -> false)
        x0)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun _ -> false)
        (fun p1 -> eq_dec p0 p1)
        (fun _ -> false)
        x0)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun _ -> false)
        (fun _ -> false)
        (fun _ -> true)
        x0)
      p
 end

module N =
 struct
  (** val to_nat : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let to_nat a =
    (fun fO fp n -> if Big_int_Z.sign_big_int n <= 0 then fO () else fp n)
      (fun _ -> Big_int_Z.zero_big_int)
      (fun p -> Pos.to_nat p)
      a

  (** val of_nat : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let of_nat n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun n' -> (Pos.of_succ_nat n'))
      n0

  (** val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let eq_dec n0 m =
    (fun fO fp n -> if Big_int_Z.sign_big_int n <= 0 then fO () else fp n)
      (fun _ ->
      (fun fO fp n -> if Big_int_Z.sign_big_int n <= 0 then fO () else fp n)
        (fun _ -> true)
        (fun _ -> false)
        m)
      (fun p ->
      (fun fO fp n -> if Big_int_Z.sign_big_int n <= 0 then fO () else fp n)
        (fun _ -> false)
        (fun p0 -> Pos.eq_dec p p0)
        m)
      n0
 end

(** val nth_error : 'a1 list -> Big_int_Z.big_int -> 'a1 option **)

let rec nth_error l n0 =
  (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
    (fun _ -> match l with
              | [] -> None
              | x :: _ -> Some x)
    (fun n1 -> match l with
               | [] -> None
               | _ :: l0 -> nth_error l0 n1)
    n0

(** val rev : 'a1 list -> 'a1 list **)

let rec rev = function
| [] -> []
| x :: l' -> app (rev l') (x :: [])

(** val list_eq_dec : ('a1 -> 'a1 -> bool) -> 'a1 list -> 'a1 list -> bool **)

let rec list_eq_dec eq_dec0 l l' =
  match l with
  | [] -> (match l' with
           | [] -> true
           | _ :: _ -> false)
  | y :: l0 ->
    (match l' with
     | [] -> false
     | a :: l1 -> if eq_dec0 y a then list_eq_dec eq_dec0 l0 l1 else false)

(** val map : ('a1 -> 'a2) -> 'a1 list -> 'a2 list **)

let rec map f = function
| [] -> []
| a :: t -> (f a) :: (map f t)

(** val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1 **)

let rec fold_right f a0 = function
| [] -> a0
| b :: t -> f b (fold_right f a0 t)

(** val skipn : Big_int_Z.big_int -> 'a1 list -> 'a1 list **)

let rec skipn n0 l =
  (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
    (fun _ -> l)
    (fun n1 -> match l with
               | [] -> []
               | _ :: l0 -> skipn n1 l0)
    n0

(** val seq :
    Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int list **)

let rec seq start len =
  (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
    (fun _ -> [])
    (fun len0 -> start :: (seq (Big_int_Z.succ_big_int start) len0))
    len

(** val repeat : 'a1 -> Big_int_Z.big_int -> 'a1 list **)

let rec repeat x n0 =
  (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
    (fun _ -> [])
    (fun k -> x :: (repeat x k))
    n0

module Z =
 struct
  (** val double : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let double x =
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun p -> (Big_int_Z.mult_int_big_int 2 p))
      (fun p -> Big_int_Z.minus_big_int (Big_int_Z.mult_int_big_int 2 p))
      x

  (** val succ_double : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let succ_double x =
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> Big_int_Z.unit_big_int)
      (fun p ->
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      p))
      (fun p -> Big_int_Z.minus_big_int (Pos.pred_double p))
      x

  (** val pred_double : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let pred_double x =
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> Big_int_Z.minus_big_int Big_int_Z.unit_big_int)
      (fun p -> (Pos.pred_double p))
      (fun p -> Big_int_Z.minus_big_int
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x)) p))
      x

  (** val pos_sub :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec pos_sub x y =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q -> double (pos_sub p q))
        (fun q -> succ_double (pos_sub p q))
        (fun _ -> (Big_int_Z.mult_int_big_int 2 p))
        y)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q -> pred_double (pos_sub p q))
        (fun q -> double (pos_sub p q))
        (fun _ -> (Pos.pred_double p))
        y)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q -> Big_int_Z.minus_big_int (Big_int_Z.mult_int_big_int 2
        q))
        (fun q -> Big_int_Z.minus_big_int (Pos.pred_double q))
        (fun _ -> Big_int_Z.zero_big_int)
        y)
      x

  (** val add :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let add = Big_int_Z.add_big_int

  (** val opp : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let opp = Big_int_Z.minus_big_int

  (** val sub :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let sub = Big_int_Z.sub_big_int

  (** val to_nat : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let to_nat z0 =
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun p -> Pos.to_nat p)
      (fun _ -> Big_int_Z.zero_big_int)
      z0

  (** val of_nat : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let of_nat n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun n1 -> (Pos.of_succ_nat n1))
      n0

  (** val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let eq_dec = Big_int_Z.eq_big_int
 end

(** val zero : char **)

let zero = '\000'

(** val shift : bool -> char -> char **)

let shift = fun b c -> Char.chr (((Char.code c) lsl 1) land 255 + if b then 1 else 0)



(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

type decision = bool

(** val decide : decision -> bool **)

let decide decision0 =
  decision0

type ('a, 'b) relDecision = 'a -> 'b -> decision

(** val decide_rel : ('a1, 'a2) relDecision -> 'a1 -> 'a2 -> decision **)

let decide_rel relDecision0 =
  relDecision0

(** val prod_map :
    ('a1 -> 'a2) -> ('a3 -> 'a4) -> ('a1 * 'a3) -> 'a2 * 'a4 **)

let prod_map f g p =
  ((f (fst p)), (g (snd p)))

type 'a empty = 'a

(** val empty0 : 'a1 empty -> 'a1 **)

let empty0 empty1 =
  empty1

type 'a union = 'a -> 'a -> 'a

(** val union0 : 'a1 union -> 'a1 -> 'a1 -> 'a1 **)

let union0 union1 =
  union1

type ('a, 'b) filter = __ -> ('a -> decision) -> 'b -> 'b

(** val filter0 : ('a1, 'a2) filter -> ('a1 -> decision) -> 'a2 -> 'a2 **)

let filter0 filter1 h x =
  filter1 __ h x

type 'm mRet = __ -> __ -> 'm

(** val mret : 'a1 mRet -> 'a2 -> 'a1 **)

let mret mRet0 x =
  Obj.magic mRet0 __ x

type 'm mBind = __ -> __ -> (__ -> 'm) -> 'm -> 'm

(** val mbind : 'a1 mBind -> ('a2 -> 'a1) -> 'a1 -> 'a1 **)

let mbind mBind0 x x0 =
  Obj.magic mBind0 __ __ x x0

type 'm fMap = __ -> __ -> (__ -> __) -> 'm -> 'm

(** val fmap : 'a1 fMap -> ('a2 -> 'a3) -> 'a1 -> 'a1 **)

let fmap fMap0 x x0 =
  Obj.magic fMap0 __ __ x x0

type 'm oMap = __ -> __ -> (__ -> __ option) -> 'm -> 'm

(** val omap : 'a1 oMap -> ('a2 -> 'a3 option) -> 'a1 -> 'a1 **)

let omap oMap0 x x0 =
  Obj.magic oMap0 __ __ x x0

type ('k, 'a, 'm) lookup = 'k -> 'm -> 'a option

(** val lookup0 : ('a1, 'a2, 'a3) lookup -> 'a1 -> 'a3 -> 'a2 option **)

let lookup0 lookup1 =
  lookup1

type ('k, 'a, 'm) insert = 'k -> 'a -> 'm -> 'm

(** val insert0 : ('a1, 'a2, 'a3) insert -> 'a1 -> 'a2 -> 'a3 -> 'a3 **)

let insert0 insert2 =
  insert2

type ('k, 'a, 'm) partialAlter = ('a option -> 'a option) -> 'k -> 'm -> 'm

(** val partial_alter :
    ('a1, 'a2, 'a3) partialAlter -> ('a2 option -> 'a2 option) -> 'a1 -> 'a3
    -> 'a3 **)

let partial_alter partialAlter0 =
  partialAlter0

type 'm merge =
  __ -> __ -> __ -> (__ option -> __ option -> __ option) -> 'm -> 'm -> 'm

(** val merge0 :
    'a1 merge -> ('a2 option -> 'a3 option -> 'a4 option) -> 'a1 -> 'a1 -> 'a1 **)

let merge0 merge1 x x0 x1 =
  Obj.magic merge1 __ __ __ x x0 x1

type ('a, 'm) unionWith = ('a -> 'a -> 'a option) -> 'm -> 'm -> 'm

(** val union_with :
    ('a1, 'a2) unionWith -> ('a1 -> 'a1 -> 'a1 option) -> 'a2 -> 'a2 -> 'a2 **)

let union_with unionWith0 =
  unionWith0

(** val unit_eq_dec : (unit, unit) relDecision **)

let unit_eq_dec _ _ =
  true

(** val prod_eq_dec :
    ('a1, 'a1) relDecision -> ('a2, 'a2) relDecision -> ('a1 * 'a2,
    'a1 * 'a2) relDecision **)

let prod_eq_dec eqDecision0 eqDecision1 x y =
  let (a, b) = x in
  let (a0, b0) = y in
  if decide_rel eqDecision0 a a0 then decide_rel eqDecision1 b b0 else false

(** val sum_eq_dec :
    ('a1, 'a1) relDecision -> ('a2, 'a2) relDecision -> (('a1, 'a2) sum,
    ('a1, 'a2) sum) relDecision **)

let sum_eq_dec eqDecision0 eqDecision1 x y =
  match x with
  | Inl a ->
    (match y with
     | Inl a0 -> decide_rel eqDecision0 a a0
     | Inr _ -> false)
  | Inr b ->
    (match y with
     | Inl _ -> false
     | Inr b0 -> decide_rel eqDecision1 b b0)

(** val uncurry_dec : ('a1 -> 'a2 -> decision) -> ('a1 * 'a2) -> decision **)

let uncurry_dec p_dec = function
| (x, y) -> p_dec x y

(** val from_option : ('a1 -> 'a2) -> 'a2 -> 'a1 option -> 'a2 **)

let from_option f y = function
| Some x -> f x
| None -> y

(** val option_eq_None_dec : 'a1 option -> decision **)

let option_eq_None_dec = function
| Some _ -> false
| None -> true

(** val option_ret : __ -> __ option **)

let option_ret x =
  Some x

(** val option_bind : (__ -> __ option) -> __ option -> __ option **)

let option_bind f = function
| Some x -> f x
| None -> None

(** val option_fmap : (__ -> __) -> __ option -> __ option **)

let option_fmap =
  option_map

(** val option_union_with : ('a1, 'a1 option) unionWith **)

let option_union_with f mx my =
  match mx with
  | Some x -> (match my with
               | Some y -> f x y
               | None -> Some x)
  | None -> my

module Coq_Nat =
 struct
  (** val eq_dec : (Big_int_Z.big_int, Big_int_Z.big_int) relDecision **)

  let eq_dec =
    Nat.eq_dec
 end

module Coq_Pos =
 struct
  (** val eq_dec : (Big_int_Z.big_int, Big_int_Z.big_int) relDecision **)

  let eq_dec =
    Pos.eq_dec

  (** val app :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec app p1 p2 =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p3 ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (app p1 p3))
      (fun p3 -> Big_int_Z.mult_int_big_int 2 (app p1 p3))
      (fun _ -> p1)
      p2

  (** val reverse_go :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec reverse_go p1 p2 =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p3 ->
      reverse_go
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        p1) p3)
      (fun p3 -> reverse_go (Big_int_Z.mult_int_big_int 2 p1) p3)
      (fun _ -> p1)
      p2

  (** val reverse : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let reverse =
    reverse_go Big_int_Z.unit_big_int

  (** val dup : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec dup p =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p' ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (dup p')))
      (fun p' -> Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
      (dup p')))
      (fun _ -> Big_int_Z.unit_big_int)
      p
 end

module Coq_Z =
 struct
  (** val eq_dec : (Big_int_Z.big_int, Big_int_Z.big_int) relDecision **)

  let eq_dec =
    Z.eq_dec
 end

(** val foldl : ('a1 -> 'a2 -> 'a1) -> 'a1 -> 'a2 list -> 'a1 **)

let rec foldl f a = function
| [] -> a
| x :: l0 -> foldl f (f a x) l0

(** val list_fmap : (__ -> __) -> __ list -> __ list **)

let rec list_fmap f = function
| [] -> []
| x :: l0 -> (f x) :: (list_fmap f l0)

(** val list_omap : (__ -> __ option) -> __ list -> __ list **)

let rec list_omap f = function
| [] -> []
| x :: l0 ->
  (match f x with
   | Some y -> y :: (list_omap f l0)
   | None -> list_omap f l0)

(** val mapM : 'a1 mBind -> 'a1 mRet -> ('a2 -> 'a1) -> 'a2 list -> 'a1 **)

let rec mapM h h0 f = function
| [] -> mret h0 []
| x :: l0 ->
  mbind h (fun y -> mbind h (fun k -> mret h0 (y :: k)) (mapM h h0 f l0))
    (f x)

(** val elem_of_list_dec :
    ('a1, 'a1) relDecision -> ('a1, 'a1 list) relDecision **)

let rec elem_of_list_dec dec x = function
| [] -> false
| y :: l0 ->
  if decide (decide_rel dec x y) then true else elem_of_list_dec dec x l0

(** val list_difference :
    ('a1, 'a1) relDecision -> 'a1 list -> 'a1 list -> 'a1 list **)

let rec list_difference dec l k =
  match l with
  | [] -> []
  | x :: l0 ->
    if decide_rel (elem_of_list_dec dec) x k
    then list_difference dec l0 k
    else x :: (list_difference dec l0 k)

(** val positives_flatten_go :
    Big_int_Z.big_int list -> Big_int_Z.big_int -> Big_int_Z.big_int **)

let rec positives_flatten_go xs acc =
  match xs with
  | [] -> acc
  | x :: xs0 ->
    positives_flatten_go xs0
      (Coq_Pos.app (Big_int_Z.mult_int_big_int 2
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        acc)) (Coq_Pos.reverse (Coq_Pos.dup x)))

(** val positives_flatten : Big_int_Z.big_int list -> Big_int_Z.big_int **)

let positives_flatten xs =
  positives_flatten_go xs Big_int_Z.unit_big_int

(** val positives_unflatten_go :
    Big_int_Z.big_int -> Big_int_Z.big_int list -> Big_int_Z.big_int ->
    Big_int_Z.big_int list option **)

let rec positives_unflatten_go p acc_xs acc_elm =
  (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
    (fun p0 ->
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p' ->
      positives_unflatten_go p' acc_xs
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        acc_elm))
      (fun _ -> None)
      (fun _ -> None)
      p0)
    (fun p0 ->
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p' ->
      positives_unflatten_go p' (acc_elm :: acc_xs) Big_int_Z.unit_big_int)
      (fun p' ->
      positives_unflatten_go p' acc_xs (Big_int_Z.mult_int_big_int 2 acc_elm))
      (fun _ -> None)
      p0)
    (fun _ -> Some acc_xs)
    p

(** val positives_unflatten :
    Big_int_Z.big_int -> Big_int_Z.big_int list option **)

let positives_unflatten p =
  positives_unflatten_go p [] Big_int_Z.unit_big_int

(** val list_eq_dec0 :
    ('a1, 'a1) relDecision -> ('a1 list, 'a1 list) relDecision **)

let list_eq_dec0 =
  list_eq_dec

type 'a countable = { encode : ('a -> Big_int_Z.big_int);
                      decode : (Big_int_Z.big_int -> 'a option) }

(** val unit_countable : unit countable **)

let unit_countable =
  { encode = (fun _ -> Big_int_Z.unit_big_int); decode = (fun _ -> Some ()) }

(** val sum_countable :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision -> 'a2
    countable -> ('a1, 'a2) sum countable **)

let sum_countable _ h _ h0 =
  { encode = (fun xy ->
    match xy with
    | Inl x -> Big_int_Z.mult_int_big_int 2 (h.encode x)
    | Inr y ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        (h0.encode y)); decode = (fun p ->
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p0 ->
      let p1 = Obj.magic p0 in
      Obj.magic fmap (fun _ _ -> option_fmap) (fun x -> Inr x) (h0.decode p1))
      (fun p0 ->
      let p1 = Obj.magic p0 in
      Obj.magic fmap (fun _ _ -> option_fmap) (fun x -> Inl x) (h.decode p1))
      (fun _ -> None)
      p) }

(** val prod_encode_fst : Big_int_Z.big_int -> Big_int_Z.big_int **)

let rec prod_encode_fst p =
  (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
    (fun p0 ->
    (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2 (prod_encode_fst p0)))
    (fun p0 -> Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    (prod_encode_fst p0)))
    (fun _ -> Big_int_Z.unit_big_int)
    p

(** val prod_encode_snd : Big_int_Z.big_int -> Big_int_Z.big_int **)

let rec prod_encode_snd p =
  (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
    (fun p0 -> Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (prod_encode_snd p0)))
    (fun p0 -> Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    (prod_encode_snd p0)))
    (fun _ -> Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)
    p

(** val prod_encode :
    Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

let rec prod_encode p q =
  (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
    (fun p0 ->
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun q0 ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (prod_encode p0 q0)))
      (fun q0 ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (Big_int_Z.mult_int_big_int 2 (prod_encode p0 q0)))
      (fun _ ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (prod_encode_fst p0)))
      q)
    (fun p0 ->
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun q0 -> Big_int_Z.mult_int_big_int 2
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (prod_encode p0 q0)))
      (fun q0 -> Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
      (prod_encode p0 q0)))
      (fun _ -> Big_int_Z.mult_int_big_int 2
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (prod_encode_fst p0)))
      q)
    (fun _ ->
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun q0 ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (prod_encode_snd q0)))
      (fun q0 ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (Big_int_Z.mult_int_big_int 2 (prod_encode_snd q0)))
      (fun _ ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      Big_int_Z.unit_big_int)
      q)
    p

(** val prod_decode_fst : Big_int_Z.big_int -> Big_int_Z.big_int option **)

let rec prod_decode_fst p =
  (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
    (fun p0 ->
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p1 -> Some
      (match prod_decode_fst p1 with
       | Some q ->
         (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x)) q
       | None -> Big_int_Z.unit_big_int))
      (fun p1 -> Some
      (match prod_decode_fst p1 with
       | Some q ->
         (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x)) q
       | None -> Big_int_Z.unit_big_int))
      (fun _ -> Some Big_int_Z.unit_big_int)
      p0)
    (fun p0 ->
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p1 ->
      fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x ->
        Big_int_Z.mult_int_big_int 2 x) (prod_decode_fst p1))
      (fun p1 ->
      fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x ->
        Big_int_Z.mult_int_big_int 2 x) (prod_decode_fst p1))
      (fun _ -> None)
      p0)
    (fun _ -> Some Big_int_Z.unit_big_int)
    p

(** val prod_decode_snd : Big_int_Z.big_int -> Big_int_Z.big_int option **)

let rec prod_decode_snd p =
  (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
    (fun p0 ->
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p1 -> Some
      (match prod_decode_snd p1 with
       | Some q ->
         (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x)) q
       | None -> Big_int_Z.unit_big_int))
      (fun p1 ->
      fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x ->
        Big_int_Z.mult_int_big_int 2 x) (prod_decode_snd p1))
      (fun _ -> Some Big_int_Z.unit_big_int)
      p0)
    (fun p0 ->
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p1 -> Some
      (match prod_decode_snd p1 with
       | Some q ->
         (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x)) q
       | None -> Big_int_Z.unit_big_int))
      (fun p1 ->
      fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x ->
        Big_int_Z.mult_int_big_int 2 x) (prod_decode_snd p1))
      (fun _ -> Some Big_int_Z.unit_big_int)
      p0)
    (fun _ -> None)
    p

(** val prod_countable :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision -> 'a2
    countable -> ('a1 * 'a2) countable **)

let prod_countable _ h _ h0 =
  { encode = (fun xy ->
    prod_encode (h.encode (fst xy)) (h0.encode (snd xy))); decode = (fun p ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun x ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun y -> Some (x, y))
        (mbind (Obj.magic (fun _ _ -> option_bind)) (Obj.magic h0).decode
          (Obj.magic prod_decode_snd p)))
      (mbind (Obj.magic (fun _ _ -> option_bind)) (Obj.magic h).decode
        (Obj.magic prod_decode_fst p))) }

(** val list_countable :
    ('a1, 'a1) relDecision -> 'a1 countable -> 'a1 list countable **)

let list_countable _ h =
  { encode = (fun xs ->
    positives_flatten
      (fmap (Obj.magic (fun _ _ -> list_fmap)) h.encode (Obj.magic xs)));
    decode = (fun p ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun positives ->
      mapM (Obj.magic (fun _ _ -> option_bind))
        (Obj.magic (fun _ -> option_ret)) (Obj.magic h).decode positives)
      (Obj.magic positives_unflatten p)) }

(** val n_countable : Big_int_Z.big_int countable **)

let n_countable =
  { encode = (fun x ->
    (fun fO fp n -> if Big_int_Z.sign_big_int n <= 0 then fO () else fp n)
      (fun _ -> Big_int_Z.unit_big_int)
      (fun p -> Pos.succ p)
      x); decode = (fun p ->
    if decide (decide_rel Coq_Pos.eq_dec p Big_int_Z.unit_big_int)
    then Some Big_int_Z.zero_big_int
    else Some (Pos.pred p)) }

(** val z_countable : Big_int_Z.big_int countable **)

let z_countable =
  { encode = (fun x ->
    (fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
      (fun _ -> Big_int_Z.unit_big_int)
      (fun p -> Big_int_Z.mult_int_big_int 2 p)
      (fun p ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x)) p)
      x); decode = (fun p -> Some
    ((fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
       (fun p0 -> Big_int_Z.minus_big_int p0)
       (fun p0 -> p0)
       (fun _ -> Big_int_Z.zero_big_int)
       p)) }

(** val nat_countable : Big_int_Z.big_int countable **)

let nat_countable =
  { encode = (fun x -> n_countable.encode (N.of_nat x)); decode = (fun p ->
    fmap (Obj.magic (fun _ _ -> option_fmap)) N.to_nat
      ((Obj.magic n_countable).decode p)) }

(** val ascii_eq_dec : (char, char) relDecision **)

let ascii_eq_dec =
  (=)

(** val string_eq_dec : (char list, char list) relDecision **)

let rec string_eq_dec s x0 =
  match s with
  | [] -> (match x0 with
           | [] -> true
           | _::_ -> false)
  | a::s0 ->
    (match x0 with
     | [] -> false
     | a0::s1 ->
       if decide_rel ascii_eq_dec a a0 then string_eq_dec s0 s1 else false)

(** val digits_to_pos : bool list -> Big_int_Z.big_int **)

let rec digits_to_pos = function
| [] -> Big_int_Z.unit_big_int
| b :: _UU03b2_s0 ->
  if b
  then (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
         (digits_to_pos _UU03b2_s0)
  else Big_int_Z.mult_int_big_int 2 (digits_to_pos _UU03b2_s0)

(** val ascii_to_digits : char -> bool list **)

let ascii_to_digits a =
  (* If this appears, you're using Ascii internals. Please don't *)
 (fun f c ->
  let n = Char.code c in
  let h i = (n land (1 lsl i)) <> 0 in
  f (h 0) (h 1) (h 2) (h 3) (h 4) (h 5) (h 6) (h 7))
    (fun _UU03b2_1 _UU03b2_2 _UU03b2_3 _UU03b2_4 _UU03b2_5 _UU03b2_6 _UU03b2_7 _UU03b2_8 ->
    _UU03b2_1 :: (_UU03b2_2 :: (_UU03b2_3 :: (_UU03b2_4 :: (_UU03b2_5 :: (_UU03b2_6 :: (_UU03b2_7 :: (_UU03b2_8 :: []))))))))
    a

(** val string_to_pos : char list -> Big_int_Z.big_int **)

let rec string_to_pos = function
| [] -> Big_int_Z.unit_big_int
| a::s0 -> Coq_Pos.app (string_to_pos s0) (digits_to_pos (ascii_to_digits a))

(** val digits_of_pos : Big_int_Z.big_int -> bool list **)

let rec digits_of_pos p =
  (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
    (fun p0 -> true :: (digits_of_pos p0))
    (fun p0 -> false :: (digits_of_pos p0))
    (fun _ -> [])
    p

(** val ascii_of_digits : bool list -> char **)

let rec ascii_of_digits = function
| [] -> zero
| _UU03b2_ :: _UU03b2_s0 -> shift _UU03b2_ (ascii_of_digits _UU03b2_s0)

(** val string_of_digits : bool list -> char list **)

let rec string_of_digits = function
| [] -> []
| _UU03b2_1 :: l ->
  (match l with
   | [] -> []
   | _UU03b2_2 :: l0 ->
     (match l0 with
      | [] -> []
      | _UU03b2_3 :: l1 ->
        (match l1 with
         | [] -> []
         | _UU03b2_4 :: l2 ->
           (match l2 with
            | [] -> []
            | _UU03b2_5 :: l3 ->
              (match l3 with
               | [] -> []
               | _UU03b2_6 :: l4 ->
                 (match l4 with
                  | [] -> []
                  | _UU03b2_7 :: l5 ->
                    (match l5 with
                     | [] -> []
                     | _UU03b2_8 :: _UU03b2_s0 ->
                       (ascii_of_digits
                         (_UU03b2_1 :: (_UU03b2_2 :: (_UU03b2_3 :: (_UU03b2_4 :: (_UU03b2_5 :: (_UU03b2_6 :: (_UU03b2_7 :: (_UU03b2_8 :: [])))))))))::
                         (string_of_digits _UU03b2_s0))))))))

(** val string_of_pos : Big_int_Z.big_int -> char list **)

let string_of_pos p =
  string_of_digits (digits_of_pos p)

(** val string_countable : char list countable **)

let string_countable =
  { encode = string_to_pos; decode = (fun p -> Some (string_of_pos p)) }

type ('k, 'a, 'm) finMapToList = 'm -> ('k * 'a) list

(** val map_to_list :
    ('a1, 'a2, 'a3) finMapToList -> 'a3 -> ('a1 * 'a2) list **)

let map_to_list finMapToList0 =
  finMapToList0

(** val diag_None :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 option -> 'a2 option ->
    'a3 option **)

let diag_None f mx my =
  match mx with
  | Some _ -> f mx my
  | None -> (match my with
             | Some _ -> f mx my
             | None -> None)

(** val map_insert :
    ('a1, 'a2, 'a3) partialAlter -> ('a1, 'a2, 'a3) insert **)

let map_insert h i x =
  partial_alter h (fun _ -> Some x) i

(** val list_to_map :
    ('a1, 'a2, 'a3) insert -> 'a3 empty -> ('a1 * 'a2) list -> 'a3 **)

let list_to_map h h0 =
  fold_right (fun p -> insert0 h (fst p) (snd p)) (empty0 h0)

(** val map_union_with : 'a1 merge -> ('a2, 'a1) unionWith **)

let map_union_with h f =
  merge0 h (union_with option_union_with f)

(** val map_union : 'a1 merge -> 'a1 union **)

let map_union h =
  union_with (map_union_with h) (fun x _ -> Some x)

(** val kmap :
    (__ -> ('a1, __, 'a2) insert) -> (__ -> 'a2 empty) -> (__ -> ('a3, __,
    'a4) finMapToList) -> ('a3 -> 'a1) -> 'a4 -> 'a2 **)

let kmap h h0 h1 f m =
  list_to_map (Obj.magic h __) (h0 __)
    (fmap (Obj.magic (fun _ _ -> list_fmap)) (prod_map f id)
      (map_to_list (h1 __) m))

(** val map_fold :
    ('a1, 'a2, 'a3) finMapToList -> ('a1 -> 'a2 -> 'a4 -> 'a4) -> 'a4 -> 'a3
    -> 'a4 **)

let map_fold h f b =
  compose (fold_right (prod_curry_subdef f) b) (map_to_list h)

(** val map_filter :
    ('a1, 'a2, 'a3) finMapToList -> ('a1, 'a2, 'a3) insert -> 'a3 empty ->
    (('a1 * 'a2) -> decision) -> 'a3 -> 'a3 **)

let map_filter h h0 h1 h2 =
  map_fold h (fun k v m ->
    if decide (h2 (k, v)) then insert0 h0 k v m else m) (empty0 h1)

type 'a pmap_raw =
| PLeaf
| PNode of 'a option * 'a pmap_raw * 'a pmap_raw

(** val pNode' :
    'a1 option -> 'a1 pmap_raw -> 'a1 pmap_raw -> 'a1 pmap_raw **)

let pNode' o l r0 =
  match l with
  | PLeaf ->
    (match o with
     | Some _ -> PNode (o, l, r0)
     | None ->
       (match r0 with
        | PLeaf -> PLeaf
        | PNode (_, _, _) -> PNode (o, l, r0)))
  | PNode (_, _, _) -> PNode (o, l, r0)

(** val pempty_raw : 'a1 pmap_raw empty **)

let pempty_raw =
  PLeaf

(** val plookup_raw : (Big_int_Z.big_int, 'a1, 'a1 pmap_raw) lookup **)

let rec plookup_raw i = function
| PLeaf -> None
| PNode (o, l, r0) ->
  ((fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
     (fun i0 -> lookup0 plookup_raw i0 r0)
     (fun i0 -> lookup0 plookup_raw i0 l)
     (fun _ -> o)
     i)

(** val psingleton_raw : Big_int_Z.big_int -> 'a1 -> 'a1 pmap_raw **)

let rec psingleton_raw i x =
  (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
    (fun i0 -> PNode (None, PLeaf, (psingleton_raw i0 x)))
    (fun i0 -> PNode (None, (psingleton_raw i0 x), PLeaf))
    (fun _ -> PNode ((Some x), PLeaf, PLeaf))
    i

(** val ppartial_alter_raw :
    ('a1 option -> 'a1 option) -> Big_int_Z.big_int -> 'a1 pmap_raw -> 'a1
    pmap_raw **)

let rec ppartial_alter_raw f i = function
| PLeaf -> (match f None with
            | Some x -> psingleton_raw i x
            | None -> PLeaf)
| PNode (o, l, r0) ->
  ((fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
     (fun i0 -> pNode' o l (ppartial_alter_raw f i0 r0))
     (fun i0 -> pNode' o (ppartial_alter_raw f i0 l) r0)
     (fun _ -> pNode' (f o) l r0)
     i)

(** val pfmap_raw : ('a1 -> 'a2) -> 'a1 pmap_raw -> 'a2 pmap_raw **)

let rec pfmap_raw f = function
| PLeaf -> PLeaf
| PNode (o, l, r0) ->
  PNode ((fmap (Obj.magic (fun _ _ -> option_fmap)) f (Obj.magic o)),
    (pfmap_raw f l), (pfmap_raw f r0))

(** val pto_list_raw :
    Big_int_Z.big_int -> 'a1 pmap_raw -> (Big_int_Z.big_int * 'a1) list ->
    (Big_int_Z.big_int * 'a1) list **)

let rec pto_list_raw j t acc =
  match t with
  | PLeaf -> acc
  | PNode (o, l, r0) ->
    app (from_option (fun x -> ((Coq_Pos.reverse j), x) :: []) [] o)
      (pto_list_raw (Big_int_Z.mult_int_big_int 2 j) l
        (pto_list_raw
          ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
          j) r0 acc))

(** val pomap_raw : ('a1 -> 'a2 option) -> 'a1 pmap_raw -> 'a2 pmap_raw **)

let rec pomap_raw f = function
| PLeaf -> PLeaf
| PNode (o, l, r0) ->
  pNode' (mbind (Obj.magic (fun _ _ -> option_bind)) f (Obj.magic o))
    (pomap_raw f l) (pomap_raw f r0)

(** val pmerge_raw :
    ('a1 option -> 'a2 option -> 'a3 option) -> 'a1 pmap_raw -> 'a2 pmap_raw
    -> 'a3 pmap_raw **)

let rec pmerge_raw f t1 t2 =
  match t1 with
  | PLeaf -> pomap_raw (compose (f None) (fun x -> Some x)) t2
  | PNode (o1, l1, r1) ->
    (match t2 with
     | PLeaf -> pomap_raw (compose (flip f None) (fun x -> Some x)) t1
     | PNode (o2, l2, r2) ->
       pNode' (diag_None f o1 o2) (pmerge_raw f l1 l2) (pmerge_raw f r1 r2))

type 'a pmap =
  'a pmap_raw
  (* singleton inductive, whose constructor was PMap *)

(** val pmap_car : 'a1 pmap -> 'a1 pmap_raw **)

let pmap_car p =
  p

(** val pempty : 'a1 pmap empty **)

let pempty =
  empty0 pempty_raw

(** val plookup : (Big_int_Z.big_int, 'a1, 'a1 pmap) lookup **)

let plookup i m =
  lookup0 plookup_raw i (pmap_car m)

(** val ppartial_alter : (Big_int_Z.big_int, 'a1, 'a1 pmap) partialAlter **)

let ppartial_alter f i m =
  partial_alter ppartial_alter_raw f i m

(** val pfmap : (__ -> __) -> __ pmap -> __ pmap **)

let pfmap f m =
  fmap (fun _ _ -> pfmap_raw) f m

(** val pto_list : (Big_int_Z.big_int, 'a1, 'a1 pmap) finMapToList **)

let pto_list m =
  pto_list_raw Big_int_Z.unit_big_int m []

(** val pomap : (__ -> __ option) -> __ pmap -> __ pmap **)

let pomap f m =
  omap (fun _ _ -> pomap_raw) f m

(** val pmerge :
    (__ option -> __ option -> __ option) -> __ pmap -> __ pmap -> __ pmap **)

let pmerge =
  pmerge_raw

type ('k, 'a) gmap =
  'a pmap
  (* singleton inductive, whose constructor was GMap *)

(** val gmap_lookup :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2, ('a1, 'a2) gmap)
    lookup **)

let gmap_lookup _ h i pat =
  lookup0 plookup (h.encode i) pat

(** val gmap_empty :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2) gmap empty **)

let gmap_empty _ _ =
  empty0 pempty

(** val gmap_partial_alter :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2, ('a1, 'a2) gmap)
    partialAlter **)

let gmap_partial_alter _ h f i pat =
  partial_alter ppartial_alter f (h.encode i) pat

(** val gmap_fmap :
    ('a1, 'a1) relDecision -> 'a1 countable -> (__ -> __) -> ('a1, __) gmap
    -> ('a1, __) gmap **)

let gmap_fmap _ _ f pat =
  fmap (fun _ _ -> pfmap) f pat

(** val gmap_omap :
    ('a1, 'a1) relDecision -> 'a1 countable -> (__ -> __ option) -> ('a1, __)
    gmap -> ('a1, __) gmap **)

let gmap_omap _ _ f pat =
  omap (fun _ _ -> pomap) f pat

(** val gmap_merge :
    ('a1, 'a1) relDecision -> 'a1 countable -> (__ option -> __ option -> __
    option) -> ('a1, __) gmap -> ('a1, __) gmap -> ('a1, __) gmap **)

let gmap_merge _ _ f pat pat0 =
  merge0 (fun _ _ _ -> pmerge) f pat pat0

(** val gmap_to_list :
    ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2, ('a1, 'a2) gmap)
    finMapToList **)

let gmap_to_list _ h pat =
  omap (Obj.magic (fun _ _ -> list_omap)) (fun pat0 ->
    let (i, x) = pat0 in
    fmap (Obj.magic (fun _ _ -> option_fmap)) (fun x0 -> (x0, x)) (h.decode i))
    (map_to_list (Obj.magic pto_list) pat)

type regName =
| PC
| STK
| R of Big_int_Z.big_int

(** val all_registers : regName list **)

let all_registers =
  (R Big_int_Z.zero_big_int) :: ((R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)) :: ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) :: ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))) :: ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))) :: ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))))) :: ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))) :: ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))))) :: ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))))))))) :: ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))))))))))) :: ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))))))))) :: ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))))))))))) :: ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))))))))))))))) :: ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))))))))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))))))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))))))))))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))))))))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))))))))))))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))))))))))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))))))))))))))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))))))))))))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))))))))))))))))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))))))))))))))))))))))) :: ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))))))))))))))))))))))))))) :: (PC :: (STK :: [])))))))))))))))))))))))))))))))))

(** val reg_eq_dec : (regName, regName) relDecision **)

let reg_eq_dec r1 r2 =
  match r1 with
  | PC -> (match r2 with
           | PC -> true
           | _ -> false)
  | STK -> (match r2 with
            | STK -> true
            | _ -> false)
  | R n0 -> (match r2 with
             | R n1 -> Nat.eq_dec n0 n1
             | _ -> false)

(** val reg_countable : regName countable **)

let reg_countable =
  { encode = (fun r0 ->
    (sum_countable (sum_eq_dec unit_eq_dec unit_eq_dec)
      (sum_countable unit_eq_dec unit_countable unit_eq_dec unit_countable)
      Coq_Nat.eq_dec nat_countable).encode
      (match r0 with
       | PC -> Inl (Inl ())
       | STK -> Inl (Inr ())
       | R n0 -> Inr n0)); decode = (fun n0 ->
    match (sum_countable (sum_eq_dec unit_eq_dec unit_eq_dec)
            (sum_countable unit_eq_dec unit_countable unit_eq_dec
              unit_countable) Coq_Nat.eq_dec nat_countable).decode n0 with
    | Some y ->
      (match y with
       | Inl y0 -> (match y0 with
                    | Inl _ -> Some PC
                    | Inr _ -> Some STK)
       | Inr n1 -> Some (R n1))
    | None -> None) }

(** val r : Big_int_Z.big_int -> (Big_int_Z.big_int, regName) sum **)

let r n0 =
  Inr (R n0)

type perm =
| O
| RO
| RW
| RWL
| RX
| E
| RWX
| RWLX
| URW
| URWL
| URWX
| URWLX

type locality =
| Global
| Local
| Directed

type addr = Big_int_Z.big_int

type cap = (((perm * locality) * addr) * addr) * addr

type word = (Big_int_Z.big_int, cap) sum

type cerise_instruction =
| Jmp of regName
| Jnz of regName * regName
| Mov of regName * (Big_int_Z.big_int, regName) sum
| Load of regName * regName
| Store of regName * (Big_int_Z.big_int, regName) sum
| Lt of regName * (Big_int_Z.big_int, regName) sum
   * (Big_int_Z.big_int, regName) sum
| Add of regName * (Big_int_Z.big_int, regName) sum
   * (Big_int_Z.big_int, regName) sum
| Sub of regName * (Big_int_Z.big_int, regName) sum
   * (Big_int_Z.big_int, regName) sum
| Mul of regName * (Big_int_Z.big_int, regName) sum
   * (Big_int_Z.big_int, regName) sum
| Rem of regName * (Big_int_Z.big_int, regName) sum
   * (Big_int_Z.big_int, regName) sum
| Div of regName * (Big_int_Z.big_int, regName) sum
   * (Big_int_Z.big_int, regName) sum
| Lea of regName * (Big_int_Z.big_int, regName) sum
| Restrict of regName * (Big_int_Z.big_int, regName) sum
| Subseg of regName * (Big_int_Z.big_int, regName) sum
   * (Big_int_Z.big_int, regName) sum
| IsPtr of regName * regName
| GetL of regName * regName
| GetP of regName * regName
| GetB of regName * regName
| GetE of regName * regName
| GetA of regName * regName
| Fail
| Halt
| LoadU of regName * regName * (Big_int_Z.big_int, regName) sum
| StoreU of regName * (Big_int_Z.big_int, regName) sum
   * (Big_int_Z.big_int, regName) sum
| PromoteU of regName

(** val perm_eq_dec : (perm, perm) relDecision **)

let perm_eq_dec x y =
  match x with
  | O -> (match y with
          | O -> true
          | _ -> false)
  | RO -> (match y with
           | RO -> true
           | _ -> false)
  | RW -> (match y with
           | RW -> true
           | _ -> false)
  | RWL -> (match y with
            | RWL -> true
            | _ -> false)
  | RX -> (match y with
           | RX -> true
           | _ -> false)
  | E -> (match y with
          | E -> true
          | _ -> false)
  | RWX -> (match y with
            | RWX -> true
            | _ -> false)
  | RWLX -> (match y with
             | RWLX -> true
             | _ -> false)
  | URW -> (match y with
            | URW -> true
            | _ -> false)
  | URWL -> (match y with
             | URWL -> true
             | _ -> false)
  | URWX -> (match y with
             | URWX -> true
             | _ -> false)
  | URWLX -> (match y with
              | URWLX -> true
              | _ -> false)

(** val local_eq_dec : (locality, locality) relDecision **)

let local_eq_dec x y =
  match x with
  | Global -> (match y with
               | Global -> true
               | _ -> false)
  | Local -> (match y with
              | Local -> true
              | _ -> false)
  | Directed -> (match y with
                 | Directed -> true
                 | _ -> false)

(** val cap_eq_dec : (cap, cap) relDecision **)

let cap_eq_dec x y =
  decide_rel
    (prod_eq_dec
      (prod_eq_dec
        (prod_eq_dec (prod_eq_dec perm_eq_dec local_eq_dec) Coq_Nat.eq_dec)
        Coq_Nat.eq_dec) Coq_Nat.eq_dec) x y

(** val word_eq_dec : (word, word) relDecision **)

let word_eq_dec x y =
  decide_rel (sum_eq_dec Coq_Z.eq_dec cap_eq_dec) x y

(** val perm_countable : perm countable **)

let perm_countable =
  let encode0 = fun p ->
    match p with
    | O -> Big_int_Z.unit_big_int
    | RO -> Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int
    | RW ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int
    | RWL ->
      Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
        Big_int_Z.unit_big_int)
    | RX ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)
    | E ->
      Big_int_Z.mult_int_big_int 2
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int)
    | RWX ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int)
    | RWLX ->
      Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
        (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))
    | URW ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
        Big_int_Z.unit_big_int))
    | URWL ->
      Big_int_Z.mult_int_big_int 2
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))
    | URWX ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))
    | URWLX ->
      Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int))
  in
  let decode0 = fun n0 ->
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun p0 ->
        (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun _ -> None)
          (fun p1 ->
          (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
            (fun _ -> None)
            (fun _ -> None)
            (fun _ -> Some URWX)
            p1)
          (fun _ -> Some RWX)
          p0)
        (fun p0 ->
        (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun _ -> None)
          (fun p1 ->
          (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
            (fun _ -> None)
            (fun _ -> None)
            (fun _ -> Some URW)
            p1)
          (fun _ -> Some RX)
          p0)
        (fun _ -> Some RW)
        p)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun p0 ->
        (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun _ -> None)
          (fun p1 ->
          (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
            (fun _ -> None)
            (fun _ -> None)
            (fun _ -> Some URWL)
            p1)
          (fun _ -> Some E)
          p0)
        (fun p0 ->
        (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
          (fun p1 ->
          (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
            (fun _ -> None)
            (fun _ -> None)
            (fun _ -> Some URWLX)
            p1)
          (fun p1 ->
          (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
            (fun _ -> None)
            (fun _ -> None)
            (fun _ -> Some RWLX)
            p1)
          (fun _ -> Some RWL)
          p0)
        (fun _ -> Some RO)
        p)
      (fun _ -> Some O)
      n0
  in
  { encode = encode0; decode = decode0 }

(** val locality_countable : locality countable **)

let locality_countable =
  let encode0 = fun l ->
    match l with
    | Global -> Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int
    | Local -> Big_int_Z.unit_big_int
    | Directed ->
      (fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int
  in
  let decode0 = fun n0 ->
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun _ -> None)
        (fun _ -> None)
        (fun _ -> Some Directed)
        p)
      (fun p ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun _ -> None)
        (fun _ -> None)
        (fun _ -> Some Global)
        p)
      (fun _ -> Some Local)
      n0
  in
  { encode = encode0; decode = decode0 }

(** val cap_countable : cap countable **)

let cap_countable =
  prod_countable
    (prod_eq_dec
      (prod_eq_dec (prod_eq_dec perm_eq_dec local_eq_dec) Coq_Nat.eq_dec)
      Coq_Nat.eq_dec)
    (prod_countable
      (prod_eq_dec (prod_eq_dec perm_eq_dec local_eq_dec) Coq_Nat.eq_dec)
      (prod_countable (prod_eq_dec perm_eq_dec local_eq_dec)
        (prod_countable perm_eq_dec perm_countable local_eq_dec
          locality_countable) Coq_Nat.eq_dec nat_countable) Coq_Nat.eq_dec
      nat_countable) Coq_Nat.eq_dec nat_countable

(** val word_countable : word countable **)

let word_countable =
  sum_countable Coq_Z.eq_dec z_countable cap_eq_dec cap_countable

type symbols = char list

(** val symbols_encode : char list -> char list -> symbols **)

let symbols_encode mod_name imp_name0 =
  append (append mod_name ('.'::[])) imp_name0

type cerise_component = { segment : (addr, word) gmap;
                          imports : (addr, symbols) gmap;
                          exports : (symbols, word) gmap;
                          submodules : (addr * addr) list; main : word option }

(** val merge_mem :
    (addr, 'a1) gmap -> (addr, 'a1) gmap -> (addr, 'a1) gmap **)

let merge_mem m1 m2 =
  merge0 (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable))
    (fun a b ->
    match a with
    | Some a0 -> (match b with
                  | Some _ -> None
                  | None -> Some a0)
    | None -> (match b with
               | Some b0 -> Some b0
               | None -> None)) m1 m2

(** val list_to_addr_gmap : 'a1 list -> addr -> (addr, 'a1) gmap **)

let rec list_to_addr_gmap l a =
  match l with
  | [] -> gmap_empty Coq_Nat.eq_dec nat_countable
  | h :: t ->
    insert0 (map_insert (gmap_partial_alter Coq_Nat.eq_dec nat_countable)) a
      h
      (list_to_addr_gmap t
        (add a (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))

(** val insert1 : (addr * 'a1) -> (addr * 'a1) list -> (addr * 'a1) list **)

let rec insert1 x l = match l with
| [] -> x :: []
| h :: t -> if Nat.ltb (fst h) (fst x) then h :: (insert1 x t) else x :: l

(** val sort : (addr * 'a1) list -> (addr * 'a1) list **)

let rec sort = function
| [] -> []
| h :: t -> insert1 h (sort t)

type immediate = Big_int_Z.big_int

type handle = { base : Big_int_Z.big_int; offset : Big_int_Z.big_int;
                bound : Big_int_Z.big_int; valid : bool;
                id0 : Big_int_Z.big_int }

type value =
| Val_int of Big_int_Z.big_int
| Val_handle of handle

type value_type =
| T_int
| T_handle

type sx =
| SX_S
| SX_U

type binop =
| BOI_add
| BOI_sub
| BOI_mul
| BOI_rem of sx
| BOI_div of sx

type relop =
| ROI_eq
| ROI_ne
| ROI_lt of sx
| ROI_gt of sx
| ROI_le of sx
| ROI_ge of sx

type result_type = value_type list

type function_type =
| Tf of result_type * result_type

type ws_basic_instruction =
| I_unreachable
| I_nop
| I_drop
| I_select
| I_block of result_type * ws_basic_instruction list
| I_loop of result_type * ws_basic_instruction list
| I_if of result_type * ws_basic_instruction list * ws_basic_instruction list
| I_br of immediate
| I_br_if of immediate
| I_br_table of immediate list * immediate
| I_return
| I_call of immediate
| I_call_indirect of immediate
| I_get_local of immediate
| I_set_local of immediate
| I_tee_local of immediate
| I_get_global of immediate
| I_set_global of immediate
| I_load of value_type
| I_store of value_type
| I_segload of value_type
| I_segstore of value_type
| I_slice
| I_segalloc
| I_handleadd
| I_segfree
| I_current_memory
| I_grow_memory
| I_const of value
| I_binop of value_type * binop
| I_testop of value_type
| I_relop of value_type * relop

(** val page_size : Big_int_Z.big_int **)

let page_size =
  (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))))

type expr = ws_basic_instruction list

type typeidx =
  Big_int_Z.big_int
  (* singleton inductive, whose constructor was Mk_typeidx *)

type funcidx =
  Big_int_Z.big_int
  (* singleton inductive, whose constructor was Mk_funcidx *)

type name = char list

type import_desc =
  typeidx
  (* singleton inductive, whose constructor was ID_func *)

type module_import = { imp_module : name; imp_name : name;
                       imp_desc : import_desc }

type module_export_desc =
  funcidx
  (* singleton inductive, whose constructor was MED_func *)

type module_export = { modexp_name : name; modexp_desc : module_export_desc }

type module_func = { modfunc_type : typeidx;
                     modfunc_locals : value_type list; modfunc_body : 
                     expr }

type module_start =
  funcidx
  (* singleton inductive, whose constructor was Build_module_start *)

(** val modstart_func : module_start -> funcidx **)

let modstart_func m =
  m

type limits = { lim_min : Big_int_Z.big_int;
                lim_max : Big_int_Z.big_int option }

type table_type =
  limits
  (* singleton inductive, whose constructor was Build_table_type *)

type module_table =
  table_type
  (* singleton inductive, whose constructor was Build_module_table *)

type ws_module = { mod_types : function_type list;
                   mod_funcs : module_func list;
                   mod_tables : module_table list;
                   mod_start : module_start option;
                   mod_imports : module_import list;
                   mod_exports : module_export list }

(** val get_type : ws_module -> typeidx -> function_type option **)

let get_type module0 i =
  nth_error module0.mod_types i

(** val get_functions : module_import list -> typeidx list **)

let rec get_functions = function
| [] -> []
| i :: imports' -> i.imp_desc :: (get_functions imports')

(** val get_function_type : ws_module -> immediate -> function_type option **)

let get_function_type module0 i =
  let imported_functions = get_functions module0.mod_imports in
  let len_impf = length imported_functions in
  if Nat.leb i len_impf
  then mbind (Obj.magic (fun _ _ -> option_bind)) (fun ftype_idx ->
         get_type module0 ftype_idx)
         (nth_error (Obj.magic imported_functions) i)
  else let i0 = sub i len_impf in
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun module_func0 ->
         get_type module0 module_func0.modfunc_type)
         (nth_error (Obj.magic module0.mod_funcs) i0)

type labeled_instr =
| Label of Big_int_Z.big_int list
| BInstr of cerise_instruction
| Br_Jmp of Big_int_Z.big_int list * Big_int_Z.big_int
| Br_Jnz of Big_int_Z.big_int list * Big_int_Z.big_int

(** val instrs : cerise_instruction list -> labeled_instr list **)

let instrs li =
  map (fun i -> BInstr i) li

type labeled_cerise_component = { l_segment : (addr, word) gmap;
                                  l_imports : (addr, symbols) gmap;
                                  l_exports : (symbols, word) gmap;
                                  l_nfunctions : Big_int_Z.big_int;
                                  l_main : word option }

type machineParameters = { decodeInstr : (Big_int_Z.big_int ->
                                         cerise_instruction);
                           encodeInstr : (cerise_instruction ->
                                         Big_int_Z.big_int);
                           encodePerm : (perm -> Big_int_Z.big_int);
                           encodeLoc : (locality -> Big_int_Z.big_int);
                           decodePermPair : (Big_int_Z.big_int ->
                                            perm * locality);
                           encodePermPair : ((perm * locality) ->
                                            Big_int_Z.big_int) }

type lMachineParameters = { l_decodeInstr : (Big_int_Z.big_int ->
                                            labeled_instr);
                            l_encodeInstr : (labeled_instr ->
                                            Big_int_Z.big_int) }

(** val encodeInstrW : machineParameters -> cerise_instruction -> word **)

let encodeInstrW h i =
  Inl (h.encodeInstr i)

(** val encodeInstrsW :
    machineParameters -> cerise_instruction list -> word list **)

let encodeInstrsW h =
  map (encodeInstrW h)

(** val l_encodeInstrW : lMachineParameters -> labeled_instr -> word **)

let l_encodeInstrW h i =
  Inl (h.l_encodeInstr i)

(** val l_encodeInstrsW :
    lMachineParameters -> labeled_instr list -> word list **)

let l_encodeInstrsW h =
  map (l_encodeInstrW h)

(** val r_stk : regName **)

let r_stk =
  STK

(** val page_size0 : Big_int_Z.big_int **)

let page_size0 =
  (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))))

(** val eq_instrs :
    regName -> regName -> regName -> regName -> cerise_instruction list **)

let eq_instrs res r1 r2 tmp =
  (Sub (res, (Inr r2), (Inr r1))) :: ((Mov (tmp, (Inr PC))) :: ((Lea (tmp,
    (Inl (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))) :: ((Jnz (tmp, res)) :: ((Mov (res, (Inl
    Big_int_Z.unit_big_int))) :: ((Lea (tmp, (Inl
    Big_int_Z.unit_big_int))) :: ((Jmp tmp) :: ((Mov (res, (Inl
    Big_int_Z.zero_big_int))) :: ((Mov (tmp, (Inl
    Big_int_Z.zero_big_int))) :: []))))))))

(** val neq_instrs :
    regName -> regName -> regName -> regName -> cerise_instruction list **)

let neq_instrs res r1 r2 tmp =
  (Sub (res, (Inr r2), (Inr r1))) :: ((Mov (tmp, (Inr PC))) :: ((Lea (tmp,
    (Inl (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))) :: ((Jnz (tmp, res)) :: ((Mov (res, (Inl
    Big_int_Z.zero_big_int))) :: ((Lea (tmp, (Inl
    Big_int_Z.unit_big_int))) :: ((Jmp tmp) :: ((Mov (res, (Inl
    Big_int_Z.unit_big_int))) :: ((Mov (tmp, (Inl
    Big_int_Z.zero_big_int))) :: []))))))))

(** val eqz_instrs :
    regName -> regName -> regName -> cerise_instruction list **)

let eqz_instrs res r0 tmp =
  (Mov (tmp, (Inr PC))) :: ((Lea (tmp, (Inl (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))) :: ((Jnz (tmp, r0)) :: ((Mov (res, (Inl
    Big_int_Z.unit_big_int))) :: ((Lea (tmp, (Inl
    Big_int_Z.unit_big_int))) :: ((Jmp tmp) :: ((Mov (res, (Inl
    Big_int_Z.zero_big_int))) :: ((Mov (tmp, (Inl
    Big_int_Z.zero_big_int))) :: [])))))))

(** val ge_instrs :
    regName -> regName -> regName -> regName -> cerise_instruction list **)

let ge_instrs res r1 r2 tmp =
  (Lt (res, (Inr r2), (Inr r1))) :: ((Mov (tmp, (Inr PC))) :: ((Lea (tmp,
    (Inl (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))) :: ((Jnz (tmp, res)) :: ((Mov (res, (Inl
    Big_int_Z.zero_big_int))) :: ((Lea (tmp, (Inl
    Big_int_Z.unit_big_int))) :: ((Jmp tmp) :: ((Mov (res, (Inl
    Big_int_Z.unit_big_int))) :: ((Mov (tmp, (Inl
    Big_int_Z.zero_big_int))) :: []))))))))

(** val rclear_instrs : regName list -> cerise_instruction list **)

let rclear_instrs r0 =
  map (fun r_i -> Mov (r_i, (Inl Big_int_Z.zero_big_int))) r0

(** val push_instrs :
    (Big_int_Z.big_int, regName) sum list -> cerise_instruction list **)

let push_instrs _UU03c1_s =
  map (fun _UU03c1_ -> StoreU (r_stk, (Inl Big_int_Z.zero_big_int),
    _UU03c1_)) _UU03c1_s

(** val pop_instrs : regName -> cerise_instruction list **)

let pop_instrs r0 =
  (LoadU (r0, r_stk, (Inl (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int)))) :: ((Lea (r_stk, (Inl (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int)))) :: [])

(** val push_env_instrs : cerise_instruction list **)

let push_env_instrs =
  push_instrs
    (map (fun r0 -> Inr r0)
      (list_difference reg_eq_dec all_registers (PC :: (r_stk :: []))))

(** val pop_env_instrs : cerise_instruction list **)

let pop_env_instrs =
  foldl (fun b r0 -> app (pop_instrs r0) b) []
    (list_difference reg_eq_dec all_registers (PC :: (r_stk :: [])))

(** val activation_record :
    machineParameters -> regName -> cerise_instruction list **)

let activation_record h rtmp =
  let offset_ret =
    Z.add (Z.of_nat (length push_env_instrs)) Big_int_Z.unit_big_int
  in
  push_instrs
    (app ((Inl (h.encodeInstr (Mov (rtmp, (Inr PC))))) :: ((Inl
      (h.encodeInstr (Lea (rtmp, (Inl (Big_int_Z.minus_big_int
        Big_int_Z.unit_big_int)))))) :: ((Inl
      (h.encodeInstr (Load (r_stk, rtmp)))) :: []))) ((Inl
      (h.encodeInstr (LoadU (PC, r_stk, (Inl (Z.opp offset_ret)))))) :: []))

(** val call_instrs_prologue_1 :
    machineParameters -> regName -> cerise_instruction list **)

let call_instrs_prologue_1 h rtmp =
  app (push_instrs ((Inl Big_int_Z.zero_big_int) :: []))
    (app (push_instrs ((Inl Big_int_Z.zero_big_int) :: []))
      (app push_env_instrs
        (app (push_instrs ((Inr r_stk) :: [])) (activation_record h rtmp))))

(** val call_instrs_prologue :
    machineParameters -> regName list -> regName -> cerise_instruction list **)

let call_instrs_prologue h rargs rtmp =
  let offset_pc =
    Z.of_nat
      (add (length (app (activation_record h rtmp) push_env_instrs))
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))
  in
  let offset_ret =
    Z.add ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (Big_int_Z.mult_int_big_int 2
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      Big_int_Z.unit_big_int))))) (Z.of_nat (length rargs))
  in
  app (call_instrs_prologue_1 h rtmp)
    (app ((Mov (rtmp, (Inr PC))) :: ((Lea (rtmp, (Inl offset_ret))) :: []))
      ((StoreU (r_stk, (Inl (Z.opp offset_pc)), (Inr rtmp))) :: []))

(** val call_instrs :
    machineParameters -> regName -> regName list -> regName -> regName ->
    regName -> regName -> regName -> cerise_instruction list **)

let call_instrs h r0 rargs tmp0 tmp1 tmp2 tmp3 tmp4 =
  let len_act = Z.of_nat (length (activation_record h tmp0)) in
  let len_args = Z.of_nat (length rargs) in
  let offset_ret_val =
    Z.add
      (Z.add (Z.add (Z.add len_args len_act) Big_int_Z.unit_big_int)
        (Z.of_nat (length push_env_instrs))) (Big_int_Z.mult_int_big_int 2
      Big_int_Z.unit_big_int)
  in
  app (call_instrs_prologue h rargs tmp0)
    (app (push_instrs (map (fun x -> Inr x) (rev rargs)))
      (app ((Mov (tmp4, (Inr r_stk))) :: ((GetA (tmp1, tmp4)) :: ((Sub (tmp1,
        (Inr tmp1), (Inl (Z.of_nat (length rargs))))) :: ((GetA (tmp2,
        tmp4)) :: ((Subseg (tmp4, (Inr tmp1), (Inr tmp2))) :: [])))))
        (app ((Mov (tmp0, (Inr r_stk))) :: ((Lea (tmp0, (Inl
          (Z.opp offset_ret_val)))) :: ((GetA (tmp1, tmp0)) :: ((GetA (tmp2,
          tmp0)) :: ((Add (tmp2, (Inr tmp2), (Inl
          Big_int_Z.unit_big_int))) :: ((Subseg (tmp0, (Inr tmp1), (Inr
          tmp2))) :: []))))))
          (app ((Mov (tmp1, (Inr r_stk))) :: ((PromoteU tmp1) :: ((Lea (tmp1,
            (Inl (Z.opp (Z.add len_act len_args))))) :: ((Restrict (tmp1,
            (Inl (h.encodePermPair (E, Directed))))) :: []))))
            (app ((GetA (tmp3, r_stk)) :: ((GetE (tmp2, r_stk)) :: ((Subseg
              (r_stk, (Inr tmp3), (Inr tmp2))) :: [])))
              (app (push_instrs ((Inr tmp1) :: []))
                (app (push_instrs ((Inr tmp0) :: []))
                  (app (push_instrs ((Inr tmp4) :: []))
                    (app
                      (rclear_instrs
                        (list_difference reg_eq_dec all_registers
                          (PC :: (r0 :: (r_stk :: [])))))
                      (app ((Jmp r0) :: []) pop_env_instrs))))))))))

(** val reqloc_instrs :
    regName -> Big_int_Z.big_int -> regName -> regName -> cerise_instruction
    list **)

let reqloc_instrs r0 z0 rtmp1 rtmp2 =
  (IsPtr (rtmp1, r0)) :: ((Sub (rtmp1, (Inr rtmp1), (Inl
    Big_int_Z.unit_big_int))) :: ((Mov (rtmp2, (Inr PC))) :: ((Lea (rtmp2,
    (Inl ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))))) :: ((Jnz
    (rtmp2, rtmp1)) :: ((GetL (rtmp1, r0)) :: ((Sub (rtmp1, (Inr rtmp1), (Inl
    z0))) :: ((Mov (rtmp2, (Inr PC))) :: ((Lea (rtmp2, (Inl
    (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))) :: ((Jnz (rtmp2, rtmp1)) :: ((Mov (rtmp2,
    (Inr PC))) :: ((Lea (rtmp2, (Inl (Big_int_Z.mult_int_big_int 2
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))) :: ((Jmp
    rtmp2) :: (Fail :: ((Mov (rtmp1, (Inl Big_int_Z.zero_big_int))) :: ((Mov
    (rtmp2, (Inl Big_int_Z.zero_big_int))) :: [])))))))))))))))

(** val malloc_subroutine_instrs' :
    Big_int_Z.big_int -> cerise_instruction list **)

let malloc_subroutine_instrs' offset0 =
  (Lt ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (Inl
    Big_int_Z.zero_big_int), (Inr (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Mov ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr PC))) :: ((Lea
    ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))), (Inl (Big_int_Z.mult_int_big_int 2
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))) :: ((Jnz ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))), (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))) :: (Fail :: ((Mov ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))), (Inr PC))) :: ((Lea ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))), (Inl offset0))) :: ((Load ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))), (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((GetA ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Lea ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))), (Inr (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((GetA ((R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Mov ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))) :: ((Subseg ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Sub ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Lea ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((Sub ((R
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
    Big_int_Z.zero_big_int), (Inr (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Lea ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (Inr (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((GetB ((R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))) :: ((Lea ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (Inr (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Store ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((Mov ((R
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))) :: ((Mov ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))), (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inl
    Big_int_Z.zero_big_int))) :: ((Jmp (R
    Big_int_Z.zero_big_int)) :: [])))))))))))))))))))))))))

(** val malloc_subroutine_instrs : cerise_instruction list **)

let malloc_subroutine_instrs =
  malloc_subroutine_instrs'
    (Z.sub
      (Z.of_nat (length (malloc_subroutine_instrs' Big_int_Z.zero_big_int)))
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))

(** val malloc_instrs :
    regName -> regName -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> cerise_instruction list **)

let malloc_instrs r_malloc0 r_size tmp_res tmp_r0 tmp_r1 tmp_r2 tmp_r3 tmp_r4 =
  app ((Mov ((R tmp_r0), (Inr (R Big_int_Z.zero_big_int)))) :: ((Mov ((R
    tmp_r1), (Inr (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Mov ((R tmp_r2), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))) :: ((Mov ((R tmp_r3), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))) :: ((Mov ((R tmp_r4), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))) :: [])))))
    (app ((Mov ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr
      r_size))) :: ((Mov ((R Big_int_Z.zero_big_int), (Inr PC))) :: ((Lea ((R
      Big_int_Z.zero_big_int), (Inl
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      Big_int_Z.unit_big_int)))) :: ((Jmp r_malloc0) :: ((Mov ((R tmp_res),
      (Inr (R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: [])))))
      ((Mov ((R Big_int_Z.zero_big_int), (Inr (R tmp_r0)))) :: ((Mov ((R
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr (R
      tmp_r1)))) :: ((Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))), (Inr (R tmp_r2)))) :: ((Mov ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))), (Inr (R tmp_r3)))) :: ((Mov ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inr (R
      tmp_r4)))) :: ((Mov ((R tmp_r0), (Inl
      Big_int_Z.zero_big_int))) :: ((Mov ((R tmp_r1), (Inl
      Big_int_Z.zero_big_int))) :: ((Mov ((R tmp_r2), (Inl
      Big_int_Z.zero_big_int))) :: ((Mov ((R tmp_r3), (Inl
      Big_int_Z.zero_big_int))) :: ((Mov ((R tmp_r4), (Inl
      Big_int_Z.zero_big_int))) :: [])))))))))))

(** val grow_subroutine_instrs : cerise_instruction list **)

let grow_subroutine_instrs =
  (GetE ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((GetA ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Mul ((R
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr (R
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
    page_size0))) :: ((Add ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))), (Inr (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Mov ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inr PC))) :: ((Lea
    ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))), (Inl (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))) :: ((Lt ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))) :: ((Jnz ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))) :: ((Lea ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inl
    (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int)))))) :: ((Jmp (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: ((GetB ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((GetA ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Sub ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))) :: ((Div ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))), (Inl page_size0))) :: ((Lea ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))), (Inr (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((GetA ((R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Mov ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))) :: ((Subseg ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Sub ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Lea ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))) :: ((Mov ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))), (Inr PC))) :: ((Lea ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))), (Inl (Big_int_Z.mult_int_big_int 2
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))) :: ((Jmp (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)), (Inl (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))) :: ((Mov ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))), (Inl Big_int_Z.zero_big_int))) :: ((Jmp (R
    Big_int_Z.zero_big_int)) :: [])))))))))))))))))))))))))))))

(** val grow_instrs_main : cerise_instruction list **)

let grow_instrs_main =
  (Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inr PC))) :: ((Lea ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inl
    (Big_int_Z.minus_big_int
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))) :: ((Load ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: ((Lea ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inl
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))) :: ((Load ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))), (Inr (R
    Big_int_Z.zero_big_int)))) :: ((Mov ((R Big_int_Z.zero_big_int), (Inr
    PC))) :: ((Lea ((R Big_int_Z.zero_big_int), (Inl
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int)))) :: ((Jmp (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inr PC))) :: ((Lea ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inl
    (Big_int_Z.minus_big_int (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))))) :: ((Load ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))) :: ((Lea ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inl Big_int_Z.unit_big_int))) :: ((Store
    ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((Mov ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))), (Inr PC))) :: ((Lea ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))), (Inl (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))) :: ((Add ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr (R
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
    Big_int_Z.unit_big_int))) :: ((Jnz ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Lea ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inl
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))) :: ((Jmp (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))) :: ((Lea ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inl Big_int_Z.unit_big_int))) :: ((Store
    ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
    Big_int_Z.zero_big_int))) :: ((Mov ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inl
    Big_int_Z.zero_big_int))) :: ((Mov ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inl
    Big_int_Z.zero_big_int))) :: ((Mov ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))), (Inl Big_int_Z.zero_big_int))) :: ((Jmp (R
    Big_int_Z.zero_big_int)) :: []))))))))))))))))))))))))))))

(** val grow_instrs :
    regName -> regName -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> cerise_instruction list **)

let grow_instrs r_grow0 r_size tmp_res _ tmp_r0 tmp_r1 tmp_r2 tmp_r3 tmp_r4 tmp_r5 tmp_r6 =
  app ((Mov ((R tmp_r0), (Inr (R Big_int_Z.zero_big_int)))) :: ((Mov ((R
    tmp_r1), (Inr (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Mov ((R tmp_r2), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))) :: ((Mov ((R tmp_r3), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))) :: ((Mov ((R tmp_r4), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))) :: ((Mov ((R
    tmp_r5), (Inr (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))) :: ((Mov ((R tmp_r6), (Inr (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)), (Inr r_size))) :: []))))))))
    (app ((Mov ((R Big_int_Z.zero_big_int), (Inr PC))) :: ((Lea ((R
      Big_int_Z.zero_big_int), (Inl
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      Big_int_Z.unit_big_int)))) :: ((Jmp r_grow0) :: ((Mov ((R tmp_res),
      (Inr (R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: []))))
      ((Mov ((R Big_int_Z.zero_big_int), (Inr (R tmp_r0)))) :: ((Mov ((R
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr (R
      tmp_r1)))) :: ((Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))), (Inr (R tmp_r2)))) :: ((Mov ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))), (Inr (R tmp_r3)))) :: ((Mov ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inr (R
      tmp_r4)))) :: ((Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))))), (Inr (R tmp_r5)))) :: ((Mov ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))))), (Inr (R tmp_r6)))) :: ((Mov ((R tmp_r0),
      (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R tmp_r1), (Inl
      Big_int_Z.zero_big_int))) :: ((Mov ((R tmp_r2), (Inl
      Big_int_Z.zero_big_int))) :: ((Mov ((R tmp_r3), (Inl
      Big_int_Z.zero_big_int))) :: ((Mov ((R tmp_r4), (Inl
      Big_int_Z.zero_big_int))) :: ((Mov ((R tmp_r5), (Inl
      Big_int_Z.zero_big_int))) :: ((Mov ((R tmp_r6), (Inl
      Big_int_Z.zero_big_int))) :: [])))))))))))))))

type scope = { scope_name : Big_int_Z.big_int; scope_ret_type : result_type;
               scope_children : Big_int_Z.big_int;
               scope_reg_base : Big_int_Z.big_int }

(** val init_scope : scope **)

let init_scope =
  { scope_name = Big_int_Z.zero_big_int; scope_ret_type = [];
    scope_children = Big_int_Z.zero_big_int; scope_reg_base =
    Big_int_Z.zero_big_int }

type scope_state = scope list

(** val new_child : scope -> scope **)

let new_child s =
  { scope_name = s.scope_name; scope_ret_type = s.scope_ret_type;
    scope_children =
    (add s.scope_children (Big_int_Z.succ_big_int Big_int_Z.zero_big_int));
    scope_reg_base = s.scope_reg_base }

(** val init_scope_state : scope list **)

let init_scope_state =
  init_scope :: []

(** val push_scope :
    result_type -> Big_int_Z.big_int -> scope_state -> scope_state **)

let push_scope rt reg_base = function
| [] -> init_scope :: []
| p :: s' ->
  { scope_name =
    (add p.scope_children (Big_int_Z.succ_big_int Big_int_Z.zero_big_int));
    scope_ret_type = rt; scope_children = Big_int_Z.zero_big_int;
    scope_reg_base = reg_base } :: ((new_child p) :: s')

(** val pop_scope : scope_state -> Big_int_Z.big_int -> scope_state **)

let rec pop_scope s n0 =
  match s with
  | [] ->
    ((fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
       (fun _ -> s)
       (fun _ -> [])
       n0)
  | _ :: s' ->
    (match s' with
     | [] ->
       ((fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
          (fun _ -> s)
          (fun _ -> [])
          n0)
     | _ :: _ ->
       ((fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
          (fun _ -> s)
          (fun n' -> pop_scope s' n')
          n0))

(** val getn_scope : scope_state -> Big_int_Z.big_int -> scope option **)

let getn_scope =
  nth_error

type label = Big_int_Z.big_int list

(** val generate_label : scope_state -> Big_int_Z.big_int -> label **)

let generate_label s n0 =
  skipn (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
    (foldl (fun lbl scp -> scp.scope_name :: lbl) [] (pop_scope s n0))

(** val r_stk0 : regName **)

let r_stk0 =
  STK

(** val r_fun : regName **)

let r_fun =
  R Big_int_Z.zero_big_int

(** val r_mem : regName **)

let r_mem =
  R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)

(** val r_glob : regName **)

let r_glob =
  R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))

(** val r_table : regName **)

let r_table =
  R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))

(** val r_malloc : regName **)

let r_malloc =
  R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))

(** val r_grow : regName **)

let r_grow =
  R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))

(** val base_reg : Big_int_Z.big_int **)

let base_reg =
  Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))

(** val compile_value : value -> (Big_int_Z.big_int, regName) sum option **)

let compile_value = function
| Val_int n0 -> Some (Inl n0)
| Val_handle _ -> None

type compilation_state = { regidx : Big_int_Z.big_int;
                           current_scope : scope_state }

(** val new_state : Big_int_Z.big_int -> scope_state -> compilation_state **)

let new_state reg0 st =
  { regidx = reg0; current_scope = st }

(** val add_reg :
    compilation_state -> Big_int_Z.big_int -> compilation_state **)

let add_reg s n0 =
  new_state (add s.regidx n0) s.current_scope

(** val sub_reg :
    compilation_state -> Big_int_Z.big_int -> compilation_state **)

let sub_reg s n0 =
  new_state (sub s.regidx n0) s.current_scope

(** val enter_scope :
    compilation_state -> result_type -> Big_int_Z.big_int -> compilation_state **)

let enter_scope s rt ridx =
  new_state s.regidx (push_scope rt ridx s.current_scope)

(** val leave_scope : compilation_state -> compilation_state option **)

let leave_scope s =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun top_scope ->
    match top_scope.scope_ret_type with
    | [] ->
      Some
        (new_state top_scope.scope_reg_base
          (pop_scope s.current_scope (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int)))
    | _ :: l ->
      (match l with
       | [] ->
         Some
           (new_state
             (add top_scope.scope_reg_base (Big_int_Z.succ_big_int
               Big_int_Z.zero_big_int))
             (pop_scope s.current_scope (Big_int_Z.succ_big_int
               Big_int_Z.zero_big_int)))
       | _ :: _ -> None))
    (Obj.magic getn_scope s.current_scope Big_int_Z.zero_big_int)

type cfg = labeled_instr list * compilation_state

(** val dyn_typecheck :
    machineParameters -> value_type -> Big_int_Z.big_int -> Big_int_Z.big_int
    -> cerise_instruction list **)

let dyn_typecheck h vtype reg_idx tmp_reg =
  let tmp_jmp = R
    (add tmp_reg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let tmp_reg0 = R tmp_reg in
  (match vtype with
   | T_int ->
     (IsPtr (tmp_reg0, (R reg_idx))) :: ((Mov (tmp_jmp, (Inr PC))) :: ((Lea
       (tmp_jmp, (Inl
       ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
       ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
       Big_int_Z.unit_big_int))))) :: ((Jnz (tmp_jmp, tmp_reg0)) :: ((Mov
       (tmp_jmp, (Inr PC))) :: ((Lea (tmp_jmp, (Inl
       (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
       Big_int_Z.unit_big_int))))) :: ((Jmp tmp_jmp) :: (Fail :: [])))))))
   | T_handle ->
     app ((IsPtr (tmp_reg0, (R reg_idx))) :: ((Mov (tmp_jmp, (Inr
       PC))) :: ((Lea (tmp_jmp, (Inl (Big_int_Z.mult_int_big_int 2
       (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))) :: ((Jnz
       (tmp_jmp, tmp_reg0)) :: (Fail :: [])))))
       (reqloc_instrs (R reg_idx) (h.encodeLoc Global) tmp_reg0 tmp_jmp))

(** val call_template :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    labeled_instr list -> regName list -> result_type -> result_type ->
    compilation_state -> (labeled_instr list * compilation_state) option **)

let call_template h nreg res prologue args _ ret_type new_state0 =
  let tmp = fun i -> add nreg i in
  let call =
    instrs
      (call_instrs h (R
        (tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) args (R
        (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))) (R
        (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) (R
        (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))) (R
        (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) (R
        (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))
  in
  let epilogue1 = (BInstr (Lea (r_stk0, (Inl (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int))))) :: []
  in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun epilogue2 -> Some
    ((app prologue (app call (app epilogue1 epilogue2))), new_state0))
    (match ret_type with
     | [] ->
       Some
         (Obj.magic ((BInstr (Lea (r_stk0, (Inl (Big_int_Z.minus_big_int
           Big_int_Z.unit_big_int))))) :: []))
     | ret_type0 :: l ->
       (match l with
        | [] ->
          Some
            (Obj.magic app
              (instrs ((LoadU ((R res), r_stk0, (Inl (Big_int_Z.minus_big_int
                Big_int_Z.unit_big_int)))) :: ((Lea (r_stk0, (Inl
                (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: [])))
              (instrs
                (dyn_typecheck h ret_type0 res
                  (tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
        | _ :: _ -> None))

(** val local_offset : Big_int_Z.big_int **)

let local_offset =
  (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)

(** val prologue_return :
    function_type -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> cerise_instruction list option **)

let prologue_return f_type _ ret off tmp2 =
  let ret_type = let Tf (_, r0) = f_type in r0 in
  (match ret_type with
   | [] -> Some []
   | _ :: l ->
     (match l with
      | [] ->
        Some ((GetB ((R tmp2), r_stk0)) :: ((GetA ((R off), r_stk0)) :: ((Sub
          ((R off), (r tmp2), (r off))) :: ((Add ((R off), (r off), (Inl
          (Z.sub local_offset Big_int_Z.unit_big_int)))) :: ((LoadU ((R
          tmp2), r_stk0, (r off))) :: ((StoreU ((R tmp2), (Inl
          Big_int_Z.zero_big_int), (r ret))) :: []))))))
      | _ :: _ -> None))

(** val compile_basic_instr :
    machineParameters -> ws_module -> typeidx -> value_type list ->
    ws_basic_instruction -> compilation_state -> cfg option **)

let rec compile_basic_instr h module0 f_typeidx f_locals i s =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun f_type ->
    let nreg = s.regidx in
    (match i with
     | I_unreachable -> Some ((instrs (Fail :: [])), s)
     | I_nop ->
       Some
         ((instrs ((Mov ((R Big_int_Z.zero_big_int), (Inr (R
            Big_int_Z.zero_big_int)))) :: [])), s)
     | I_drop ->
       Some ([], (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | I_select ->
       let c = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let v2 =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))
       in
       let v1 =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
       in
       let res =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
       in
       Some
       ((instrs ((Mov ((R nreg), (Inr PC))) :: ((Lea ((R nreg), (Inl
          ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
          (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))) :: ((Jnz
          ((R nreg), (R c))) :: ((Mov ((R res), (Inr (R v2)))) :: ((Lea (PC,
          (Inl Big_int_Z.unit_big_int))) :: ((Mov ((R res), (Inr (R
          v1)))) :: []))))))),
       (sub_reg s (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))
     | I_block (rt, body) ->
       let loop_state = enter_scope s rt nreg in
       let lbl_loop =
         generate_label loop_state.current_scope Big_int_Z.zero_big_int
       in
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
         let (compiled_body, _) = pat in
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun new_state0 -> Some
           ((app compiled_body ((Label lbl_loop) :: [])), new_state0))
           (Obj.magic leave_scope loop_state))
         (foldl (fun acc i0 ->
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
             let (instrs_acc, state_acc) = pat in
             mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat0 ->
               let (instrs_comp, state_comp) = pat0 in
               Some ((app instrs_acc instrs_comp), state_comp))
               (compile_basic_instr h module0 f_typeidx f_locals i0 state_acc))
             acc) (Some ([], loop_state)) body)
     | I_loop (rt, body) ->
       let loop_state = enter_scope s rt nreg in
       let lbl_loop =
         generate_label loop_state.current_scope Big_int_Z.zero_big_int
       in
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
         let (compiled_body, _) = pat in
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun new_state0 -> Some
           (((Label lbl_loop) :: compiled_body), new_state0))
           (Obj.magic leave_scope loop_state))
         (foldl (fun acc i0 ->
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
             let (instrs_acc, state_acc) = pat in
             mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat0 ->
               let (instrs_comp, state_comp) = pat0 in
               Some ((app instrs_acc instrs_comp), state_comp))
               (compile_basic_instr h module0 f_typeidx f_locals i0 state_acc))
             acc) (Some ([], loop_state)) body)
     | I_if (rt, body_true, body_false) ->
       let s1 =
         push_scope rt
           (sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
           s.current_scope
       in
       let s2 =
         push_scope rt
           (sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
           (pop_scope s1 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
       in
       let lbl_true = generate_label s1 Big_int_Z.zero_big_int in
       let lbl_end = generate_label s2 Big_int_Z.zero_big_int in
       let bodyF_state = { regidx =
         (sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int));
         current_scope = s2 }
       in
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
         let (bodyF, pre_bodyT_state) = pat in
         let bodyT_state = { regidx =
           (sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int));
           current_scope = pre_bodyT_state.current_scope }
         in
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat0 ->
           let (bodyT, end_state) = pat0 in
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun new_state0 -> Some
             ((app ((Br_Jnz (lbl_true, nreg)) :: [])
                (app bodyF
                  (app ((Br_Jmp (lbl_end, nreg)) :: ((Label lbl_true) :: []))
                    (app bodyT ((Label lbl_end) :: []))))), new_state0))
             (Obj.magic leave_scope end_state))
           (foldl (fun acc i0 ->
             mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat0 ->
               let (instrs_acc, state_acc) = pat0 in
               mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat1 ->
                 let (instrs_comp, state_comp) = pat1 in
                 Some ((app instrs_acc instrs_comp), state_comp))
                 (compile_basic_instr h module0 f_typeidx f_locals i0
                   state_acc)) acc) (Some ([], bodyT_state)) body_true))
         (foldl (fun acc i0 ->
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
             let (instrs_acc, state_acc) = pat in
             mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat0 ->
               let (instrs_comp, state_comp) = pat0 in
               Some ((app instrs_acc instrs_comp), state_comp))
               (compile_basic_instr h module0 f_typeidx f_locals i0 state_acc))
             acc) (Some ([], bodyF_state)) body_false)
     | I_br n0 ->
       let lbl = generate_label s.current_scope n0 in
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun scopen ->
         let rt = scopen.scope_ret_type in
         (match rt with
          | [] -> Some (((Br_Jmp (lbl, nreg)) :: []), s)
          | _ :: l ->
            (match l with
             | [] ->
               let rval =
                 sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
               in
               let base0 = scopen.scope_reg_base in
               let code = (BInstr (Mov ((R nreg), (Inr (R
                 base0))))) :: ((BInstr (Mov ((R base0), (Inr (R
                 rval))))) :: ((Br_Jmp (lbl, nreg)) :: ((BInstr (Mov ((R
                 base0), (Inr (R nreg))))) :: [])))
               in
               Some (code, s)
             | _ :: _ -> None))) (Obj.magic getn_scope s.current_scope n0)
     | I_br_if n0 ->
       let lbl = generate_label s.current_scope n0 in
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun scopen ->
         let rt = scopen.scope_ret_type in
         (match rt with
          | [] ->
            Some (((Br_Jnz (lbl, nreg)) :: []),
              (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
          | _ :: l ->
            (match l with
             | [] ->
               let rval =
                 sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                   Big_int_Z.zero_big_int))
               in
               let base0 = scopen.scope_reg_base in
               let code = (BInstr (Mov ((R nreg), (Inr (R
                 base0))))) :: ((BInstr (Mov ((R base0), (Inr (R
                 rval))))) :: ((Br_Jnz (lbl, nreg)) :: ((BInstr (Mov ((R
                 base0), (Inr (R nreg))))) :: [])))
               in
               Some (code,
               (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
             | _ :: _ -> None))) (Obj.magic getn_scope s.current_scope n0)
     | I_return ->
       let ret = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let tmp2 = add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let jaddr = add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       mbind (Obj.magic (fun _ _ -> option_bind))
         (fun prepare_return_instrs -> Some
         ((instrs
            (app prepare_return_instrs
              (app ((GetB ((R nreg), r_stk0)) :: ((GetA ((R tmp2),
                r_stk0)) :: ((Sub ((R nreg), (r nreg), (r tmp2))) :: ((LoadU
                ((R jaddr), r_stk0, (r nreg))) :: []))))
                (app
                  (rclear_instrs
                    (list_difference reg_eq_dec all_registers (PC :: ((R
                      jaddr) :: (r_stk0 :: []))))) ((Jmp (R jaddr)) :: []))))),
         s)) (Obj.magic prologue_return f_type nreg ret nreg tmp2)
     | I_call i0 ->
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
         let Tf (arg_type, ret_type) = pat in
         let len_arg_type = length arg_type in
         let tmp = fun i1 -> add nreg i1 in
         let res = sub nreg len_arg_type in
         let args' = seq (sub nreg len_arg_type) len_arg_type in
         let args = map (fun x -> R x) args' in
         let prologue =
           instrs ((Lea (r_fun, (Inl (Z.of_nat i0)))) :: ((Load ((R
             (tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
             r_fun)) :: ((Lea (r_fun, (Inl (Z.opp (Z.of_nat i0))))) :: [])))
         in
         let new_state0 = sub_reg (add_reg s (length ret_type)) len_arg_type
         in
         call_template h nreg res prologue args arg_type ret_type new_state0)
         (Obj.magic get_function_type module0 i0)
     | I_call_indirect i0 ->
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
         let Tf (arg_type, ret_type) = pat in
         let len_arg_type = length arg_type in
         let nfun = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
         in
         let res = sub nfun len_arg_type in
         let tmp = fun i1 -> add nreg i1 in
         let args' = seq (sub nfun len_arg_type) len_arg_type in
         let args = map (fun x -> R x) args' in
         let prologue =
           instrs ((Lea (r_table, (r nfun))) :: ((Load ((R
             (tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
             r_table)) :: ((Sub ((R nfun), (Inl Big_int_Z.zero_big_int),
             (r nfun))) :: ((Lea (r_table, (r nfun))) :: []))))
         in
         let new_state0 =
           sub_reg (add_reg s (length ret_type))
             (add len_arg_type (Big_int_Z.succ_big_int
               Big_int_Z.zero_big_int))
         in
         call_template h nreg res prologue args arg_type ret_type new_state0)
         (Obj.magic get_type module0 i0)
     | I_get_local i0 ->
       let off = add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some
       ((instrs ((GetB ((R nreg), r_stk0)) :: ((Add ((R nreg), (r nreg), (Inl
          (Z.of_nat i0)))) :: ((GetA ((R off), r_stk0)) :: ((Sub ((R off),
          (r nreg), (r off))) :: ((Add ((R off), (r off), (Inl
          local_offset))) :: ((LoadU ((R nreg), r_stk0, (r off))) :: []))))))),
       (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | I_set_local i0 ->
       let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let off = add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some
       ((instrs ((GetB ((R nreg), r_stk0)) :: ((Add ((R nreg), (r nreg), (Inl
          (Z.of_nat i0)))) :: ((GetA ((R off), r_stk0)) :: ((Sub ((R off),
          (r nreg), (r off))) :: ((Add ((R off), (r off), (Inl
          local_offset))) :: ((StoreU (r_stk0, (r off), (r v))) :: []))))))),
       (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | I_tee_local i0 ->
       let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let off = add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some
       ((instrs ((GetB ((R nreg), r_stk0)) :: ((Add ((R nreg), (r nreg), (Inl
          (Z.of_nat i0)))) :: ((GetA ((R off), r_stk0)) :: ((Sub ((R off),
          (r nreg), (r off))) :: ((Add ((R off), (r off), (Inl
          local_offset))) :: ((StoreU (r_stk0, (r off), (r v))) :: []))))))),
       s)
     | I_get_global i0 ->
       Some
         ((instrs ((Lea (r_glob, (Inl (Z.of_nat i0)))) :: ((Load ((R nreg),
            r_glob)) :: ((Lea (r_glob, (Inl (Z.opp (Z.of_nat i0))))) :: [])))),
         (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | I_set_global i0 ->
       let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some
       ((instrs ((Lea (r_glob, (Inl (Z.of_nat i0)))) :: ((Store (r_glob,
          (r v))) :: ((Lea (r_glob, (Inl (Z.opp (Z.of_nat i0))))) :: [])))),
       (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | I_load _ ->
       let a = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some
       ((instrs
          (app ((Load ((R nreg), r_mem)) :: ((Lea ((R nreg),
            (r a))) :: ((Load ((R res), (R nreg))) :: [])))
            (dyn_typecheck h T_int res nreg))), s)
     | I_store _ ->
       let a =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))
       in
       let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some
       ((instrs ((Load ((R nreg), r_mem)) :: ((Lea ((R nreg),
          (r a))) :: ((Store ((R nreg), (r v))) :: [])))),
       (sub_reg s (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))
     | I_segload _ ->
       let h0 = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some
       ((instrs
          (app ((Load ((R res), (R h0))) :: [])
            (dyn_typecheck h T_int res nreg))), s)
     | I_segstore _ ->
       let h0 =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))
       in
       let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some ((instrs ((Store ((R h0), (r v))) :: [])),
       (sub_reg s (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))
     | I_slice ->
       let h0 =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
       in
       let o1 =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))
       in
       let o2 = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let tmp2 = add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let res =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
       in
       Some
       ((instrs ((GetB ((R nreg), (R h0))) :: ((Add ((R o1), (r o1),
          (r nreg))) :: ((GetE ((R tmp2), (R h0))) :: ((Sub ((R o2),
          (r tmp2), (r o2))) :: ((Subseg ((R h0), (r o1), (r o2))) :: ((Mov
          ((R res), (r h0))) :: []))))))),
       (sub_reg s (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))
     | I_segalloc ->
       let size = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some
       ((instrs
          (app
            (malloc_instrs r_malloc (R size) nreg
              (add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
              (add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                Big_int_Z.zero_big_int)))
              (add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))
              (add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                Big_int_Z.zero_big_int)))))
              (add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) ((Mov
            ((R res), (r nreg))) :: []))), s)
     | I_handleadd ->
       let h0 = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let off =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))
       in
       let res =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))
       in
       Some
       ((instrs ((Lea ((R h0), (r off))) :: ((Mov ((R res), (r h0))) :: []))),
       (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | I_current_memory ->
       let tmp1 = add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some
       ((instrs ((GetB ((R tmp1), r_mem)) :: ((GetE ((R nreg),
          r_mem)) :: ((Sub ((R nreg), (Inr (R nreg)), (Inr (R
          tmp1)))) :: [])))),
       (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | I_grow_memory ->
       let size = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let tmp_res = add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
       in
       let tmp = fun i0 -> add tmp_res i0 in
       let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some
       ((instrs
          (app
            (grow_instrs r_grow (R size) tmp_res nreg
              (tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
              (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                Big_int_Z.zero_big_int)))
              (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))
              (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                Big_int_Z.zero_big_int)))))
              (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))
              (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                Big_int_Z.zero_big_int)))))))
              (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))) ((Mov
            ((R res), (r tmp_res))) :: []))), s)
     | I_const v ->
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun val0 -> Some
         ((instrs ((Mov ((R nreg), val0)) :: [])),
         (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))
         (Obj.magic compile_value v)
     | I_binop (_, op) ->
       let v1 =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))
       in
       let v2 = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let res =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))
       in
       Some
       ((instrs
          (match op with
           | BOI_add -> (Add ((R res), (r v1), (r v2))) :: []
           | BOI_sub -> (Sub ((R res), (r v1), (r v2))) :: []
           | BOI_mul -> (Mul ((R res), (r v1), (r v2))) :: []
           | BOI_rem _ -> (Rem ((R res), (r v1), (r v2))) :: []
           | BOI_div _ -> (Div ((R res), (r v1), (r v2))) :: [])),
       (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | I_testop _ ->
       let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some ((instrs (eqz_instrs (R res) (R v) (R nreg))), s)
     | I_relop (_, op) ->
       let v1 =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))
       in
       let v2 = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let res =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))
       in
       Some
       ((instrs
          (match op with
           | ROI_eq -> eq_instrs (R res) (R v1) (R v2) (R nreg)
           | ROI_ne -> neq_instrs (R res) (R v1) (R v2) (R nreg)
           | ROI_lt _ -> (Lt ((R res), (Inr (R v1)), (Inr (R v2)))) :: []
           | ROI_gt _ -> (Lt ((R res), (Inr (R v2)), (Inr (R v1)))) :: []
           | ROI_le _ -> ge_instrs (R res) (R v2) (R v1) (R nreg)
           | ROI_ge _ -> ge_instrs (R res) (R v1) (R v2) (R nreg))),
       (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | _ -> None)) (Obj.magic get_type module0 f_typeidx)

(** val labeled_compile_expr' :
    machineParameters -> ws_module -> typeidx -> value_type list ->
    ws_basic_instruction list -> compilation_state -> (labeled_instr
    list * compilation_state) option **)

let rec labeled_compile_expr' h module0 f_typeidx f_locals il s =
  match il with
  | [] -> Some ([], s)
  | i :: il' ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
      let (instrs_comp, s_comp) = pat in
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat0 ->
        let (instr_rec, state_rec) = pat0 in
        Some ((app instrs_comp instr_rec), state_rec))
        (labeled_compile_expr' h module0 f_typeidx f_locals il' s_comp))
      (compile_basic_instr h module0 f_typeidx f_locals i s)

(** val labeled_compile_expr :
    machineParameters -> ws_module -> typeidx -> value_type list ->
    ws_basic_instruction list -> compilation_state -> (labeled_instr
    list * compilation_state) option **)

let labeled_compile_expr =
  labeled_compile_expr'

(** val compile_import_desc : symbols -> import_desc -> symbols **)

let compile_import_desc s _ =
  s

(** val compile_import : module_import -> symbols **)

let compile_import imp =
  let symbol = symbols_encode imp.imp_module imp.imp_name in
  compile_import_desc symbol imp.imp_desc

(** val compile_imports : module_import list -> symbols list **)

let rec compile_imports = function
| [] -> []
| imp :: imps' -> (compile_import imp) :: (compile_imports imps')

(** val compile_exp_desc : module_export_desc -> Big_int_Z.big_int **)

let compile_exp_desc desc =
  desc

(** val compile_exports :
    module_export list -> name -> Big_int_Z.big_int -> (addr, word) gmap ->
    (symbols, word) gmap **)

let rec compile_exports exps module_name offset0 segment0 =
  match exps with
  | [] -> gmap_empty string_eq_dec string_countable
  | exp :: exps' ->
    let s = symbols_encode module_name exp.modexp_name in
    let nid = compile_exp_desc exp.modexp_desc in
    let w =
      match lookup0 (gmap_lookup Coq_Nat.eq_dec nat_countable)
              (add nid offset0) segment0 with
      | Some w -> w
      | None -> Inl Big_int_Z.zero_big_int
    in
    insert0 (map_insert (gmap_partial_alter string_eq_dec string_countable))
      s w (compile_exports exps' module_name offset0 segment0)

(** val load_args :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> result_type -> cerise_instruction list **)

let rec load_args mP arg tmp2 tmp3 tf =
  let rarg = R arg in
  let rtmp2 = R tmp2 in
  (match tf with
   | [] -> []
   | t :: tf' ->
     app ((LoadU (rtmp2, rarg, (Inl (Big_int_Z.minus_big_int
       Big_int_Z.unit_big_int)))) :: ((Lea (rarg, (Inl
       (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: []))
       (app (dyn_typecheck mP t tmp2 tmp3)
         (app ((StoreU (r_stk0, (Inl Big_int_Z.zero_big_int), (Inr
           rtmp2))) :: []) (load_args mP arg tmp2 tmp3 tf'))))

(** val prologue_function :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> function_type -> Big_int_Z.big_int -> labeled_instr
    list **)

let prologue_function mP tmp tmp2 tmp3 tf size_locals =
  let rtmp = R tmp in
  let prepare_locals =
    let rec prepare_locals n0 =
      (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ -> [])
        (fun n' -> (StoreU (r_stk0, (Inl Big_int_Z.zero_big_int), (Inl
        Big_int_Z.zero_big_int))) :: (prepare_locals n'))
        n0
    in prepare_locals
  in
  let Tf (rt, _) = tf in
  app
    (instrs
      (app ((Mov (rtmp, (Inr PC))) :: ((Lea (rtmp, (Inl
        (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: ((Load (rtmp,
        rtmp)) :: ((Load (r_fun, rtmp)) :: ((Lea (rtmp, (Inl
        Big_int_Z.unit_big_int))) :: ((Load (r_mem, rtmp)) :: ((Lea (rtmp,
        (Inl Big_int_Z.unit_big_int))) :: ((Load (r_glob, rtmp)) :: ((Lea
        (rtmp, (Inl Big_int_Z.unit_big_int))) :: ((Load (r_table,
        rtmp)) :: ((Lea (rtmp, (Inl Big_int_Z.unit_big_int))) :: ((Load
        (r_malloc, rtmp)) :: ((Lea (rtmp, (Inl
        Big_int_Z.unit_big_int))) :: ((Load (r_grow,
        rtmp)) :: []))))))))))))))
        (app
          (match rt with
           | [] -> []
           | _ :: _ ->
             (LoadU (rtmp, r_stk0, (Inl (Big_int_Z.minus_big_int
               Big_int_Z.unit_big_int)))) :: ((Lea (r_stk0, (Inl
               (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: []))
          (load_args mP tmp tmp2 tmp3 rt))))
    (instrs (prepare_locals size_locals))

(** val compile_expr_mod :
    machineParameters -> ws_module -> typeidx -> value_type list ->
    ws_basic_instruction list -> compilation_state -> (labeled_instr
    list * compilation_state) option **)

let compile_expr_mod mP module0 f_typeidx f_locals il s =
  let fe = app il (I_return :: []) in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
    let (body, state) = pat in
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun f_type ->
      let nreg = s.regidx in
      Some
      ((app
         (prologue_function mP nreg
           (add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
           (add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
             Big_int_Z.zero_big_int))) f_type (length f_locals)) body), state))
      (Obj.magic get_type module0 f_typeidx))
    (labeled_compile_expr mP module0 f_typeidx f_locals fe s)

(** val frame_segment :
    Big_int_Z.big_int -> Big_int_Z.big_int -> (addr, word) gmap **)

let frame_segment addr_fun len_fun =
  let placeholder = Inr ((((O, Global), Big_int_Z.zero_big_int),
    Big_int_Z.zero_big_int), Big_int_Z.zero_big_int)
  in
  let cap_fun = Inr ((((RO, Global), addr_fun), (add addr_fun len_fun)),
    addr_fun)
  in
  list_to_addr_gmap
    (cap_fun :: (placeholder :: (placeholder :: (placeholder :: (placeholder :: (placeholder :: []))))))
    Big_int_Z.zero_big_int

(** val get_start :
    ws_module -> (addr, word) gmap -> Big_int_Z.big_int -> word option option **)

let get_start m segment0 offset0 =
  match m.mod_start with
  | Some mstart ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun w -> Some (Some w))
      (lookup0 (Obj.magic gmap_lookup Coq_Nat.eq_dec nat_countable)
        (add offset0 (modstart_func mstart)) segment0)
  | None -> Some None

(** val compile_func :
    machineParameters -> lMachineParameters -> module_func -> ws_module ->
    compilation_state -> (word list * compilation_state) option **)

let compile_func mP lMP f m s =
  let init_compilation_state = { regidx = base_reg; current_scope =
    s.current_scope }
  in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
    let (compiled_body, compiled_state) = pat in
    let frame_cap = ((((RO, Global), Big_int_Z.zero_big_int), base_reg),
      Big_int_Z.zero_big_int)
    in
    Some (((Inr frame_cap) :: (l_encodeInstrsW lMP compiled_body)),
    compiled_state))
    (Obj.magic compile_expr_mod mP m f.modfunc_type f.modfunc_locals
      f.modfunc_body init_compilation_state)

(** val compile_funcs' :
    machineParameters -> lMachineParameters -> ws_module -> module_func list
    -> Big_int_Z.big_int -> compilation_state -> (cap list * word list) option **)

let rec compile_funcs' mP lMP m funcs offset0 s =
  match funcs with
  | [] -> Some ([], [])
  | f :: funcs' ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
      let (compiled_f, compiled_state) = pat in
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat0 ->
        let (acc_cap, acc_mem) = pat0 in
        let cap0 = ((((E, Global), offset0),
          (add offset0 (length compiled_f))),
          (add offset0 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
        in
        Some ((cap0 :: acc_cap), (app compiled_f acc_mem)))
        (compile_funcs' mP lMP m funcs' (add offset0 (length compiled_f))
          compiled_state)) (Obj.magic compile_func mP lMP f m s)

(** val compile_funcs :
    machineParameters -> lMachineParameters -> ws_module -> Big_int_Z.big_int
    -> (addr, word) gmap option **)

let compile_funcs mP lMP m offset_start =
  let init_compilation_state = { regidx = base_reg; current_scope =
    init_scope_state }
  in
  let offset_local_fun = length m.mod_funcs in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
    let (caps, mem0) = pat in
    Some
    (list_to_addr_gmap (app (map (fun c -> Inr c) caps) mem0) offset_start))
    (Obj.magic compile_funcs' mP lMP m m.mod_funcs
      (add offset_start offset_local_fun) init_compilation_state)

(** val compile_module :
    machineParameters -> lMachineParameters -> ws_module -> name ->
    labeled_cerise_component option **)

let compile_module mP lMP m module_name =
  let offset_imports = length m.mod_imports in
  let compiled_imports =
    list_to_addr_gmap (compile_imports m.mod_imports) base_reg
  in
  let offset_local_funs = add offset_imports base_reg in
  let frame_caps =
    frame_segment base_reg (add offset_imports (length m.mod_funcs))
  in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_segment ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun start_word -> Some
      { l_segment = (merge_mem frame_caps compiled_segment); l_imports =
      compiled_imports; l_exports =
      (compile_exports m.mod_exports module_name offset_local_funs
        compiled_segment); l_nfunctions = (length m.mod_funcs); l_main =
      start_word }) (Obj.magic get_start m compiled_segment base_reg))
    (Obj.magic compile_funcs mP lMP m offset_local_funs)

(** val addresses_labels' :
    labeled_instr list -> Big_int_Z.big_int -> (label, Big_int_Z.big_int) gmap **)

let rec addresses_labels' il addr0 =
  match il with
  | [] ->
    gmap_empty (list_eq_dec0 Coq_Nat.eq_dec)
      (list_countable Coq_Nat.eq_dec nat_countable)
  | l :: il' ->
    (match l with
     | Label lbl ->
       insert0
         (map_insert
           (gmap_partial_alter (list_eq_dec0 Coq_Nat.eq_dec)
             (list_countable Coq_Nat.eq_dec nat_countable))) lbl addr0
         (addresses_labels' il' addr0)
     | BInstr _ ->
       addresses_labels' il'
         (add addr0 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
     | _ ->
       addresses_labels' il'
         (add addr0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))

(** val addresses_labels :
    labeled_instr list -> (label, Big_int_Z.big_int) gmap **)

let addresses_labels il =
  addresses_labels' il Big_int_Z.zero_big_int

(** val branch_labels' :
    labeled_instr list -> (label, Big_int_Z.big_int) gmap ->
    Big_int_Z.big_int -> cerise_instruction list option **)

let rec branch_labels' il label_map addr0 =
  match il with
  | [] -> Some []
  | l :: il' ->
    (match l with
     | Label _ -> branch_labels' il' label_map addr0
     | BInstr i ->
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun next -> Some
         (i :: next))
         (branch_labels' il' label_map
           (add addr0 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | Br_Jmp (lbl, reg_tmp) ->
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun o ->
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun next ->
           let off = Z.sub (Z.of_nat o) (Z.of_nat addr0) in
           Some
           (app ((Mov ((R reg_tmp), (Inr PC))) :: ((Lea ((R reg_tmp), (Inl
             off))) :: ((Jmp (R reg_tmp)) :: []))) next))
           (branch_labels' il' label_map
             (add addr0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
               (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))
         (lookup0
           (Obj.magic gmap_lookup (list_eq_dec0 Coq_Nat.eq_dec)
             (list_countable Coq_Nat.eq_dec nat_countable)) lbl label_map)
     | Br_Jnz (lbl, reg_tmp) ->
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun o ->
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun next ->
           let reg_cond =
             sub reg_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
           in
           let off = Z.sub (Z.of_nat o) (Z.of_nat addr0) in
           Some
           (app ((Mov ((R reg_tmp), (Inr PC))) :: ((Lea ((R reg_tmp), (Inl
             off))) :: ((Jnz ((R reg_tmp), (R reg_cond))) :: []))) next))
           (branch_labels' il' label_map
             (add addr0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
               (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))
         (lookup0
           (Obj.magic gmap_lookup (list_eq_dec0 Coq_Nat.eq_dec)
             (list_countable Coq_Nat.eq_dec nat_countable)) lbl label_map))

(** val branch_labels :
    labeled_instr list -> cerise_instruction list option **)

let branch_labels il =
  branch_labels' il (addresses_labels il) Big_int_Z.zero_big_int

(** val get_interval_mem :
    (addr, word) gmap -> Big_int_Z.big_int -> Big_int_Z.big_int -> word list
    option **)

let get_interval_mem lsegment b e =
  fold_right (fun n0 l_word_opt ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun l_word ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun current_word -> Some
        (current_word :: l_word))
        (lookup0 (Obj.magic gmap_lookup Coq_Nat.eq_dec nat_countable) n0
          lsegment)) l_word_opt) (Some []) (seq b (sub e b))

(** val get_cap_from_word : word -> cap option **)

let get_cap_from_word = function
| Inl _ -> None
| Inr cap0 -> Some cap0

(** val extract_frame_segment : (addr, word) gmap -> cap list option **)

let extract_frame_segment lsegment =
  let b_frame_addr = Big_int_Z.zero_big_int in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun frame_words ->
    fold_right (fun w l_caps_opt ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun l_caps ->
        mbind (Obj.magic (fun _ _ -> option_bind)) (fun cap0 -> Some
          (cap0 :: l_caps)) (Obj.magic get_cap_from_word w)) l_caps_opt)
      (Some []) frame_words)
    (Obj.magic get_interval_mem lsegment b_frame_addr base_reg)

(** val extract_local_linking_table :
    (addr, word) gmap -> Big_int_Z.big_int -> Big_int_Z.big_int -> cap list
    option **)

let extract_local_linking_table lsegment offset_local_lt n_functions =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun frame_words ->
    fold_right (fun w l_caps_opt ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun l_caps ->
        mbind (Obj.magic (fun _ _ -> option_bind)) (fun cap0 -> Some
          (cap0 :: l_caps)) (Obj.magic get_cap_from_word w)) l_caps_opt)
      (Some []) frame_words)
    (Obj.magic get_interval_mem lsegment offset_local_lt
      (add offset_local_lt n_functions))

(** val extract_function :
    lMachineParameters -> cap -> (addr, word) gmap -> labeled_instr list
    option **)

let extract_function lMP cap0 lsegment =
  let (p, b) = cap0 in
  let (_, e) = p in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun function_words ->
    fold_right (fun w l_instrs_opt ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun l_instrs ->
        match w with
        | Inl z0 -> Some ((lMP.l_decodeInstr z0) :: l_instrs)
        | Inr _ -> None) l_instrs_opt) (Some []) function_words)
    (Obj.magic get_interval_mem lsegment b e)

(** val extract_functions :
    lMachineParameters -> (addr, word) gmap -> cap list -> labeled_instr list
    list option **)

let extract_functions lMP lsegment local_linking_table =
  fold_right (fun cap0 l_functions_opt ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun l_functions ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun current_func -> Some
        (current_func :: l_functions))
        (Obj.magic extract_function lMP cap0 lsegment)) l_functions_opt)
    (Some []) local_linking_table

(** val compile_functions :
    machineParameters -> labeled_instr list list -> Big_int_Z.big_int -> (cap
    list * word list) option **)

let rec compile_functions mP functions offset0 =
  match functions with
  | [] -> Some ([], [])
  | linstrs :: functions' ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_instrs ->
      let frame_capability = ((((RO, Global), Big_int_Z.zero_big_int),
        base_reg), Big_int_Z.zero_big_int)
      in
      let mem0 = (Inr frame_capability) :: (encodeInstrsW mP compiled_instrs)
      in
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
        let (acc_cap, acc_mem) = pat in
        let cap0 = ((((E, Global), offset0),
          (add (add offset0 (length compiled_instrs)) (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int))),
          (add offset0 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
        in
        Some ((cap0 :: acc_cap), (app mem0 acc_mem)))
        (compile_functions mP functions' (add offset0 (length mem0))))
      (Obj.magic branch_labels linstrs)

(** val create_translation_caps :
    cap list -> cap list -> (word, word) gmap **)

let rec create_translation_caps old new0 =
  match old with
  | [] -> gmap_empty word_eq_dec word_countable
  | c_old :: old' ->
    (match new0 with
     | [] -> gmap_empty word_eq_dec word_countable
     | c_new :: new' ->
       insert0 (map_insert (gmap_partial_alter word_eq_dec word_countable))
         (Inr c_old) (Inr c_new) (create_translation_caps old' new'))

(** val compile_segment :
    machineParameters -> lMachineParameters -> (addr, word) gmap ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> ((addr, word) gmap * (word,
    word) gmap) option **)

let compile_segment mP lMP lsegment offset_import n_local_functions =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun frame ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun local_linking_table ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun functions ->
        let offset_locals = length local_linking_table in
        mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
          let (caps, mem0) = pat in
          let translate_cap0 =
            create_translation_caps local_linking_table caps
          in
          let memseg =
            list_to_addr_gmap (app (map (fun c -> Inr c) caps) mem0)
              (add base_reg offset_import)
          in
          let frameseg =
            list_to_addr_gmap (map (fun c -> Inr c) frame)
              Big_int_Z.zero_big_int
          in
          let segment0 =
            union0
              (map_union
                (Obj.magic (fun _ _ _ ->
                  gmap_merge Coq_Nat.eq_dec nat_countable))) memseg frameseg
          in
          Some (segment0, translate_cap0))
          (Obj.magic compile_functions mP functions
            (add (add base_reg offset_locals) offset_import)))
        (Obj.magic extract_functions lMP lsegment local_linking_table))
      (Obj.magic extract_local_linking_table lsegment
        (add base_reg offset_import) n_local_functions))
    (Obj.magic extract_frame_segment lsegment)

(** val translate_cap : (word, word) gmap -> word -> word **)

let translate_cap dict w =
  match lookup0 (gmap_lookup word_eq_dec word_countable) w dict with
  | Some w' -> w'
  | None -> w

(** val compile_component :
    machineParameters -> lMachineParameters -> labeled_cerise_component ->
    cerise_component option **)

let compile_component mP lMP m =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
    let (compiled_segment, caps_translation) = pat in
    let size_component0 =
      add
        (length (gmap_to_list Coq_Nat.eq_dec nat_countable compiled_segment))
        (length (gmap_to_list Coq_Nat.eq_dec nat_countable m.l_imports))
    in
    let main_word =
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun w -> Some
        (translate_cap caps_translation w)) m.l_main
    in
    Some { segment = compiled_segment; imports = m.l_imports; exports =
    (fmap (Obj.magic (fun _ _ -> gmap_fmap string_eq_dec string_countable))
      (translate_cap caps_translation) m.l_exports); submodules =
    ((Big_int_Z.zero_big_int, size_component0) :: []); main = main_word })
    (Obj.magic compile_segment mP lMP m.l_segment
      (length (gmap_to_list Coq_Nat.eq_dec nat_countable m.l_imports))
      m.l_nfunctions)

(** val map_compose :
    'a4 fMap -> (__ -> ('a1, __, 'a4) lookup) -> (__ -> 'a4 empty) -> (__ ->
    ('a1, __, 'a4) partialAlter) -> 'a4 oMap -> 'a4 merge -> (__ -> ('a1, __,
    'a4) finMapToList) -> ('a1, 'a1) relDecision -> 'a5 fMap -> (__ -> ('a2,
    __, 'a5) lookup) -> (__ -> 'a5 empty) -> (__ -> ('a2, __, 'a5)
    partialAlter) -> 'a5 oMap -> 'a5 merge -> (__ -> ('a2, __, 'a5)
    finMapToList) -> ('a2, 'a2) relDecision -> 'a5 -> 'a4 -> 'a4 **)

let map_compose _ _ _ _ h4 _ _ _ _ h8 _ _ _ _ _ _ m n0 =
  omap h4 (fun i -> lookup0 (h8 __) i m) n0

(** val resolve_imports :
    (addr, symbols) gmap -> (symbols, word) gmap -> (addr, word) gmap ->
    (addr, word) gmap **)

let resolve_imports imp exp ms =
  union0
    (map_union
      (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
    (map_compose
      (Obj.magic (fun _ _ -> gmap_fmap Coq_Nat.eq_dec nat_countable))
      (Obj.magic (fun _ -> gmap_lookup Coq_Nat.eq_dec nat_countable))
      (fun _ -> gmap_empty Coq_Nat.eq_dec nat_countable)
      (Obj.magic (fun _ -> gmap_partial_alter Coq_Nat.eq_dec nat_countable))
      (Obj.magic (fun _ _ -> gmap_omap Coq_Nat.eq_dec nat_countable))
      (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable))
      (Obj.magic (fun _ -> gmap_to_list Coq_Nat.eq_dec nat_countable))
      Coq_Nat.eq_dec
      (Obj.magic (fun _ _ -> gmap_fmap string_eq_dec string_countable))
      (Obj.magic (fun _ -> gmap_lookup string_eq_dec string_countable))
      (fun _ -> gmap_empty string_eq_dec string_countable)
      (Obj.magic (fun _ -> gmap_partial_alter string_eq_dec string_countable))
      (Obj.magic (fun _ _ -> gmap_omap string_eq_dec string_countable))
      (Obj.magic (fun _ _ _ -> gmap_merge string_eq_dec string_countable))
      (Obj.magic (fun _ -> gmap_to_list string_eq_dec string_countable))
      string_eq_dec exp (Obj.magic imp)) ms

(** val size_component : cerise_component -> Big_int_Z.big_int **)

let size_component comp =
  add (length (gmap_to_list Coq_Nat.eq_dec nat_countable comp.segment))
    (length (gmap_to_list Coq_Nat.eq_dec nat_countable comp.imports))

(** val shift_cap : cap -> Big_int_Z.big_int -> cap **)

let shift_cap c n0 =
  let (p0, a) = c in
  let (p1, e) = p0 in
  let (p2, b) = p1 in (((p2, (add b n0)), (add e n0)), (add a n0))

(** val shift_segment :
    (addr, word) gmap -> Big_int_Z.big_int -> (addr, word) gmap **)

let shift_segment m n0 =
  let shift_addr =
    kmap (fun _ ->
      map_insert (gmap_partial_alter Coq_Nat.eq_dec nat_countable)) (fun _ ->
      gmap_empty Coq_Nat.eq_dec nat_countable)
      (Obj.magic (fun _ -> gmap_to_list Coq_Nat.eq_dec nat_countable))
      (fun a -> add a n0) m
  in
  fmap (Obj.magic (fun _ _ -> gmap_fmap Coq_Nat.eq_dec nat_countable))
    (fun w ->
    match w with
    | Inl _ -> w
    | Inr y ->
      let (y0, a) = y in
      let (y1, e) = y0 in
      let (y2, b) = y1 in
      let (p, g) = y2 in Inr ((((p, g), (add b n0)), (add e n0)), (add a n0)))
    (Obj.magic shift_addr)

(** val shift_component :
    cerise_component -> Big_int_Z.big_int -> cerise_component **)

let shift_component comp n0 =
  let main_word =
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun main_word -> Some
      (match main_word with
       | Inl z0 -> Inl z0
       | Inr c -> Inr (shift_cap c n0))) comp.main
  in
  { segment = (shift_segment comp.segment n0); imports =
  (kmap (fun _ ->
    map_insert (Obj.magic gmap_partial_alter Coq_Nat.eq_dec nat_countable))
    (fun _ -> gmap_empty Coq_Nat.eq_dec nat_countable)
    (Obj.magic (fun _ -> gmap_to_list Coq_Nat.eq_dec nat_countable))
    (fun a -> add a n0) comp.imports); exports =
  (fmap (Obj.magic (fun _ _ -> gmap_fmap string_eq_dec string_countable))
    (fun w ->
    match w with
    | Inl _ -> w
    | Inr y ->
      let (y0, a) = y in
      let (y1, e) = y0 in
      let (y2, b) = y1 in
      let (p, g) = y2 in Inr ((((p, g), (add b n0)), (add e n0)), (add a n0)))
    comp.exports); submodules =
  (map (fun a -> ((add (fst a) n0), (add (snd a) n0))) comp.submodules);
  main = main_word }

(** val resolve_main :
    cerise_component -> cerise_component -> word option option **)

let resolve_main comp_l comp_r =
  match comp_l.main with
  | Some m -> (match comp_r.main with
               | Some _ -> None
               | None -> Some (Some m))
  | None -> Some comp_r.main

(** val link :
    cerise_component -> cerise_component -> cerise_component option **)

let link comp_l comp_r =
  let comp_r0 = shift_component comp_r (size_component comp_l) in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun main_word -> Some
    { segment =
    (union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
      (resolve_imports comp_l.imports comp_r0.exports comp_l.segment)
      (resolve_imports comp_r0.imports comp_l.exports comp_r0.segment));
    imports =
    (filter0 (fun _ ->
      map_filter (gmap_to_list Coq_Nat.eq_dec nat_countable)
        (map_insert (gmap_partial_alter Coq_Nat.eq_dec nat_countable))
        (gmap_empty Coq_Nat.eq_dec nat_countable))
      (uncurry_dec (fun _ y ->
        option_eq_None_dec
          (lookup0 (gmap_lookup string_eq_dec string_countable) y
            (union0
              (map_union
                (Obj.magic (fun _ _ _ ->
                  gmap_merge string_eq_dec string_countable))) comp_l.exports
              comp_r0.exports))))
      (union0
        (map_union
          (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
        comp_l.imports comp_r0.imports)); exports =
    (union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge string_eq_dec string_countable)))
      comp_l.exports comp_r0.exports); submodules =
    (app comp_l.submodules comp_r0.submodules); main = main_word })
    (Obj.magic resolve_main comp_l comp_r0)

type reg = (regName, word) gmap

type mem = (addr, word) gmap

(** val reg_insert : (regName, word, (regName, word) gmap empty) insert **)

let reg_insert =
  map_insert (gmap_partial_alter reg_eq_dec reg_countable)

(** val load :
    machineParameters -> cerise_component -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> reg * mem **)

let load h prog size_safe_mem size_lin_mem size_globals size_table start_stack end_stack =
  let allocate_mem = fun z0 n0 -> repeat (Inl z0) n0 in
  let size_prog = size_component prog in
  let end_grow_routine = add size_prog (length grow_subroutine_instrs) in
  let grow_routine_sentry = ((((E, Global), size_prog), end_grow_routine),
    size_prog)
  in
  let start_safe_mem = add end_grow_routine (length malloc_subroutine_instrs)
  in
  let end_safe_mem =
    add (add start_safe_mem (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
      size_safe_mem
  in
  let safe_mem =
    let free_space_malloc_cap = ((((RW, Global), start_safe_mem),
      end_safe_mem),
      (add start_safe_mem (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
    in
    app (encodeInstrsW h malloc_subroutine_instrs)
      (app ((Inr free_space_malloc_cap) :: [])
        (allocate_mem Big_int_Z.zero_big_int size_safe_mem))
  in
  let cap_safe_mem = Inr ((((E, Global), end_grow_routine), end_safe_mem),
    end_grow_routine)
  in
  let size_datas =
    add
      (add
        (add
          (add (length grow_instrs_main) (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) size_lin_mem)
        size_globals) size_table
  in
  let start_grow_main = fun n0 -> add end_safe_mem (mul n0 size_datas) in
  let start_lin_mem = fun n0 ->
    add
      (add (start_grow_main n0) (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
      (length grow_instrs_main)
  in
  let start_globals = fun n0 -> add (start_lin_mem n0) size_lin_mem in
  let start_table = fun n0 -> add (start_globals n0) size_globals in
  let end_datas = fun n0 -> add (start_table n0) size_table in
  let init_lin_size = Big_int_Z.zero_big_int in
  let grow_lin_mem = fun n0 write_cap ->
    let b_mega = start_lin_mem n0 in
    let e_mega = start_globals n0 in
    let a_mega = add b_mega init_lin_size in
    let mega_cap = Inr ((((RW, Global), b_mega), e_mega), a_mega) in
    let lin_mem_cap = Inr ((((RW, Global), b_mega), a_mega), b_mega) in
    app (write_cap :: (mega_cap :: (lin_mem_cap :: ((Inr
      grow_routine_sentry) :: []))))
      (app (encodeInstrsW h grow_instrs_main)
        (allocate_mem Big_int_Z.zero_big_int size_lin_mem))
  in
  let lin_mem_cap = fun n0 ->
    let lin_mem_addr =
      add (start_grow_main n0) (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
    in
    Inr ((((RO, Global), lin_mem_addr),
    (add lin_mem_addr (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
    lin_mem_addr)
  in
  let cap_glob = fun n0 -> Inr ((((RW, Global), (start_globals n0)),
    (start_table n0)), (start_globals n0))
  in
  let cap_table = fun n0 -> Inr ((((RO, Global), (start_table n0)),
    (end_datas n0)), (start_table n0))
  in
  let cap_grow = fun n0 -> Inr ((((E, Global), (start_grow_main n0)),
    (start_lin_mem n0)),
    (add (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))) (start_grow_main n0)))
  in
  let cap_grow_write = fun n0 -> Inr ((((RW, Global), (start_grow_main n0)),
    (start_lin_mem n0)), (start_grow_main n0))
  in
  let globals = allocate_mem Big_int_Z.zero_big_int size_globals in
  let table = allocate_mem Big_int_Z.zero_big_int size_table in
  let (allocated_datas, _) =
    foldl (fun acc _ ->
      let (mem_map, n0) = acc in
      ((app mem_map
         (app (grow_lin_mem n0 (cap_grow_write n0)) (app globals table))),
      (add n0 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) ([],
      Big_int_Z.zero_big_int) prog.submodules
  in
  let (updated_caps, _) =
    foldl (fun acc a ->
      let (mem_map, n0) = acc in
      let (a0, _) = a in
      let a_lin_mem = add a0 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
      in
      let a_glob =
        add a0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))
      in
      let a_table =
        add a0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
      in
      let a_malloc =
        add a0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))))
      in
      let a_grow =
        add a0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
      in
      let updated_mem =
        insert0
          (map_insert (gmap_partial_alter Coq_Nat.eq_dec nat_countable))
          a_lin_mem (lin_mem_cap n0)
          (insert0
            (map_insert (gmap_partial_alter Coq_Nat.eq_dec nat_countable))
            a_glob (cap_glob n0)
            (insert0
              (map_insert (gmap_partial_alter Coq_Nat.eq_dec nat_countable))
              a_table (cap_table n0)
              (insert0
                (map_insert (gmap_partial_alter Coq_Nat.eq_dec nat_countable))
                a_malloc cap_safe_mem
                (insert0
                  (map_insert
                    (gmap_partial_alter Coq_Nat.eq_dec nat_countable)) a_grow
                  (cap_grow n0) mem_map))))
      in
      (updated_mem, (add n0 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))
      (prog.segment, Big_int_Z.zero_big_int) prog.submodules
  in
  let boot_code =
    encodeInstrsW h ((StoreU (STK, (Inl Big_int_Z.zero_big_int), (Inr (R
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Lea ((R
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
      Big_int_Z.unit_big_int))) :: ((StoreU (STK, (Inl
      Big_int_Z.zero_big_int), (Inr STK))) :: ((Jmp (R
      Big_int_Z.zero_big_int)) :: []))))
  in
  let len_boot = length boot_code in
  let end_code = encodeInstrsW h (Halt :: []) in
  let grow_code = encodeInstrsW h grow_subroutine_instrs in
  let heap =
    union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
      (list_to_addr_gmap boot_code Big_int_Z.zero_big_int)
      (shift_segment
        (union0
          (map_union
            (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
          (list_to_addr_gmap
            (app grow_code (app safe_mem (app allocated_datas end_code)))
            size_prog) updated_caps) len_boot)
  in
  let len_heap = length (gmap_to_list Coq_Nat.eq_dec nat_countable heap) in
  let main_cap =
    match prog.main with
    | Some w ->
      (match w with
       | Inl z0 -> Inl z0
       | Inr c -> Inr (shift_cap c len_boot))
    | None -> Inl Big_int_Z.zero_big_int
  in
  let end_cap = Inr ((((RX, Global),
    (sub len_heap (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
    len_heap), (sub len_heap (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
  in
  let pc_cap = Inr ((((RX, Global), Big_int_Z.zero_big_int), len_boot),
    Big_int_Z.zero_big_int)
  in
  let stk_cap = Inr ((((URWLX, Directed), start_stack), end_stack),
    start_stack)
  in
  let regfile =
    insert0 reg_insert (R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
      end_cap
      (insert0 reg_insert (R Big_int_Z.zero_big_int) main_cap
        (insert0 reg_insert STK stk_cap
          (insert0 reg_insert PC pc_cap (gmap_empty reg_eq_dec reg_countable))))
  in
  (regfile, heap)

(** val bank_prog : ws_basic_instruction list **)

let bank_prog =
  let account_ptr = Big_int_Z.zero_big_int in
  let account_id = Big_int_Z.succ_big_int Big_int_Z.zero_big_int in
  let account_balance = Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)
  in
  (I_const (Val_int (Big_int_Z.mult_int_big_int 2
  (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
  Big_int_Z.unit_big_int))))) :: (I_segalloc :: ((I_set_local
  account_ptr) :: ((I_get_local account_ptr) :: ((I_const (Val_int
  Big_int_Z.zero_big_int)) :: ((I_const (Val_int
  (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
  Big_int_Z.unit_big_int)))) :: (I_slice :: ((I_set_local
  account_id) :: ((I_get_local account_ptr) :: ((I_const (Val_int
  (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
  Big_int_Z.unit_big_int)))) :: ((I_const (Val_int
  Big_int_Z.zero_big_int)) :: (I_slice :: ((I_set_local
  account_balance) :: ((I_const (Val_int (Big_int_Z.mult_int_big_int 2
  (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))) :: ((I_get_local
  account_balance) :: (I_handleadd :: ((I_set_local
  account_balance) :: ((I_get_local account_balance) :: ((I_const (Val_int
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  Big_int_Z.unit_big_int))))) :: ((I_segstore T_int) :: ((I_get_local
  account_id) :: ((I_call Big_int_Z.zero_big_int) :: (I_drop :: ((I_get_local
  account_balance) :: ((I_segload T_int) :: []))))))))))))))))))))))))

(** val bank_tf : function_type **)

let bank_tf =
  Tf ([], (T_int :: []))

(** val adv_tf : function_type **)

let adv_tf =
  Tf ((T_handle :: []), (T_int :: []))

(** val bank_module : ws_module **)

let bank_module =
  { mod_types = (bank_tf :: (adv_tf :: [])); mod_funcs = ({ modfunc_type =
    Big_int_Z.zero_big_int; modfunc_locals = []; modfunc_body =
    bank_prog } :: []); mod_tables = []; mod_start = (Some
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)); mod_imports =
    ({ imp_module = ('E'::('n'::('v'::[]))); imp_name =
    ('a'::('d'::('v'::[]))); imp_desc = (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int) } :: []); mod_exports = [] }

(** val env_adv_prog : ws_basic_instruction list **)

let env_adv_prog =
  (I_get_local Big_int_Z.zero_big_int) :: (I_drop :: ((I_const (Val_int
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2
    Big_int_Z.unit_big_int))))) :: (I_return :: [])))

(** val env_module : ws_module **)

let env_module =
  { mod_types = (adv_tf :: []); mod_funcs = ({ modfunc_type =
    Big_int_Z.zero_big_int; modfunc_locals = []; modfunc_body =
    env_adv_prog } :: []); mod_tables = []; mod_start = None; mod_imports =
    []; mod_exports = ({ modexp_name = ('a'::('d'::('v'::[]))); modexp_desc =
    Big_int_Z.zero_big_int } :: []) }

(** val new_stack : ws_basic_instruction list **)

let new_stack =
  (I_const (Val_int
    Big_int_Z.unit_big_int)) :: (I_grow_memory :: ((I_tee_local
    Big_int_Z.zero_big_int) :: ((I_const (Val_int (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int))) :: ((I_relop (T_int, ROI_eq)) :: ((I_if
    ((T_int :: []), ((I_const (Val_int (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int))) :: []), ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_const (Val_int page_size)) :: ((I_binop
    (T_int, BOI_mul)) :: ((I_tee_local
    Big_int_Z.zero_big_int) :: ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_store T_int) :: ((I_get_local
    Big_int_Z.zero_big_int) :: []))))))))) :: [])))))

(** val validate_stack : immediate -> ws_basic_instruction list **)

let validate_stack x =
  (I_get_local x) :: ((I_const (Val_int page_size)) :: ((I_binop (T_int,
    (BOI_rem SX_S))) :: ((I_if ([], (I_unreachable :: []), [])) :: [])))

(** val validate_stack_bound : immediate -> ws_basic_instruction list **)

let validate_stack_bound x =
  (I_get_local x) :: ((I_load T_int) :: (I_drop :: []))

(** val is_full_op : ws_basic_instruction list **)

let is_full_op =
  (I_const (Val_int Big_int_Z.unit_big_int)) :: ((I_const (Val_int
    Big_int_Z.zero_big_int)) :: ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_load T_int) :: ((I_const (Val_int
    page_size)) :: ((I_binop (T_int, (BOI_rem SX_S))) :: ((I_const (Val_int
    (Z.sub page_size (Big_int_Z.mult_int_big_int 2
      (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))) :: ((I_relop
    (T_int, ROI_eq)) :: (I_select :: []))))))))

(** val is_full : ws_basic_instruction list **)

let is_full =
  app (validate_stack Big_int_Z.zero_big_int)
    (app (validate_stack_bound Big_int_Z.zero_big_int) is_full_op)

(** val is_empty_op : ws_basic_instruction list **)

let is_empty_op =
  (I_get_local Big_int_Z.zero_big_int) :: ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_load T_int) :: ((I_relop (T_int,
    ROI_eq)) :: [])))

(** val is_empty : ws_basic_instruction list **)

let is_empty =
  app (validate_stack Big_int_Z.zero_big_int)
    (app (validate_stack_bound Big_int_Z.zero_big_int) is_empty_op)

(** val push_op : ws_basic_instruction list **)

let push_op =
  app is_full_op ((I_if ([], (I_unreachable :: []), [])) :: ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_load T_int) :: ((I_const (Val_int
    (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    Big_int_Z.unit_big_int)))) :: ((I_binop (T_int,
    BOI_add)) :: ((I_tee_local (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) :: ((I_get_local
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((I_store
    T_int) :: ((I_get_local Big_int_Z.zero_big_int) :: ((I_get_local
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) :: ((I_store T_int) :: [])))))))))))

(** val push : ws_basic_instruction list **)

let push =
  app (validate_stack Big_int_Z.zero_big_int)
    (app (validate_stack_bound Big_int_Z.zero_big_int) push_op)

(** val pop_op : ws_basic_instruction list **)

let pop_op =
  app is_empty_op ((I_if ([], (I_unreachable :: []), [])) :: ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_load T_int) :: ((I_tee_local
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((I_load
    T_int) :: ((I_get_local Big_int_Z.zero_big_int) :: ((I_get_local
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((I_const (Val_int
    (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    Big_int_Z.unit_big_int)))) :: ((I_binop (T_int, BOI_sub)) :: ((I_store
    T_int) :: []))))))))))

(** val pop : ws_basic_instruction list **)

let pop =
  app (validate_stack Big_int_Z.zero_big_int)
    (app (validate_stack_bound Big_int_Z.zero_big_int) pop_op)

(** val stack_length_op : ws_basic_instruction list **)

let stack_length_op =
  (I_get_local Big_int_Z.zero_big_int) :: ((I_load T_int) :: ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_binop (T_int, BOI_sub)) :: ((I_const
    (Val_int (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    Big_int_Z.unit_big_int)))) :: ((I_binop (T_int, (BOI_div
    SX_U))) :: [])))))

(** val stack_length : ws_basic_instruction list **)

let stack_length =
  app (validate_stack Big_int_Z.zero_big_int)
    (app (validate_stack_bound Big_int_Z.zero_big_int) stack_length_op)

(** val map_initialise : ws_basic_instruction list **)

let map_initialise =
  (I_get_local Big_int_Z.zero_big_int) :: ((I_load T_int) :: ((I_set_local
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) :: ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_set_local (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) :: []))))

(** val map_loop_body : ws_basic_instruction list **)

let map_loop_body =
  (I_get_local (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) :: ((I_get_local (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) :: ((I_relop (T_int, ROI_eq)) :: ((I_br_if
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((I_get_local
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) :: ((I_const (Val_int
    (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    Big_int_Z.unit_big_int)))) :: ((I_binop (T_int,
    BOI_add)) :: ((I_tee_local (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) :: ((I_get_local
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) :: ((I_load T_int) :: ((I_get_local
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((I_call_indirect
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((I_store
    T_int) :: ((I_br Big_int_Z.zero_big_int) :: [])))))))))))))

(** val map_op : ws_basic_instruction list **)

let map_op =
  app map_initialise ((I_block ([], ((I_loop ([],
    map_loop_body)) :: []))) :: [])

(** val stack_map : ws_basic_instruction list **)

let stack_map =
  app (validate_stack Big_int_Z.zero_big_int)
    (app (validate_stack_bound Big_int_Z.zero_big_int) map_op)

(** val main_stack : ws_basic_instruction list **)

let main_stack =
  (I_call Big_int_Z.zero_big_int) :: ((I_tee_local
    Big_int_Z.zero_big_int) :: ((I_const (Val_int (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int))) :: ((I_relop (T_int, ROI_eq)) :: ((I_if ([],
    ((I_const (Val_int (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int))) :: ((I_set_global
    Big_int_Z.zero_big_int) :: (I_return :: []))), [])) :: ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_const (Val_int
    (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    Big_int_Z.unit_big_int)))) :: ((I_call (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_const (Val_int
    (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int)))) :: ((I_call (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_const (Val_int
    Big_int_Z.zero_big_int)) :: ((I_call (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_call (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) :: ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_call (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) :: ((I_binop (T_int,
    BOI_sub)) :: ((I_set_global
    Big_int_Z.zero_big_int) :: [])))))))))))))))))))

(** val square : ws_basic_instruction list **)

let square =
  (I_get_local Big_int_Z.zero_big_int) :: ((I_get_local
    Big_int_Z.zero_big_int) :: ((I_binop (T_int, BOI_mul)) :: []))

(** val stack_module : ws_module **)

let stack_module =
  { mod_types = ((Tf ([], (T_int :: []))) :: ((Tf ((T_int :: []),
    (T_int :: []))) :: ((Tf ((T_int :: (T_int :: [])), [])) :: [])));
    mod_funcs = ({ modfunc_type = Big_int_Z.zero_big_int; modfunc_locals =
    (T_int :: []); modfunc_body = new_stack } :: ({ modfunc_type =
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int); modfunc_locals = [];
    modfunc_body = is_empty } :: ({ modfunc_type = (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int); modfunc_locals = []; modfunc_body =
    is_full } :: ({ modfunc_type = (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int); modfunc_locals = (T_int :: []); modfunc_body =
    pop } :: ({ modfunc_type = (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)); modfunc_locals =
    (T_int :: []); modfunc_body = push } :: ({ modfunc_type =
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int));
    modfunc_locals = (T_int :: (T_int :: [])); modfunc_body =
    stack_map } :: ({ modfunc_type = (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int); modfunc_locals = []; modfunc_body =
    stack_length } :: []))))))); mod_tables = ({ lim_min =
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int); lim_max = None } :: []);
    mod_start = None; mod_imports = []; mod_exports = ({ modexp_name =
    ('n'::('e'::('w'::('_'::('s'::('t'::('a'::('c'::('k'::[])))))))));
    modexp_desc = Big_int_Z.zero_big_int } :: ({ modexp_name =
    ('i'::('s'::('_'::('e'::('m'::('p'::('t'::('y'::[])))))))); modexp_desc =
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) } :: ({ modexp_name =
    ('i'::('s'::('_'::('f'::('u'::('l'::('l'::[]))))))); modexp_desc =
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)) } :: ({ modexp_name = ('p'::('o'::('p'::[])));
    modexp_desc = (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) } :: ({ modexp_name =
    ('p'::('u'::('s'::('h'::[])))); modexp_desc = (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) } :: ({ modexp_name =
    ('s'::('t'::('a'::('c'::('k'::('_'::('m'::('a'::('p'::[])))))))));
    modexp_desc = (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) } :: ({ modexp_name =
    ('s'::('t'::('a'::('c'::('k'::('_'::('l'::('e'::('n'::('g'::('t'::('h'::[]))))))))))));
    modexp_desc = (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) } :: []))))))) }

(** val client_module : ws_module **)

let client_module =
  { mod_types = ((Tf ([], [])) :: ((Tf ([], (T_int :: []))) :: ((Tf
    ((T_int :: []), (T_int :: []))) :: ((Tf ((T_int :: (T_int :: [])),
    [])) :: [])))); mod_funcs = ({ modfunc_type = Big_int_Z.zero_big_int;
    modfunc_locals = (T_int :: []); modfunc_body =
    main_stack } :: ({ modfunc_type = (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)); modfunc_locals = [];
    modfunc_body = square } :: [])); mod_tables = []; mod_start = (Some
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))); mod_imports =
    ({ imp_module = ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('n'::('e'::('w'::('_'::('s'::('t'::('a'::('c'::('k'::[])))))))));
    imp_desc = (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int) } :: ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('i'::('s'::('_'::('e'::('m'::('p'::('t'::('y'::[])))))))); imp_desc =
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)) } :: ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('i'::('s'::('_'::('f'::('u'::('l'::('l'::[]))))))); imp_desc =
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)) } :: ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('p'::('o'::('p'::[]))); imp_desc = (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) } :: ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('p'::('u'::('s'::('h'::[])))); imp_desc = (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) } :: ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('s'::('t'::('a'::('c'::('k'::('_'::('m'::('a'::('p'::[])))))))));
    imp_desc = (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) } :: ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('s'::('t'::('a'::('c'::('k'::('_'::('l'::('e'::('n'::('g'::('t'::('h'::[]))))))))))));
    imp_desc = (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)) } :: []))))))); mod_exports = [] }

(** val bank_example :
    ((ws_module * typeidx) * value_type list) * ws_basic_instruction list **)

let bank_example =
  (((bank_module, (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), []),
    bank_prog)

(** val adv_example :
    ((ws_module * typeidx) * value_type list) * ws_basic_instruction list **)

let adv_example =
  (((env_module, Big_int_Z.zero_big_int), []), env_adv_prog)

(** val empty_component : cerise_component **)

let empty_component =
  { segment = (gmap_empty Coq_Nat.eq_dec nat_countable); imports =
    (gmap_empty Coq_Nat.eq_dec nat_countable); exports =
    (gmap_empty string_eq_dec string_countable); submodules = []; main =
    None }

(** val link_multiple :
    cerise_component option list -> cerise_component option **)

let link_multiple components =
  foldl (fun acc c ->
    match acc with
    | Some p -> (match c with
                 | Some p' -> link p p'
                 | None -> None)
    | None -> None) (Some empty_component) components

(** val compile :
    machineParameters -> lMachineParameters -> ws_module -> name ->
    cerise_component option **)

let compile mP lMP m n0 =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun labeled ->
    compile_component mP lMP labeled) (Obj.magic compile_module mP lMP m n0)

(** val compile_list :
    machineParameters -> lMachineParameters -> (ws_module * name) list ->
    cerise_component option list **)

let compile_list mP lMP ml =
  map (fun m -> compile mP lMP (fst m) (snd m)) ml

(** val load_test :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> cerise_component option list -> (regName * word)
    list * (addr * word) list **)

let load_test mP size_safe_mem size_lin_mem size_globals size_table start_stack end_stack components =
  match link_multiple components with
  | Some p ->
    let (regs, mem0) =
      load mP p size_safe_mem size_lin_mem size_globals size_table
        start_stack end_stack
    in
    ((gmap_to_list reg_eq_dec reg_countable regs),
    (sort (gmap_to_list Coq_Nat.eq_dec nat_countable mem0)))
  | None -> ([], [])

(** val loaded_bank_example :
    machineParameters -> lMachineParameters -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> (regName * word) list * (addr * word) list **)

let loaded_bank_example mP lMP start_stack end_stack =
  load_test mP (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))))))))))))))))))))))))
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int) start_stack end_stack
    (compile_list mP lMP ((bank_module,
      ('B'::('a'::('n'::('k'::[]))))) :: ((env_module,
      ('E'::('n'::('v'::[])))) :: [])))

(** val loaded_stack_example :
    machineParameters -> lMachineParameters -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> (regName * word) list * (addr * word) list **)

let loaded_stack_example mP lMP start_stack end_stack =
  load_test mP (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))))))))))))))))))))))))
    (mul (Z.to_nat page_size) (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))) (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) start_stack end_stack
    (compile_list mP lMP ((stack_module,
      ('S'::('t'::('a'::('c'::('k'::[])))))) :: ((client_module,
      ('C'::('l'::('i'::('e'::('n'::('t'::[]))))))) :: [])))
