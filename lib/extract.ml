
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

(** val min : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

let rec min = Big_int_Z.min_big_int

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

(** val concat : 'a1 list list -> 'a1 list **)

let rec concat = function
| [] -> []
| x :: l0 -> app x (concat l0)

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

  (** val of_nat : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let of_nat n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun n1 -> (Pos.of_succ_nat n1))
      n0
 end

(** val zero : char **)

let zero = '\000'

(** val shift : bool -> char -> char **)

let shift = fun b c -> Char.chr (((Char.code c) lsl 1) land 255 + if b then 1 else 0)



(** val eqb : char list -> char list -> bool **)

let rec eqb s1 s2 =
  match s1 with
  | [] -> (match s2 with
           | [] -> true
           | _::_ -> false)
  | c1::s1' ->
    (match s2 with
     | [] -> false
     | c2::s2' -> if (=) c1 c2 then eqb s1' s2' else false)

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

type ('k, 'a, 'm) singletonM = 'k -> 'a -> 'm

(** val singletonM0 : ('a1, 'a2, 'a3) singletonM -> 'a1 -> 'a2 -> 'a3 **)

let singletonM0 singletonM1 =
  singletonM1

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

(** val from_option : ('a1 -> 'a2) -> 'a2 -> 'a1 option -> 'a2 **)

let from_option f y = function
| Some x -> f x
| None -> y

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

(** val map_singleton :
    ('a1, 'a2, 'a3) partialAlter -> 'a3 empty -> ('a1, 'a2, 'a3) singletonM **)

let map_singleton h h0 i x =
  insert0 (map_insert h) i x (empty0 h0)

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

type symbols = char list

(** val symbols_encode : char list -> char list -> symbols **)

let symbols_encode mod_name imp_name0 =
  append (append mod_name ('.'::[])) imp_name0

(** val symbols_append : symbols -> char list -> symbols **)

let symbols_append =
  append

type cerise_program = cerise_instruction list

type data = word list

type id0 = char list

type frame_entry =
| Imports of symbols
| Define of id0 * Big_int_Z.big_int
| SafeMem

type cerise_frame = { cframe_functions_imports : frame_entry list;
                      cframe_functions_defined : frame_entry list;
                      cframe_linear_memory : frame_entry list;
                      cframe_globals_imports : frame_entry list;
                      cframe_globals_defined : frame_entry list;
                      cframe_indirect_table : frame_entry list;
                      cframe_safe_mem : frame_entry list }

type cerise_pre_component = { c_functions : (id0, cerise_program list) gmap;
                              c_frame : (id0, cerise_frame) gmap;
                              c_lin_mem : (id0, data) gmap;
                              c_globals : (id0, data list) gmap;
                              c_indirect_table : (id0, data) gmap;
                              c_exports : (symbols, id0 * Big_int_Z.big_int)
                                          gmap;
                              c_main : (id0 * Big_int_Z.big_int) option }

type cerise_component = { segment : (addr, word) gmap;
                          imports : (addr, symbols) gmap;
                          exports : (symbols, word) gmap; main : word option }

(** val list_to_mem : 'a1 list -> addr -> (addr, 'a1) gmap **)

let rec list_to_mem l a =
  match l with
  | [] -> gmap_empty Coq_Nat.eq_dec nat_countable
  | h :: t ->
    insert0 (map_insert (gmap_partial_alter Coq_Nat.eq_dec nat_countable)) a
      h
      (list_to_mem t (add a (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))

(** val insert1 : (addr * 'a1) -> (addr * 'a1) list -> (addr * 'a1) list **)

let rec insert1 x l = match l with
| [] -> x :: []
| h :: t -> if Nat.ltb (fst h) (fst x) then h :: (insert1 x t) else x :: l

(** val sort : (addr * 'a1) list -> (addr * 'a1) list **)

let rec sort = function
| [] -> []
| h :: t -> insert1 h (sort t)

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

(** val shift_word : word -> Big_int_Z.big_int -> word **)

let shift_word w n0 =
  match w with
  | Inl z0 -> Inl z0
  | Inr c -> Inr (shift_cap c n0)

(** val shift_data : data -> Big_int_Z.big_int -> data **)

let shift_data d n0 =
  map (fun w -> shift_word w n0) d

type immediate = Big_int_Z.big_int

type handle = { base : Big_int_Z.big_int; offset : Big_int_Z.big_int;
                bound : Big_int_Z.big_int; valid : bool;
                id1 : Big_int_Z.big_int }

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

type memidx =
  Big_int_Z.big_int
  (* singleton inductive, whose constructor was Mk_memidx *)

type globalidx =
  Big_int_Z.big_int
  (* singleton inductive, whose constructor was Mk_globalidx *)

type tableidx =
  Big_int_Z.big_int
  (* singleton inductive, whose constructor was Mk_tableidx *)

type name = char list

type limits = { lim_min : Big_int_Z.big_int;
                lim_max : Big_int_Z.big_int option }

type mutability =
| MUT_immut
| MUT_mut

type global_type = { tg_mut : mutability; tg_t : value_type }

type table_type =
  limits
  (* singleton inductive, whose constructor was Build_table_type *)

(** val tt_limits : table_type -> limits **)

let tt_limits t =
  t

type memory_type = limits

type import_desc =
| ID_func of typeidx
| ID_table of table_type
| ID_mem of memory_type
| ID_global of global_type

type module_import = { imp_module : name; imp_name : name;
                       imp_desc : import_desc }

type module_export_desc =
| MED_func of funcidx
| MED_table of tableidx
| MED_mem of memidx
| MED_global of globalidx

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

type module_table =
  table_type
  (* singleton inductive, whose constructor was Build_module_table *)

(** val modtab_type : module_table -> table_type **)

let modtab_type m =
  m

type module_glob = { modglob_type : global_type; modglob_init : value }

type ws_module = { mod_types : function_type list;
                   mod_funcs : module_func list;
                   mod_tables : module_table list;
                   mod_mems : memory_type list;
                   mod_globals : module_glob list;
                   mod_start : module_start option;
                   mod_imports : module_import list;
                   mod_exports : module_export list }

(** val get_type : ws_module -> typeidx -> function_type option **)

let get_type module0 i =
  nth_error module0.mod_types i

(** val get_functions : module_import list -> typeidx list **)

let rec get_functions = function
| [] -> []
| i :: imports' ->
  (match i.imp_desc with
   | ID_func tidx -> tidx :: (get_functions imports')
   | _ -> get_functions imports')

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

type labeled_program = labeled_instr list

type labeled_cerise_component = { l_functions : (id0, labeled_program list)
                                                gmap;
                                  l_frame : (id0, cerise_frame) gmap;
                                  l_lin_mem : (id0, data) gmap;
                                  l_globals : (id0, data list) gmap;
                                  l_indirect_table : (id0, data) gmap;
                                  l_exports : (symbols,
                                              id0 * Big_int_Z.big_int) gmap;
                                  l_main : (id0 * Big_int_Z.big_int) option }

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

(** val encodeInstrW : machineParameters -> cerise_instruction -> word **)

let encodeInstrW h i =
  Inl (h.encodeInstr i)

(** val encodeInstrsW :
    machineParameters -> cerise_instruction list -> word list **)

let encodeInstrsW h =
  map (encodeInstrW h)

(** val r_stk : regName **)

let r_stk =
  STK

(** val page_size0 : Big_int_Z.big_int **)

let page_size0 =
  Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
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
    Big_int_Z.zero_big_int)))))))))))))))))))))))))))))))

(** val z_page_size : Big_int_Z.big_int **)

let z_page_size =
  Z.of_nat page_size0

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
    regName -> Big_int_Z.big_int -> cerise_instruction list **)

let reqloc_instrs r0 z0 =
  let r1 = R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
  let r2 = R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))
  in
  (IsPtr (r1, r0)) :: ((Sub (r1, (Inr r1), (Inl
  Big_int_Z.unit_big_int))) :: ((Mov (r2, (Inr PC))) :: ((Lea (r2, (Inl
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))))) :: ((Jnz (r2,
  r1)) :: ((GetL (r1, r0)) :: ((Sub (r1, (Inr r1), (Inl z0))) :: ((Mov (r2,
  (Inr PC))) :: ((Lea (r2, (Inl (Big_int_Z.mult_int_big_int 2
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  Big_int_Z.unit_big_int))))) :: ((Jnz (r2, r1)) :: ((Mov (r2, (Inr
  PC))) :: ((Lea (r2, (Inl (Big_int_Z.mult_int_big_int 2
  (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))) :: ((Jmp
  r2) :: (Fail :: ((Mov (r1, (Inl Big_int_Z.zero_big_int))) :: ((Mov (r2,
  (Inl Big_int_Z.zero_big_int))) :: [])))))))))))))))

(** val dyn_typecheck_instrs :
    machineParameters -> regName -> value_type -> cerise_instruction list **)

let dyn_typecheck_instrs h r0 vtype =
  let tmp_reg = R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
  let tmp_jmp = R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))
  in
  (match vtype with
   | T_int ->
     (IsPtr (tmp_reg, r0)) :: ((Mov (tmp_jmp, (Inr PC))) :: ((Lea (tmp_jmp,
       (Inl
       ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
       ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
       Big_int_Z.unit_big_int))))) :: ((Jnz (tmp_jmp, tmp_reg)) :: ((Mov
       (tmp_jmp, (Inr PC))) :: ((Lea (tmp_jmp, (Inl
       (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
       Big_int_Z.unit_big_int))))) :: ((Jmp tmp_jmp) :: (Fail :: [])))))))
   | T_handle ->
     app ((IsPtr (tmp_reg, r0)) :: ((Mov (tmp_jmp, (Inr PC))) :: ((Lea
       (tmp_jmp, (Inl (Big_int_Z.mult_int_big_int 2
       (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))) :: ((Jnz
       (tmp_jmp, tmp_reg)) :: (Fail :: [])))))
       (reqloc_instrs r0 (h.encodeLoc Global)))

(** val len_dyn_typecheck :
    machineParameters -> value_type -> Big_int_Z.big_int **)

let len_dyn_typecheck h _ =
  Z.of_nat (length (dyn_typecheck_instrs h PC T_int))

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

let malloc_instrs r_malloc r_size tmp_r0 tmp_r1 tmp_r2 tmp_r3 tmp_r4 tmp_res =
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
      Big_int_Z.unit_big_int)))) :: ((Jmp r_malloc) :: ((Mov ((R tmp_res),
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

(** val entry_point_load_lin_mem : Big_int_Z.big_int **)

let entry_point_load_lin_mem =
  Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)

(** val gated_load_lin_mem : cerise_instruction list **)

let gated_load_lin_mem =
  let offset_mem_cap =
    Z.sub Big_int_Z.unit_big_int (Z.of_nat entry_point_load_lin_mem)
  in
  (Load ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))), PC)) :: ((Lea ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  offset_mem_cap))) :: ((Load ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))) :: ((Lea ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Load ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  Big_int_Z.zero_big_int))) :: ((Jmp (R Big_int_Z.zero_big_int)) :: []))))))

(** val entry_point_store_lin_mem : Big_int_Z.big_int **)

let entry_point_store_lin_mem =
  add entry_point_load_lin_mem (length gated_load_lin_mem)

(** val gated_store_lin_mem : machineParameters -> cerise_instruction list **)

let gated_store_lin_mem h =
  let offset_mem_cap =
    Z.sub Big_int_Z.unit_big_int
      (Z.add
        (Z.add (Z.of_nat entry_point_store_lin_mem)
          (len_dyn_typecheck h T_int)) (Big_int_Z.mult_int_big_int 2
        Big_int_Z.unit_big_int))
  in
  app ((Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
    (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))) :: [])
    (app
      (dyn_typecheck_instrs h (R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) T_int) ((Mov ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))),
      (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((Load ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))), PC)) :: ((Lea ((R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))), (Inl offset_mem_cap))) :: ((Load ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))))) :: ((Lea ((R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))), (Inr (R (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))) :: ((Store ((R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))), (Inr (R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((Mov ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))), (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inl
      Big_int_Z.zero_big_int))) :: ((Jmp (R
      Big_int_Z.zero_big_int)) :: []))))))))))

(** val entry_point_grow_lin_mem : machineParameters -> Big_int_Z.big_int **)

let entry_point_grow_lin_mem h =
  add entry_point_store_lin_mem (length (gated_store_lin_mem h))

(** val gated_grow_lin_mem : machineParameters -> cerise_instruction list **)

let gated_grow_lin_mem h =
  let offset_full_mem_cap = Z.of_nat (entry_point_grow_lin_mem h) in
  (Load ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))), PC)) :: ((Lea ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  offset_full_mem_cap))) :: ((GetE ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))) :: ((GetA ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Mul ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  z_page_size))) :: ((Add ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))), (Inr (R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inr PC))) :: ((Lea
  ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))), (Inl (Big_int_Z.mult_int_big_int 2
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  Big_int_Z.unit_big_int))))) :: ((Lt ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (Inr (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
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
  Big_int_Z.zero_big_int))))))), (Inl z_page_size))) :: ((Lea ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
  (Inr (R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((GetA ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((Subseg ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inr (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))) :: ((Sub ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (Inr (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inr (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Lea ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inr (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))) :: ((Load ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  PC)) :: ((GetB ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))) :: ((GetA ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))) :: ((Sub ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
  (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
  (r (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Add ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
  (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
  (Inl Big_int_Z.unit_big_int))) :: ((Lea ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Store
  ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))),
  (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: ((Mov ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inr PC))) :: ((Lea
  ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))), (Inl (Big_int_Z.mult_int_big_int 2
  (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))) :: ((Jmp (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (Inl (Big_int_Z.minus_big_int
  Big_int_Z.unit_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  Big_int_Z.zero_big_int))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))), (Inl Big_int_Z.zero_big_int))) :: ((Jmp (R
  Big_int_Z.zero_big_int)) :: []))))))))))))))))))))))))))))))))))))))

(** val entry_point_current_lin_mem :
    machineParameters -> Big_int_Z.big_int **)

let entry_point_current_lin_mem h =
  add (entry_point_grow_lin_mem h) (length (gated_grow_lin_mem h))

(** val gated_current_lin_mem :
    machineParameters -> cerise_instruction list **)

let gated_current_lin_mem h =
  let offset_mem_cap =
    Z.sub Big_int_Z.unit_big_int (Z.of_nat (entry_point_current_lin_mem h))
  in
  (Load ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), PC)) :: ((Lea
  ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
  offset_mem_cap))) :: ((Load ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))) :: ((GetB ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((GetA ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))) :: ((Sub ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((Div ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  z_page_size))) :: ((Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))), (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))), (Inl Big_int_Z.zero_big_int))) :: ((Jmp (R
  Big_int_Z.zero_big_int)) :: [])))))))))

(** val entry_point_load_global : Big_int_Z.big_int **)

let entry_point_load_global =
  Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)

(** val gated_load_global : cerise_instruction list **)

let gated_load_global =
  let offset_value_cap = Z.opp (Z.of_nat entry_point_load_global) in
  (Mov ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr
  PC))) :: ((Lea ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
  offset_value_cap))) :: ((Load ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))) :: []))

(** val entry_point_store_global : Big_int_Z.big_int **)

let entry_point_store_global =
  add entry_point_load_global (length gated_load_global)

(** val gated_store_global :
    machineParameters -> value_type -> cerise_instruction list **)

let gated_store_global h vtype =
  let offset_value_cap =
    Z.opp
      (Z.add
        (Z.add (Z.of_nat entry_point_store_global)
          (len_dyn_typecheck h vtype)) (Big_int_Z.mult_int_big_int 2
        Big_int_Z.unit_big_int))
  in
  app ((Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (Inr (R
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: [])
    (app
      (dyn_typecheck_instrs h (R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))) vtype) ((Mov ((R (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)), (Inr (R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr PC))) :: ((Lea
      ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))), (Inl offset_value_cap))) :: ((Store ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))),
      (r (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))), (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))), (Inl Big_int_Z.zero_big_int))) :: [])))))))

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
  let new_scope = { scope_name =
    (add p.scope_children (Big_int_Z.succ_big_int Big_int_Z.zero_big_int));
    scope_ret_type = rt; scope_children = Big_int_Z.zero_big_int;
    scope_reg_base = reg_base }
  in
  new_scope :: ((new_child p) :: s')

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

(** val r_frame : regName **)

let r_frame =
  R Big_int_Z.zero_big_int

(** val base_reg : Big_int_Z.big_int **)

let base_reg =
  Big_int_Z.succ_big_int Big_int_Z.zero_big_int

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

(** val call_template :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    labeled_instr list -> regName list -> result_type -> result_type ->
    compilation_state -> (labeled_instr list * compilation_state) option **)

let call_template h nreg res prologue args _ ret_type new_state0 =
  let tmp = fun i ->
    add
      (add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))) i
  in
  let call =
    instrs
      (call_instrs h (R
        (add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) args (R
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
            (Obj.magic instrs
              (app ((LoadU ((R res), r_stk0, (Inl (Big_int_Z.minus_big_int
                Big_int_Z.unit_big_int)))) :: ((Lea (r_stk0, (Inl
                (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: []))
                (app ((Mov ((R
                  (tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
                  (r (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov
                  ((R
                  (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                    Big_int_Z.zero_big_int)))),
                  (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                    Big_int_Z.zero_big_int))))) :: []))
                  (app (dyn_typecheck_instrs h (R res) ret_type0) ((Mov ((R
                    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
                    (r (tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Mov
                    ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                    Big_int_Z.zero_big_int))),
                    (r
                      (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                        Big_int_Z.zero_big_int)))))) :: []))))))
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

type frame = { idx_imports_functions : Big_int_Z.big_int;
               idx_defined_functions : Big_int_Z.big_int;
               idx_linear_memory : Big_int_Z.big_int;
               idx_imports_globals : Big_int_Z.big_int;
               idx_defined_globals : Big_int_Z.big_int;
               idx_indirect_table : Big_int_Z.big_int;
               idx_safe_mem : Big_int_Z.big_int }

(** val compile_basic_instr :
    machineParameters -> ws_module -> typeidx -> value_type list ->
    ws_basic_instruction -> compilation_state -> frame -> cfg option **)

let rec compile_basic_instr h module0 f_typeidx f_locals i s module_frame =
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
               (compile_basic_instr h module0 f_typeidx f_locals i0 state_acc
                 module_frame)) acc) (Some ([], loop_state)) body)
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
               (compile_basic_instr h module0 f_typeidx f_locals i0 state_acc
                 module_frame)) acc) (Some ([], loop_state)) body)
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
                   state_acc module_frame)) acc) (Some ([], bodyT_state))
             body_true))
         (foldl (fun acc i0 ->
           mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
             let (instrs_acc, state_acc) = pat in
             mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat0 ->
               let (instrs_comp, state_comp) = pat0 in
               Some ((app instrs_acc instrs_comp), state_comp))
               (compile_basic_instr h module0 f_typeidx f_locals i0 state_acc
                 module_frame)) acc) (Some ([], bodyF_state)) body_false)
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
         let tmp_fun =
           add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
         in
         let res = sub nreg len_arg_type in
         let args' = seq (sub nreg len_arg_type) len_arg_type in
         let args = map (fun x -> R x) args' in
         let offset_function = add module_frame.idx_imports_functions i0 in
         let prologue =
           instrs ((Mov ((R tmp_fun), (Inr r_frame))) :: ((Lea ((R tmp_fun),
             (Inl (Z.of_nat offset_function)))) :: ((Load ((R tmp_fun), (R
             tmp_fun))) :: [])))
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
         let tmp_fun =
           add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
         in
         let args' = seq (sub nfun len_arg_type) len_arg_type in
         let args = map (fun x -> R x) args' in
         let prologue =
           instrs ((Mov ((R tmp_fun), (Inr r_frame))) :: ((Lea ((R tmp_fun),
             (Inl (Z.of_nat module_frame.idx_indirect_table)))) :: ((Load ((R
             tmp_fun), (R tmp_fun))) :: ((Lea ((R tmp_fun),
             (r nfun))) :: ((Load ((R tmp_fun), (R tmp_fun))) :: [])))))
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
       let tmp_sentry =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))
       in
       let tmp0 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
       in
       let tmp1 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))))
       in
       let offset_load_global =
         add module_frame.idx_defined_globals
           (mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
             Big_int_Z.zero_big_int)) i0)
       in
       Some
       ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
          tmp_sentry), (Inl (Z.of_nat offset_load_global)))) :: ((Load ((R
          tmp_sentry), (R tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
          Big_int_Z.zero_big_int)))) :: ((Mov ((R tmp1), (Inr (R
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Mov ((R
          Big_int_Z.zero_big_int), (Inr PC))) :: ((Lea (PC, (Inl
          (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))) :: ((Jmp
          (R tmp_sentry)) :: ((Mov ((R nreg), (Inr (R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int),
          (Inr (R tmp0)))) :: ((Mov ((R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)), (Inr (R tmp1)))) :: [])))))))))))),
       (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | I_set_global i0 ->
       let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let tmp_sentry =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
       in
       let tmp0 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))))
       in
       let tmp1 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
       in
       let offset_store_global =
         add
           (add module_frame.idx_defined_globals
             (mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
               Big_int_Z.zero_big_int)) i0)) (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int)
       in
       Some
       ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
          tmp_sentry), (Inl (Z.of_nat offset_store_global)))) :: ((Load ((R
          tmp_sentry), (R tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
          Big_int_Z.zero_big_int)))) :: ((Mov ((R tmp1), (Inr (R
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Mov ((R
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr (R
          v)))) :: ((Mov ((R Big_int_Z.zero_big_int), (Inr PC))) :: ((Lea
          (PC, (Inl (Big_int_Z.mult_int_big_int 2
          Big_int_Z.unit_big_int)))) :: ((Jmp (R tmp_sentry)) :: ((Mov ((R
          Big_int_Z.zero_big_int), (Inr (R tmp0)))) :: ((Mov ((R
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr (R
          tmp1)))) :: [])))))))))))),
       (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | I_load _ ->
       let a = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let tmp_sentry =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
       in
       let tmp0 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))))
       in
       let tmp1 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
       in
       let tmp2 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))))))
       in
       let offset_load_lin_mem =
         add module_frame.idx_linear_memory Big_int_Z.zero_big_int
       in
       Some
       ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
          tmp_sentry), (Inl (Z.of_nat offset_load_lin_mem)))) :: ((Load ((R
          tmp_sentry), (R tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
          Big_int_Z.zero_big_int)))) :: ((Mov ((R tmp1), (Inr (R
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Mov ((R
          tmp2), (Inr (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))) :: ((Mov ((R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)), (Inr (R a)))) :: ((Mov ((R
          Big_int_Z.zero_big_int), (Inr PC))) :: ((Lea (PC, (Inl
          (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))) :: ((Jmp
          (R tmp_sentry)) :: ((Mov ((R res), (Inr (R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int),
          (Inr (R tmp0)))) :: ((Mov ((R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)), (Inr (R tmp1)))) :: ((Mov ((R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))), (Inr (R tmp2)))) :: []))))))))))))))),
       (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | I_store _ ->
       let a =
         sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))
       in
       let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let tmp_sentry =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
       in
       let tmp0 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))))))
       in
       let tmp1 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))
       in
       let tmp2 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))))))))
       in
       let tmp3 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))))
       in
       let tmp4 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))))))))))
       in
       let offset_store_lin_mem =
         add module_frame.idx_linear_memory (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int)
       in
       Some
       ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
          tmp_sentry), (Inl (Z.of_nat offset_store_lin_mem)))) :: ((Load ((R
          tmp_sentry), (R tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
          Big_int_Z.zero_big_int)))) :: ((Mov ((R tmp1), (Inr (R
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Mov ((R
          tmp2), (Inr (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))) :: ((Mov ((R tmp3), (Inr (R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: ((Mov ((R
          tmp4), (Inr (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)), (Inr (R a)))) :: ((Mov ((R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))), (Inr (R v)))) :: ((Mov ((R
          Big_int_Z.zero_big_int), (Inr PC))) :: ((Lea (PC, (Inl
          (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))) :: ((Jmp
          (R tmp_sentry)) :: ((Mov ((R Big_int_Z.zero_big_int), (Inr (R
          tmp0)))) :: ((Mov ((R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)), (Inr (R tmp1)))) :: ((Mov ((R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))), (Inr (R tmp2)))) :: ((Mov ((R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (Inr (R
          tmp3)))) :: ((Mov ((R (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inr (R
          tmp4)))) :: []))))))))))))))))))),
       (sub_reg s (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))
     | I_segload _ ->
       let h0 = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let tmp2 = add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some
       ((instrs
          (app ((Load ((R res), (R h0))) :: [])
            (app ((Mov ((R nreg),
              (r (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov
              ((R tmp2),
              (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                Big_int_Z.zero_big_int))))) :: []))
              (app (dyn_typecheck_instrs h (R res) T_int) ((Mov ((R
                (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
                (r nreg))) :: ((Mov ((R (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
                (r tmp2))) :: [])))))), s)
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
       let tmp = fun i0 ->
         add
           (add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
             (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
             Big_int_Z.zero_big_int))))) i0
       in
       let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       Some
       ((instrs
          (app ((Mov ((R nreg), (Inr r_frame))) :: ((Lea ((R nreg), (Inl
            (Z.of_nat module_frame.idx_safe_mem)))) :: ((Load ((R nreg), (R
            nreg))) :: [])))
            (app
              (malloc_instrs (R nreg) (R size) (tmp Big_int_Z.zero_big_int)
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
                  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) ((Mov
              ((R res),
              (r
                (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))) :: [])))),
       s)
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
     | I_segfree -> None
     | I_current_memory ->
       let tmp_sentry =
         add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
       in
       let tmp0 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))
       in
       let tmp1 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
       in
       let tmp2 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))))
       in
       let tmp3 =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
       in
       let offset_current_lin_mem =
         add module_frame.idx_linear_memory (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))
       in
       Some
       ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
          tmp_sentry), (Inl (Z.of_nat offset_current_lin_mem)))) :: ((Load
          ((R tmp_sentry), (R tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
          Big_int_Z.zero_big_int)))) :: ((Mov ((R tmp1), (Inr (R
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Mov ((R
          tmp2), (Inr (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))) :: ((Mov ((R tmp3), (Inr (R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: ((Mov ((R
          Big_int_Z.zero_big_int), (Inr PC))) :: ((Lea (PC, (Inl
          (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))) :: ((Jmp
          (R tmp_sentry)) :: ((Mov ((R nreg), (Inr (R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int),
          (Inr (R tmp0)))) :: ((Mov ((R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)), (Inr (R tmp1)))) :: ((Mov ((R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))), (Inr (R tmp2)))) :: ((Mov ((R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (Inr (R
          tmp3)))) :: [])))))))))))))))),
       (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | I_grow_memory ->
       let size = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
       let tmp_sentry =
         add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))))))
       in
       let tmp = fun k -> add tmp_sentry k in
       let offset_grow_lin_mem =
         add module_frame.idx_linear_memory (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int)))
       in
       Some
       ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
          tmp_sentry), (Inl (Z.of_nat offset_grow_lin_mem)))) :: ((Load ((R
          tmp_sentry), (R tmp_sentry))) :: ((Mov ((R
          (tmp Big_int_Z.zero_big_int)), (Inr (R
          Big_int_Z.zero_big_int)))) :: ((Mov ((R
          (tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr (R
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Mov ((R
          (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int)))), (Inr (R (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((Mov ((R
          (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inr (R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: ((Mov ((R
          (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int)))))), (Inr (R (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))) :: ((Mov ((R
          (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))), (Inr (R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))) :: ((Mov
          ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr (R
          size)))) :: ((Mov ((R Big_int_Z.zero_big_int), (Inr PC))) :: ((Lea
          (PC, (Inl (Big_int_Z.mult_int_big_int 2
          Big_int_Z.unit_big_int)))) :: ((Jmp (R tmp_sentry)) :: ((Mov ((R
          res), (Inr (R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int),
          (Inr (R (tmp Big_int_Z.zero_big_int))))) :: ((Mov ((R
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr (R
          (tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((Mov
          ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))), (Inr (R
          (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int))))))) :: ((Mov ((R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (Inr (R
          (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))) :: ((Mov
          ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))))), (Inr (R
          (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int))))))))) :: ((Mov ((R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inr (R
          (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))))) :: []))))))))))))))))))))),
       s)
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
       (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
    (Obj.magic get_type module0 f_typeidx)

(** val labeled_compile_expr' :
    machineParameters -> ws_module -> typeidx -> value_type list ->
    ws_basic_instruction list -> compilation_state -> frame -> (labeled_instr
    list * compilation_state) option **)

let rec labeled_compile_expr' h module0 f_typeidx f_locals il s module_frame =
  match il with
  | [] -> Some ([], s)
  | i :: il' ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
      let (instrs_comp, s_comp) = pat in
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat0 ->
        let (instr_rec, state_rec) = pat0 in
        Some ((app instrs_comp instr_rec), state_rec))
        (labeled_compile_expr' h module0 f_typeidx f_locals il' s_comp
          module_frame))
      (compile_basic_instr h module0 f_typeidx f_locals i s module_frame)

(** val len_imports_functions : ws_module -> Big_int_Z.big_int **)

let len_imports_functions module0 =
  let rec len_imports_functions' = function
  | [] -> Big_int_Z.zero_big_int
  | import :: imports' ->
    add
      (match import.imp_desc with
       | ID_func _ -> Big_int_Z.succ_big_int Big_int_Z.zero_big_int
       | _ -> Big_int_Z.zero_big_int) (len_imports_functions' imports')
  in len_imports_functions' module0.mod_imports

(** val len_defined_functions : ws_module -> Big_int_Z.big_int **)

let len_defined_functions module0 =
  length module0.mod_funcs

(** val len_imports_globals : ws_module -> Big_int_Z.big_int **)

let len_imports_globals module0 =
  let rec len_imports_globals' = function
  | [] -> Big_int_Z.zero_big_int
  | import :: imports' ->
    add
      (match import.imp_desc with
       | ID_global _ ->
         Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int)
       | _ -> Big_int_Z.zero_big_int) (len_imports_globals' imports')
  in len_imports_globals' module0.mod_imports

(** val len_defined_globals : ws_module -> Big_int_Z.big_int **)

let len_defined_globals module0 =
  mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)) (length module0.mod_globals)

(** val define_module_frame : ws_module -> frame **)

let define_module_frame module0 =
  let len_linear_memory_table = Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))
  in
  let len_indirect_table0 = Big_int_Z.succ_big_int Big_int_Z.zero_big_int in
  let idx_imports_functions0 = Big_int_Z.zero_big_int in
  let idx_local_functions =
    add idx_imports_functions0 (len_imports_functions module0)
  in
  let idx_linear_memory0 =
    add idx_local_functions (len_defined_functions module0)
  in
  let idx_imports_global = add idx_linear_memory0 len_linear_memory_table in
  let idx_globals = add idx_imports_global (len_imports_globals module0) in
  let idx_indirect_table0 = add idx_globals (len_defined_globals module0) in
  let idx_safe_mem0 = add idx_indirect_table0 len_indirect_table0 in
  { idx_imports_functions = idx_imports_functions0; idx_defined_functions =
  idx_local_functions; idx_linear_memory = idx_linear_memory0;
  idx_imports_globals = idx_imports_global; idx_defined_globals =
  idx_globals; idx_indirect_table = idx_indirect_table0; idx_safe_mem =
  idx_safe_mem0 }

(** val labeled_compile_expr :
    machineParameters -> ws_module -> typeidx -> value_type list ->
    ws_basic_instruction list -> compilation_state -> (labeled_instr
    list * compilation_state) option **)

let labeled_compile_expr h module0 f_typeidx f_locals il s =
  let module_frame = define_module_frame module0 in
  labeled_compile_expr' h module0 f_typeidx f_locals il s module_frame

(** val compile_export :
    module_export -> frame -> id0 -> (symbols, id0 * Big_int_Z.big_int) gmap **)

let compile_export exp frm module_name =
  let s = symbols_encode module_name exp.modexp_name in
  (match exp.modexp_desc with
   | MED_func f ->
     singletonM0
       (map_singleton (gmap_partial_alter string_eq_dec string_countable)
         (gmap_empty string_eq_dec string_countable)) s (module_name, f)
   | MED_table _ ->
     singletonM0
       (map_singleton (gmap_partial_alter string_eq_dec string_countable)
         (gmap_empty string_eq_dec string_countable)) s (module_name,
       frm.idx_indirect_table)
   | MED_mem _ ->
     insert0 (map_insert (gmap_partial_alter string_eq_dec string_countable))
       (symbols_append s ('_'::('L'::('o'::('a'::('d'::[])))))) (module_name,
       frm.idx_linear_memory)
       (insert0
         (map_insert (gmap_partial_alter string_eq_dec string_countable))
         (symbols_append s ('_'::('S'::('t'::('o'::('r'::('e'::[])))))))
         (module_name, frm.idx_linear_memory)
         (insert0
           (map_insert (gmap_partial_alter string_eq_dec string_countable))
           (symbols_append s ('_'::('G'::('r'::('o'::('w'::[]))))))
           (module_name, frm.idx_linear_memory)
           (singletonM0
             (map_singleton
               (gmap_partial_alter string_eq_dec string_countable)
               (gmap_empty string_eq_dec string_countable))
             (symbols_append s
               ('_'::('C'::('u'::('r'::('r'::('e'::('n'::('t'::[])))))))))
             (module_name, frm.idx_linear_memory))))
   | MED_global g ->
     insert0 (map_insert (gmap_partial_alter string_eq_dec string_countable))
       (symbols_append s ('_'::('L'::('o'::('a'::('d'::[])))))) (module_name,
       (add frm.idx_imports_globals g))
       (singletonM0
         (map_singleton (gmap_partial_alter string_eq_dec string_countable)
           (gmap_empty string_eq_dec string_countable))
         (symbols_append s ('_'::('S'::('t'::('o'::('r'::('e'::[])))))))
         (module_name, (add frm.idx_imports_globals g))))

(** val compile_exports :
    module_export list -> frame -> name -> (symbols, id0 * Big_int_Z.big_int)
    gmap **)

let rec compile_exports exps frm module_name =
  match exps with
  | [] -> gmap_empty string_eq_dec string_countable
  | exp :: exps' ->
    union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge string_eq_dec string_countable)))
      (compile_export exp frm module_name)
      (compile_exports exps' frm module_name)

(** val load_args :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> result_type ->
    cerise_instruction list **)

let rec load_args mP arg tmp tmp1 tmp2 tf =
  let rarg = R arg in
  let rtmp = R tmp in
  (match tf with
   | [] -> []
   | t :: tf' ->
     app ((LoadU (rtmp, rarg, (Inl (Big_int_Z.minus_big_int
       Big_int_Z.unit_big_int)))) :: ((Lea (rarg, (Inl
       (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: []))
       (app ((Mov ((R tmp1),
         (r (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov ((R
         tmp2),
         (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))))) :: []))
         (app (dyn_typecheck_instrs mP rtmp t)
           (app ((Mov ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
             (r tmp1))) :: ((Mov ((R (Big_int_Z.succ_big_int
             (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
             (r tmp2))) :: []))
             (app ((StoreU (r_stk0, (Inl Big_int_Z.zero_big_int), (Inr
               rtmp))) :: []) (load_args mP arg tmp tmp1 tmp2 tf'))))))

(** val prologue_function :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> function_type ->
    Big_int_Z.big_int -> labeled_instr list **)

let prologue_function mP tmp1 tmp2 tmp3 tmp4 tf size_locals =
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
      (app ((Mov ((R tmp1), (Inr PC))) :: ((GetB ((R tmp2), (R
        tmp1))) :: ((GetA ((R tmp3), (R tmp1))) :: ((Sub ((R tmp3), (r tmp2),
        (r tmp3))) :: ((Lea ((R tmp1), (r tmp3))) :: ((Load ((R
        Big_int_Z.zero_big_int), (R tmp1))) :: []))))))
        (app
          (match rt with
           | [] -> []
           | _ :: _ ->
             (LoadU ((R tmp1), r_stk0, (Inl (Big_int_Z.minus_big_int
               Big_int_Z.unit_big_int)))) :: ((Lea (r_stk0, (Inl
               (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: []))
          (load_args mP tmp1 tmp2 tmp3 tmp4 rt))))
    (instrs (prepare_locals size_locals))

(** val allocate_data : Big_int_Z.big_int -> Big_int_Z.big_int -> data **)

let allocate_data size z0 =
  repeat (Inl z0) size

(** val get_start : ws_module -> name -> (name * Big_int_Z.big_int) option **)

let get_start m mod_name =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun mstart -> Some (mod_name,
    (modstart_func mstart))) (Obj.magic m.mod_start)

(** val compile_func :
    machineParameters -> module_func -> ws_module -> labeled_instr list option **)

let compile_func mP f m =
  let p = (f.modfunc_type, f.modfunc_locals) in
  let mf_body = f.modfunc_body in
  let (mf_type, mf_locals) = p in
  let init_state0 = { regidx = base_reg; current_scope = init_scope_state } in
  let nreg = init_state0.regidx in
  let mf_full_body = app mf_body (I_return :: []) in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
    let (body, _) = pat in
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun f_type -> Some
      (app
        (prologue_function mP nreg
          (add nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
          (add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int)))
          (add nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) f_type
          (length mf_locals)) body)) (Obj.magic get_type m mf_type))
    (Obj.magic labeled_compile_expr mP m mf_type mf_locals mf_full_body
      init_state0)

(** val compile_funcs :
    machineParameters -> ws_module -> labeled_program list option **)

let compile_funcs mP m =
  foldl (fun acc_opt f ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun acc ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_f -> Some
        (compiled_f :: acc)) (Obj.magic compile_func mP f m)) acc_opt) (Some
    []) m.mod_funcs

(** val compile_lin_mem :
    machineParameters -> memory_type list -> Big_int_Z.big_int -> data option **)

let compile_lin_mem mP m max_nb_page =
  match m with
  | [] -> Some []
  | m0 :: l ->
    (match l with
     | [] ->
       let subroutines =
         encodeInstrsW mP
           (app gated_load_lin_mem
             (app (gated_store_lin_mem mP)
               (app (gated_grow_lin_mem mP) (gated_current_lin_mem mP))))
       in
       let size_min = mul m0.lim_min page_size0 in
       let size_max =
         mul
           (match m0.lim_max with
            | Some n0 -> min n0 max_nb_page
            | None -> max_nb_page) page_size0
       in
       let m_b = Big_int_Z.zero_big_int in
       let l_m =
         add (add m_b (length subroutines)) (Big_int_Z.succ_big_int
           (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
       in
       let l_a = add l_m size_min in
       let m_e = add l_m size_max in
       let full_cap = Inr ((((RW, Global), l_m), m_e), l_a) in
       let current_cap = Inr ((((RW, Global), l_m), l_a), l_m) in
       let linear_memory = allocate_data size_max Big_int_Z.zero_big_int in
       Some (full_cap :: (current_cap :: (app subroutines linear_memory)))
     | _ :: _ -> None)

(** val compile_global : machineParameters -> module_glob -> data **)

let compile_global mP g =
  let gtype = g.modglob_type in
  let value_cap =
    let gv = Big_int_Z.succ_big_int Big_int_Z.zero_big_int in
    let perm0 = match gtype.tg_mut with
                | MUT_immut -> RO
                | MUT_mut -> RW in
    Inr ((((perm0, Global), gv),
    (add gv (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), gv)
  in
  let init_value =
    match gtype.tg_t with
    | T_int ->
      (match g.modglob_init with
       | Val_int z0 -> Inl z0
       | Val_handle _ -> Inl Big_int_Z.zero_big_int)
    | T_handle ->
      Inr ((((RW, Global), Big_int_Z.zero_big_int), Big_int_Z.zero_big_int),
        Big_int_Z.zero_big_int)
  in
  let subroutines =
    encodeInstrsW mP
      (app gated_load_global (gated_store_global mP gtype.tg_t))
  in
  value_cap :: (init_value :: subroutines)

(** val compile_globals :
    machineParameters -> module_glob list -> data list **)

let compile_globals mP lg =
  map (compile_global mP) lg

(** val compile_indirect_tables :
    module_table list -> Big_int_Z.big_int -> data option **)

let compile_indirect_tables m max_nb_entry =
  match m with
  | [] -> Some []
  | it :: l ->
    (match l with
     | [] ->
       let nb_entry =
         match (tt_limits (modtab_type it)).lim_max with
         | Some n0 -> min n0 max_nb_entry
         | None -> max_nb_entry
       in
       let indirect_table = allocate_data nb_entry Big_int_Z.zero_big_int in
       Some indirect_table
     | _ :: _ -> None)

(** val import_function : module_import -> symbols option **)

let import_function imp =
  let symbol = symbols_encode imp.imp_module imp.imp_name in
  (match imp.imp_desc with
   | ID_func _ -> Some symbol
   | _ -> None)

(** val imports_functions : module_import list -> symbols list **)

let rec imports_functions = function
| [] -> []
| imp :: imps' ->
  let rec0 = imports_functions imps' in
  (match import_function imp with
   | Some s -> s :: rec0
   | None -> rec0)

(** val import_lin_mem : module_import -> symbols option **)

let import_lin_mem imp =
  let symbol = symbols_encode imp.imp_module imp.imp_name in
  (match imp.imp_desc with
   | ID_mem _ -> Some symbol
   | _ -> None)

(** val imports_lin_mem : module_import list -> symbols list option **)

let rec imports_lin_mem = function
| [] -> None
| imp :: imps' ->
  (match import_lin_mem imp with
   | Some s ->
     Some
       ((symbols_append s ('_'::('L'::('o'::('a'::('d'::[])))))) :: (
       (symbols_append s ('_'::('S'::('t'::('o'::('r'::('e'::[]))))))) :: (
       (symbols_append s ('_'::('G'::('r'::('o'::('w'::[])))))) :: ((symbols_append
                                                                    s
                                                                    ('_'::('C'::('u'::('r'::('r'::('e'::('n'::('t'::[]))))))))) :: []))))
   | None -> imports_lin_mem imps')

(** val frame_entries_lin_mem :
    ws_module -> frame -> name -> frame_entry list option **)

let frame_entries_lin_mem m frm module_name =
  let len_lin_mem_entry = Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
  in
  (match imports_lin_mem m.mod_imports with
   | Some imports_symbols -> Some (map (fun s -> Imports s) imports_symbols)
   | None ->
     Some
       (map (fun i -> Define (module_name, i))
         (seq frm.idx_linear_memory len_lin_mem_entry)))

(** val import_global : module_import -> symbols option **)

let import_global imp =
  let symbol = symbols_encode imp.imp_module imp.imp_name in
  (match imp.imp_desc with
   | ID_global _ -> Some symbol
   | _ -> None)

(** val imports_globals : module_import list -> symbols list **)

let rec imports_globals = function
| [] -> []
| imp :: imps' ->
  let rec0 = imports_globals imps' in
  (match import_global imp with
   | Some s ->
     app
       ((symbols_append s ('_'::('L'::('o'::('a'::('d'::[])))))) :: (
       (symbols_append s ('_'::('S'::('t'::('o'::('r'::('e'::[]))))))) :: []))
       rec0
   | None -> rec0)

(** val import_indirect_table : module_import -> symbols option **)

let import_indirect_table imp =
  let symbol = symbols_encode imp.imp_module imp.imp_name in
  (match imp.imp_desc with
   | ID_table _ -> Some symbol
   | _ -> None)

(** val imports_indirect_table : module_import list -> symbols option **)

let rec imports_indirect_table = function
| [] -> None
| imp :: imps' ->
  (match import_indirect_table imp with
   | Some s -> Some s
   | None -> imports_indirect_table imps')

(** val frame_entry_indirect_cap :
    ws_module -> frame -> name -> frame_entry list option **)

let frame_entry_indirect_cap m frm module_name =
  match imports_indirect_table m.mod_imports with
  | Some imports_symbols -> Some ((Imports imports_symbols) :: [])
  | None -> Some ((Define (module_name, frm.idx_indirect_table)) :: [])

(** val compile_frame : ws_module -> frame -> name -> cerise_frame option **)

let compile_frame m frm module_name =
  let imported_functions =
    map (fun s -> Imports s) (imports_functions m.mod_imports)
  in
  let defined_functions =
    map (fun i -> Define (module_name, i))
      (seq frm.idx_defined_functions (len_defined_functions m))
  in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun linear_memory ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun indirect_tbl ->
      let imported_globals =
        map (fun s -> Imports s) (imports_globals m.mod_imports)
      in
      let defined_globals =
        map (fun i -> Define (module_name, i))
          (seq frm.idx_defined_globals (len_defined_globals m))
      in
      let safe_memory = SafeMem :: [] in
      Some { cframe_functions_imports = imported_functions;
      cframe_functions_defined = defined_functions; cframe_linear_memory =
      linear_memory; cframe_globals_imports = imported_globals;
      cframe_globals_defined = defined_globals; cframe_indirect_table =
      indirect_tbl; cframe_safe_mem = safe_memory })
      (Obj.magic frame_entry_indirect_cap m frm module_name))
    (Obj.magic frame_entries_lin_mem m frm module_name)

(** val compile_module :
    machineParameters -> ws_module -> name -> labeled_cerise_component option **)

let compile_module mP m module_name =
  let mAX_LIN_MEM = Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
  in
  let mAX_INDIRECT_TABLE = Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
  in
  let frm = define_module_frame m in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_segment ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun lin_mem ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun indirect_table ->
        mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_frame ->
          let globals = compile_globals mP m.mod_globals in
          let module_map = fun v ->
            singletonM0
              (map_singleton
                (gmap_partial_alter string_eq_dec string_countable)
                (gmap_empty string_eq_dec string_countable)) module_name v
          in
          Some { l_functions = (module_map compiled_segment); l_frame =
          (module_map compiled_frame); l_lin_mem = (module_map lin_mem);
          l_globals = (module_map globals); l_indirect_table =
          (module_map indirect_table); l_exports =
          (compile_exports m.mod_exports frm module_name); l_main =
          (get_start m module_name) })
          (Obj.magic compile_frame m frm module_name))
        (Obj.magic compile_indirect_tables m.mod_tables mAX_INDIRECT_TABLE))
      (Obj.magic compile_lin_mem mP m.mod_mems mAX_LIN_MEM))
    (Obj.magic compile_funcs mP m)

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

(** val compile_functions :
    labeled_program list -> cerise_program list option **)

let compile_functions progs =
  foldl (fun acc_opt p ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun acc ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_prog -> Some
        (compiled_prog :: acc)) (Obj.magic branch_labels p)) acc_opt) (Some
    []) progs

(** val compile_lfunctions :
    (id0, labeled_program list) gmap -> (id0, cerise_program list) gmap option **)

let compile_lfunctions progs =
  map_fold (gmap_to_list string_eq_dec string_countable)
    (fun id2 p acc_opt ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun acc ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_prog -> Some
        (insert0
          (map_insert (gmap_partial_alter string_eq_dec string_countable))
          id2 compiled_prog acc)) (Obj.magic compile_functions p)) acc_opt)
    (Some (empty0 (gmap_empty string_eq_dec string_countable))) progs

(** val compile_component :
    labeled_cerise_component -> cerise_pre_component option **)

let compile_component m =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_functions -> Some
    { c_functions = compiled_functions; c_frame = m.l_frame; c_lin_mem =
    m.l_lin_mem; c_globals = m.l_globals; c_indirect_table =
    m.l_indirect_table; c_exports = m.l_exports; c_main = m.l_main })
    (Obj.magic compile_lfunctions m.l_functions)

type submodule = { s_id : id0; s_functions : cerise_program list;
                   s_frame : cerise_frame; s_lin_mem : data;
                   s_globals : data list; s_indirect_table : data }

(** val component_to_submodules' :
    cerise_pre_component -> id0 list -> submodule list option **)

let rec component_to_submodules' precomp = function
| [] -> Some []
| i :: ids' ->
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun l ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun sfrm ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun sf ->
        mbind (Obj.magic (fun _ _ -> option_bind)) (fun slm ->
          mbind (Obj.magic (fun _ _ -> option_bind)) (fun sg ->
            mbind (Obj.magic (fun _ _ -> option_bind)) (fun sit -> Some
              ({ s_id = i; s_functions = sf; s_frame = sfrm; s_lin_mem = slm;
              s_globals = sg; s_indirect_table = sit } :: l))
              (lookup0 (Obj.magic gmap_lookup string_eq_dec string_countable)
                i precomp.c_indirect_table))
            (lookup0 (Obj.magic gmap_lookup string_eq_dec string_countable) i
              precomp.c_globals))
          (lookup0 (Obj.magic gmap_lookup string_eq_dec string_countable) i
            precomp.c_lin_mem))
        (lookup0 (Obj.magic gmap_lookup string_eq_dec string_countable) i
          precomp.c_functions))
      (lookup0 (Obj.magic gmap_lookup string_eq_dec string_countable) i
        precomp.c_frame)) (component_to_submodules' precomp ids')

(** val component_to_submodules :
    cerise_pre_component -> submodule list option **)

let component_to_submodules precomp =
  let ids =
    map fst
      (map_to_list (gmap_to_list string_eq_dec string_countable)
        precomp.c_functions)
  in
  component_to_submodules' precomp ids

(** val cap_entry_points :
    cerise_program list -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> word list **)

let rec cap_entry_points functions b e entry_point =
  match functions with
  | [] -> []
  | f :: functions' ->
    let next_entry_point = add entry_point (length f) in
    (Inr ((((E, Global), b), e),
    entry_point)) :: (cap_entry_points functions' b e next_entry_point)

type mem = (addr, word) gmap

(** val len_frame_submodule : submodule -> Big_int_Z.big_int **)

let len_frame_submodule s =
  add
    (add
      (add
        (add
          (add
            (add (length s.s_frame.cframe_functions_imports)
              (length s.s_frame.cframe_functions_defined))
            (length s.s_frame.cframe_linear_memory))
          (length s.s_frame.cframe_globals_imports))
        (length s.s_frame.cframe_globals_defined))
      (length s.s_frame.cframe_indirect_table))
    (length s.s_frame.cframe_safe_mem)

(** val len_code_submodule : submodule -> Big_int_Z.big_int **)

let len_code_submodule s =
  foldl (fun acc f -> add acc (length f)) (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int) s.s_functions

(** val len_code : submodule list -> Big_int_Z.big_int **)

let len_code submodule_list =
  foldl (fun acc s -> add acc (len_code_submodule s)) Big_int_Z.zero_big_int
    submodule_list

(** val len_frames : submodule list -> Big_int_Z.big_int **)

let len_frames submodule_list =
  foldl (fun acc s -> add acc (len_frame_submodule s)) Big_int_Z.zero_big_int
    submodule_list

(** val len_linear_memories : submodule list -> Big_int_Z.big_int **)

let len_linear_memories submodule_list =
  foldl (fun acc s -> add acc (length s.s_lin_mem)) Big_int_Z.zero_big_int
    submodule_list

(** val len_globals : submodule list -> Big_int_Z.big_int **)

let len_globals submodule_list =
  foldl (fun acc s -> add acc (length (concat s.s_globals)))
    Big_int_Z.zero_big_int submodule_list

(** val len_indirect_table : submodule list -> Big_int_Z.big_int **)

let len_indirect_table submodule_list =
  foldl (fun acc s -> add acc (length s.s_indirect_table))
    Big_int_Z.zero_big_int submodule_list

(** val global_entry_point : data -> Big_int_Z.big_int -> word * word **)

let global_entry_point global offset0 =
  let e = add offset0 (length global) in
  let load_sentry = Inr ((((E, Global), offset0), e),
    (add offset0 entry_point_load_global))
  in
  let store_sentry = Inr ((((E, Global), offset0), e),
    (add offset0 entry_point_store_global))
  in
  (load_sentry, store_sentry)

(** val globals_entry_points : data list -> Big_int_Z.big_int -> word list **)

let rec globals_entry_points globals offset0 =
  match globals with
  | [] -> []
  | g :: globals' ->
    let (load_sentry, store_sentry) = global_entry_point g offset0 in
    let new_offset = add offset0 (length g) in
    load_sentry :: (store_sentry :: (globals_entry_points globals' new_offset))

(** val frame_entry_to_word :
    frame_entry -> id0 -> mem -> Big_int_Z.big_int -> word -> (frame_entry,
    word) sum **)

let frame_entry_to_word entry comp_id frame_inst offset_frame cap_malloc =
  match entry with
  | Imports _ -> Inr (Inl Big_int_Z.zero_big_int)
  | Define (module0, idx) ->
    if eqb comp_id module0
    then Inr
           (match lookup0 (gmap_lookup Coq_Nat.eq_dec nat_countable)
                    (add offset_frame idx) frame_inst with
            | Some w -> w
            | None -> Inl Big_int_Z.zero_big_int)
    else Inl entry
  | SafeMem -> Inr cap_malloc

(** val cerise_frame_to_data :
    cerise_frame -> id0 -> mem -> Big_int_Z.big_int -> word -> (frame_entry,
    word) sum list **)

let cerise_frame_to_data frm comp_id frame_inst offset_frame cap_malloc =
  let subframe_to_data = fun sf ->
    map (fun e ->
      frame_entry_to_word e comp_id frame_inst offset_frame cap_malloc) sf
  in
  app (subframe_to_data frm.cframe_functions_imports)
    (app (subframe_to_data frm.cframe_functions_defined)
      (app (subframe_to_data frm.cframe_linear_memory)
        (app (subframe_to_data frm.cframe_globals_imports)
          (app (subframe_to_data frm.cframe_globals_defined)
            (app (subframe_to_data frm.cframe_indirect_table)
              (subframe_to_data frm.cframe_safe_mem))))))

type preMem = (addr, (frame_entry, word) sum) gmap

(** val instantiate_globals : data list -> Big_int_Z.big_int -> preMem **)

let rec instantiate_globals globals offset0 =
  match globals with
  | [] -> gmap_empty Coq_Nat.eq_dec nat_countable
  | g :: globals' ->
    union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
      (list_to_mem (map (fun x -> Inr x) (shift_data g offset0)) offset0)
      (instantiate_globals globals' (add offset0 (length g)))

(** val load_submodule_in_memory :
    machineParameters -> submodule -> Big_int_Z.big_int -> Big_int_Z.big_int
    -> Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int -> word ->
    preMem **)

let load_submodule_in_memory mP s offset_code offset_frame offset_lin_mem offset_globals offset_indirect_table cap_malloc =
  let frame_capability = ((((RO, Global), offset_frame), offset_lin_mem),
    offset_frame)
  in
  let code = (Inr
    frame_capability) :: (encodeInstrsW mP (concat s.s_functions))
  in
  let code_section = list_to_mem (map (fun x -> Inr x) code) offset_code in
  let local_ftable =
    let a_code_init =
      add offset_code (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
    in
    cap_entry_points s.s_functions offset_code offset_frame a_code_init
  in
  let len_imported_functions = length s.s_frame.cframe_functions_imports in
  let offset_local_ftable = add offset_frame len_imported_functions in
  let local_ftable_section = list_to_mem local_ftable offset_local_ftable in
  let lin_mem_table =
    let e_lin_mem = add offset_lin_mem (length s.s_lin_mem) in
    let a_lin_mem_load = add offset_lin_mem entry_point_load_lin_mem in
    let a_lin_mem_store = add offset_lin_mem entry_point_store_lin_mem in
    let a_lin_mem_grow = add offset_lin_mem (entry_point_grow_lin_mem mP) in
    let a_lin_mem_current =
      add offset_lin_mem (entry_point_current_lin_mem mP)
    in
    let cap_lin_mem_load = ((((E, Global), offset_lin_mem), e_lin_mem),
      a_lin_mem_load)
    in
    let cap_lin_mem_store = ((((E, Global), offset_lin_mem), e_lin_mem),
      a_lin_mem_store)
    in
    let cap_lin_mem_grow = ((((E, Global), offset_lin_mem), e_lin_mem),
      a_lin_mem_grow)
    in
    let cap_lin_mem_current = ((((E, Global), offset_lin_mem), e_lin_mem),
      a_lin_mem_current)
    in
    map (fun x -> Inr x)
      (cap_lin_mem_load :: (cap_lin_mem_store :: (cap_lin_mem_grow :: (cap_lin_mem_current :: []))))
  in
  let offset_lin_mem_table = add offset_local_ftable (length local_ftable) in
  let lin_mem_table_section = list_to_mem lin_mem_table offset_lin_mem_table
  in
  let globals_table = globals_entry_points s.s_globals offset_globals in
  let offset_globals_table =
    let len_globals_imports = length s.s_frame.cframe_globals_imports in
    add (add offset_lin_mem_table (length lin_mem_table)) len_globals_imports
  in
  let globals_table_section = list_to_mem globals_table offset_globals_table
  in
  let indirection_table_cap =
    let e_indirect_table =
      add offset_indirect_table (length s.s_indirect_table)
    in
    (Inr ((((RO, Global), offset_indirect_table), e_indirect_table),
    offset_indirect_table)) :: []
  in
  let offset_indirection_table_cap =
    add offset_globals_table (length globals_table)
  in
  let indirection_table_cap_section =
    list_to_mem indirection_table_cap offset_indirection_table_cap
  in
  let frame_inst =
    union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
      (union0
        (map_union
          (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
        (union0
          (map_union
            (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
          local_ftable_section lin_mem_table_section) globals_table_section)
      indirection_table_cap_section
  in
  let instantiate_data = fun d o ->
    list_to_mem (map (fun x -> Inr x) (shift_data d o)) o
  in
  let frame_data =
    cerise_frame_to_data s.s_frame s.s_id frame_inst offset_frame cap_malloc
  in
  let frame0 = list_to_mem frame_data offset_frame in
  let lin_mem = instantiate_data s.s_lin_mem offset_lin_mem in
  let globals = instantiate_globals s.s_globals offset_globals in
  let indirect_table =
    instantiate_data s.s_indirect_table offset_indirect_table
  in
  union0
    (map_union
      (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
    (union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
      (union0
        (map_union
          (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
        (union0
          (map_union
            (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
          code_section frame0) lin_mem) globals) indirect_table

(** val preload_submodules_in_memory :
    machineParameters -> submodule list -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> word -> preMem * (id0, Big_int_Z.big_int) gmap **)

let rec preload_submodules_in_memory mP submodule_list offset_code offset_frame offset_lin_mem offset_globals offset_indirect_table cap_malloc =
  match submodule_list with
  | [] ->
    ((gmap_empty Coq_Nat.eq_dec nat_countable),
      (gmap_empty string_eq_dec string_countable))
  | s :: submodule_list' ->
    let offset_code' = add offset_code (len_code_submodule s) in
    let offset_frame' = add offset_frame (len_frame_submodule s) in
    let offset_lin_mem' = add offset_lin_mem (length s.s_lin_mem) in
    let offset_globals' = add offset_globals (length s.s_globals) in
    let offset_indirect_table' =
      add offset_indirect_table (length s.s_indirect_table)
    in
    let (instantiated, offset_frames_modules) =
      preload_submodules_in_memory mP submodule_list' offset_code'
        offset_frame' offset_lin_mem' offset_globals' offset_indirect_table'
        cap_malloc
    in
    let new_module =
      load_submodule_in_memory mP s offset_code offset_frame offset_lin_mem
        offset_globals offset_indirect_table cap_malloc
    in
    ((union0
       (map_union
         (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
       new_module instantiated),
    (insert0 (map_insert (gmap_partial_alter string_eq_dec string_countable))
      s.s_id offset_frame offset_frames_modules))

(** val solve_undefined :
    (frame_entry, word) sum -> (id0, Big_int_Z.big_int) gmap -> preMem ->
    word -> word **)

let solve_undefined e offset_submodules_frames premem cap_malloc =
  match e with
  | Inl f ->
    (match f with
     | Imports _ -> Inl Big_int_Z.zero_big_int
     | Define (module0, idx) ->
       (match lookup0 (gmap_lookup string_eq_dec string_countable) module0
                offset_submodules_frames with
        | Some idx_frame ->
          (match lookup0 (gmap_lookup Coq_Nat.eq_dec nat_countable)
                   (add idx_frame idx) premem with
           | Some y ->
             (match y with
              | Inl _ -> Inl Big_int_Z.zero_big_int
              | Inr w -> w)
           | None -> Inl Big_int_Z.zero_big_int)
        | None -> Inl Big_int_Z.zero_big_int)
     | SafeMem -> cap_malloc)
  | Inr w -> w

(** val load_submodules_in_memory :
    machineParameters -> submodule list -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> word -> mem * (id0, Big_int_Z.big_int) gmap **)

let load_submodules_in_memory mP submodule_list offset_code offset_frame offset_lin_mem offset_globals offset_indirect_table cap_malloc =
  let (pre_mem, offset_submodules_frame) =
    preload_submodules_in_memory mP submodule_list offset_code offset_frame
      offset_lin_mem offset_globals offset_indirect_table cap_malloc
  in
  ((fmap (Obj.magic (fun _ _ -> gmap_fmap Coq_Nat.eq_dec nat_countable))
     (fun w -> solve_undefined w offset_submodules_frame pre_mem cap_malloc)
     (Obj.magic pre_mem)), offset_submodules_frame)

(** val get_main_word :
    cerise_pre_component -> (id0, Big_int_Z.big_int) gmap -> mem -> word
    option option **)

let get_main_word precomp offset_frames memory =
  match precomp.c_main with
  | Some p ->
    let (module0, idx) = p in
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun offset_module_frame ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun main_word -> Some (Some
        main_word))
        (lookup0 (Obj.magic gmap_lookup Coq_Nat.eq_dec nat_countable)
          (add offset_module_frame idx) memory))
      (lookup0 (Obj.magic gmap_lookup string_eq_dec string_countable) module0
        offset_frames)
  | None -> Some None

(** val load_in_memory :
    machineParameters -> cerise_pre_component -> Big_int_Z.big_int ->
    cerise_component option **)

let load_in_memory mP precomp size_safe_mem =
  let allocate_mem = fun z0 n0 -> repeat (Inl z0) n0 in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun subcomp ->
    let offset_code = Big_int_Z.zero_big_int in
    let offset_frames = add offset_code (len_code subcomp) in
    let offset_lin_mems = add offset_frames (len_frames subcomp) in
    let offset_globals = add offset_lin_mems (len_linear_memories subcomp) in
    let offset_indirect_tables = add offset_globals (len_globals subcomp) in
    let offset_safe_mem =
      add offset_indirect_tables (len_indirect_table subcomp)
    in
    let start_safe_mem = add offset_safe_mem (length malloc_subroutine_instrs)
    in
    let end_safe_mem =
      add
        (add start_safe_mem (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
        size_safe_mem
    in
    let safe_mem =
      let free_space_malloc_cap = ((((RW, Global), start_safe_mem),
        end_safe_mem),
        (add start_safe_mem (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
      in
      app (encodeInstrsW mP malloc_subroutine_instrs)
        (app ((Inr free_space_malloc_cap) :: [])
          (allocate_mem Big_int_Z.zero_big_int size_safe_mem))
    in
    let safe_mem_section = list_to_mem safe_mem offset_safe_mem in
    let malloc_cap = Inr ((((E, Global), offset_safe_mem), end_safe_mem),
      offset_safe_mem)
    in
    let (loaded_subcomp, offset_submodules_frame) =
      load_submodules_in_memory mP subcomp offset_code offset_frames
        offset_lin_mems offset_globals offset_indirect_tables malloc_cap
    in
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun main_word -> Some
      { segment =
      (union0
        (map_union
          (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
        loaded_subcomp safe_mem_section); imports =
      (gmap_empty Coq_Nat.eq_dec nat_countable); exports =
      (gmap_empty string_eq_dec string_countable); main = main_word })
      (Obj.magic get_main_word precomp offset_submodules_frame loaded_subcomp))
    (Obj.magic component_to_submodules precomp)

(** val resolve_import :
    frame_entry -> (symbols, id0 * Big_int_Z.big_int) gmap -> frame_entry **)

let resolve_import e exports0 =
  match e with
  | Imports s ->
    (match lookup0 (gmap_lookup string_eq_dec string_countable) s exports0 with
     | Some y -> let (m, idx) = y in Define (m, idx)
     | None -> e)
  | _ -> e

(** val resolve_frame_link :
    cerise_frame -> (symbols, id0 * Big_int_Z.big_int) gmap -> cerise_frame **)

let resolve_frame_link frm exports0 =
  let resolve = fun sf -> map (fun e -> resolve_import e exports0) sf in
  { cframe_functions_imports = (resolve frm.cframe_functions_imports);
  cframe_functions_defined = (resolve frm.cframe_functions_defined);
  cframe_linear_memory = (resolve frm.cframe_linear_memory);
  cframe_globals_imports = (resolve frm.cframe_globals_imports);
  cframe_globals_defined = (resolve frm.cframe_globals_defined);
  cframe_indirect_table = (resolve frm.cframe_indirect_table);
  cframe_safe_mem = (resolve frm.cframe_safe_mem) }

(** val resolve_main :
    cerise_pre_component -> cerise_pre_component -> (id0 * Big_int_Z.big_int)
    option option **)

let resolve_main p_lib p_client =
  match p_lib.c_main with
  | Some m ->
    (match p_client.c_main with
     | Some _ -> None
     | None -> Some (Some m))
  | None -> Some p_client.c_main

(** val link_seq :
    cerise_pre_component -> cerise_pre_component -> cerise_pre_component
    option **)

let link_seq p_lib p_client =
  let p_frame =
    fmap (Obj.magic (fun _ _ -> gmap_fmap string_eq_dec string_countable))
      (fun f -> resolve_frame_link f p_lib.c_exports) p_client.c_frame
  in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun main_word -> Some
    { c_functions =
    (union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge string_eq_dec string_countable)))
      p_lib.c_functions p_client.c_functions); c_frame =
    (union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge string_eq_dec string_countable)))
      p_lib.c_frame p_frame); c_lin_mem =
    (union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge string_eq_dec string_countable)))
      p_lib.c_lin_mem p_client.c_lin_mem); c_globals =
    (union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge string_eq_dec string_countable)))
      p_lib.c_globals p_client.c_globals); c_indirect_table =
    (union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge string_eq_dec string_countable)))
      p_lib.c_indirect_table p_client.c_indirect_table); c_exports =
    (union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge string_eq_dec string_countable)))
      p_lib.c_exports p_client.c_exports); c_main = main_word })
    (Obj.magic resolve_main p_lib p_client)

(** val instantiation :
    cerise_pre_component list -> cerise_pre_component option **)

let rec instantiation = function
| [] -> None
| p_client :: p_lib' ->
  (match p_lib' with
   | [] -> Some p_client
   | _ :: _ ->
     mbind (Obj.magic (fun _ _ -> option_bind)) (fun p_lib ->
       link_seq p_lib p_client) (instantiation p_lib'))

type reg = (regName, word) gmap

type mem0 = (addr, word) gmap

(** val reg_insert : (regName, word, (regName, word) gmap empty) insert **)

let reg_insert =
  map_insert (gmap_partial_alter reg_eq_dec reg_countable)

(** val boot_code : machineParameters -> word list **)

let boot_code h =
  encodeInstrsW h ((StoreU (STK, (Inl Big_int_Z.zero_big_int), (Inr (R
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Lea ((R
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
    Big_int_Z.unit_big_int))) :: ((StoreU (STK, (Inl Big_int_Z.zero_big_int),
    (Inr STK))) :: ((Jmp (R Big_int_Z.zero_big_int)) :: []))))

(** val boot_code_section : machineParameters -> (addr, word) gmap **)

let boot_code_section h =
  list_to_mem (boot_code h) Big_int_Z.zero_big_int

(** val init_state :
    machineParameters -> cerise_component -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> reg * mem0 **)

let init_state h prog start_stack end_stack =
  let len_boot = length (boot_code h) in
  let pre_heap =
    union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
      (boot_code_section h) (shift_segment prog.segment len_boot)
  in
  let len_pre_heap =
    length (gmap_to_list Coq_Nat.eq_dec nat_countable pre_heap)
  in
  let end_code = encodeInstrsW h (Halt :: []) in
  let end_code_section =
    list_to_mem end_code
      (add len_pre_heap (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let pc_cap = Inr ((((RX, Global), Big_int_Z.zero_big_int), len_boot),
    Big_int_Z.zero_big_int)
  in
  let end_cap = Inr ((((RX, Global), len_pre_heap),
    (add len_pre_heap (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
    len_pre_heap)
  in
  let main_cap =
    match prog.main with
    | Some w ->
      (match w with
       | Inl z0 -> Inl z0
       | Inr c -> Inr (shift_cap c len_boot))
    | None -> Inl Big_int_Z.zero_big_int
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
  let heap =
    union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq_Nat.eq_dec nat_countable)))
      pre_heap end_code_section
  in
  (regfile, heap)

(** val tf_f_client : function_type **)

let tf_f_client =
  Tf ([], (T_int :: []))

(** val f_client : ws_basic_instruction list **)

let f_client =
  (I_const (Val_int
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int)))))))) :: ((I_const (Val_int
    (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))))) :: ((I_call
    Big_int_Z.zero_big_int) :: []))

(** val tf_f_lib : function_type **)

let tf_f_lib =
  Tf ((T_int :: (T_int :: [])), (T_int :: []))

(** val f_lib : ws_basic_instruction list **)

let f_lib =
  (I_get_local Big_int_Z.zero_big_int) :: ((I_get_local
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((I_binop (T_int,
    BOI_add)) :: (I_return :: [])))

(** val simple_table : module_table **)

let simple_table =
  { lim_min = (Big_int_Z.succ_big_int Big_int_Z.zero_big_int); lim_max =
    (Some (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) }

(** val simple_mem : memory_type **)

let simple_mem =
  { lim_min = (Big_int_Z.succ_big_int Big_int_Z.zero_big_int); lim_max =
    (Some (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) }

(** val dummy_module_lib : ws_module **)

let dummy_module_lib =
  { mod_types = (tf_f_lib :: (tf_f_client :: [])); mod_funcs =
    ({ modfunc_type = Big_int_Z.zero_big_int; modfunc_locals = [];
    modfunc_body = f_lib } :: []); mod_tables = (simple_table :: []);
    mod_mems = (simple_mem :: []); mod_globals = []; mod_start = None;
    mod_imports = []; mod_exports = ({ modexp_name =
    ('f'::('_'::('l'::('i'::('b'::[]))))); modexp_desc = (MED_func
    Big_int_Z.zero_big_int) } :: []) }

(** val dummy_module_client : ws_module **)

let dummy_module_client =
  { mod_types = (tf_f_lib :: (tf_f_client :: [])); mod_funcs =
    ({ modfunc_type = (Big_int_Z.succ_big_int Big_int_Z.zero_big_int);
    modfunc_locals = (T_int :: (T_int :: [])); modfunc_body =
    f_client } :: []); mod_tables = (simple_table :: []); mod_mems =
    (simple_mem :: []); mod_globals = []; mod_start = (Some
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)); mod_imports =
    ({ imp_module = ('e'::('n'::('v'::[]))); imp_name =
    ('f'::('_'::('l'::('i'::('b'::[]))))); imp_desc = (ID_func
    Big_int_Z.zero_big_int) } :: []); mod_exports = [] }

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
    bank_prog } :: []); mod_tables = ({ lim_min = (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int); lim_max = (Some (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) } :: []); mod_mems =
    ({ lim_min = (Big_int_Z.succ_big_int Big_int_Z.zero_big_int); lim_max =
    (Some (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) } :: []); mod_globals = []; mod_start = (Some
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)); mod_imports =
    ({ imp_module = ('E'::('n'::('v'::[]))); imp_name =
    ('a'::('d'::('v'::[]))); imp_desc = (ID_func (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)) } :: []); mod_exports = [] }

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
    env_adv_prog } :: []); mod_tables = ({ lim_min = (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int); lim_max = (Some (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) } :: []); mod_mems =
    ({ lim_min = (Big_int_Z.succ_big_int Big_int_Z.zero_big_int); lim_max =
    (Some (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) } :: []); mod_globals = []; mod_start = None;
    mod_imports = []; mod_exports = ({ modexp_name = ('a'::('d'::('v'::[])));
    modexp_desc = (MED_func Big_int_Z.zero_big_int) } :: []) }

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
    mod_mems = ({ lim_min = Big_int_Z.zero_big_int; lim_max = None } :: []);
    mod_globals = []; mod_start = None; mod_imports = []; mod_exports =
    ({ modexp_name =
    ('n'::('e'::('w'::('_'::('s'::('t'::('a'::('c'::('k'::[])))))))));
    modexp_desc = (MED_func Big_int_Z.zero_big_int) } :: ({ modexp_name =
    ('i'::('s'::('_'::('e'::('m'::('p'::('t'::('y'::[])))))))); modexp_desc =
    (MED_func (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)) } :: ({ modexp_name =
    ('i'::('s'::('_'::('f'::('u'::('l'::('l'::[]))))))); modexp_desc =
    (MED_func (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) } :: ({ modexp_name = ('p'::('o'::('p'::[])));
    modexp_desc = (MED_func (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) } :: ({ modexp_name =
    ('p'::('u'::('s'::('h'::[])))); modexp_desc = (MED_func
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) } :: ({ modexp_name =
    ('s'::('t'::('a'::('c'::('k'::('_'::('m'::('a'::('p'::[])))))))));
    modexp_desc = (MED_func (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))) } :: ({ modexp_name =
    ('s'::('t'::('a'::('c'::('k'::('_'::('l'::('e'::('n'::('g'::('t'::('h'::[]))))))))))));
    modexp_desc = (MED_func (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))) } :: ({ modexp_name =
    ('t'::('a'::('b'::('l'::('e'::[]))))); modexp_desc = (MED_table
    Big_int_Z.zero_big_int) } :: [])))))))) }

(** val client_module : ws_module **)

let client_module =
  { mod_types = ((Tf ([], [])) :: ((Tf ([], (T_int :: []))) :: ((Tf
    ((T_int :: []), (T_int :: []))) :: ((Tf ((T_int :: (T_int :: [])),
    [])) :: [])))); mod_funcs = ({ modfunc_type = Big_int_Z.zero_big_int;
    modfunc_locals = (T_int :: []); modfunc_body =
    main_stack } :: ({ modfunc_type = (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)); modfunc_locals = [];
    modfunc_body = square } :: [])); mod_tables = []; mod_mems = [];
    mod_globals = ({ modglob_type = { tg_mut = MUT_mut; tg_t = T_int };
    modglob_init = (Val_int Big_int_Z.zero_big_int) } :: []); mod_start =
    (Some (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))); mod_imports = ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('n'::('e'::('w'::('_'::('s'::('t'::('a'::('c'::('k'::[])))))))));
    imp_desc = (ID_func (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)) } :: ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('i'::('s'::('_'::('e'::('m'::('p'::('t'::('y'::[])))))))); imp_desc =
    (ID_func (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) } :: ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('i'::('s'::('_'::('f'::('u'::('l'::('l'::[]))))))); imp_desc = (ID_func
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) } :: ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('p'::('o'::('p'::[]))); imp_desc = (ID_func (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) } :: ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('p'::('u'::('s'::('h'::[])))); imp_desc = (ID_func
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) } :: ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('s'::('t'::('a'::('c'::('k'::('_'::('m'::('a'::('p'::[])))))))));
    imp_desc = (ID_func (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) } :: ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('s'::('t'::('a'::('c'::('k'::('_'::('l'::('e'::('n'::('g'::('t'::('h'::[]))))))))))));
    imp_desc = (ID_func (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) } :: ({ imp_module =
    ('S'::('t'::('a'::('c'::('k'::[]))))); imp_name =
    ('t'::('a'::('b'::('l'::('e'::[]))))); imp_desc = (ID_table { lim_min =
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int); lim_max = (Some
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) }) } :: [])))))))); mod_exports =
    ({ modexp_name = ('a'::('n'::('s'::('w'::('e'::('r'::[]))))));
    modexp_desc = (MED_global Big_int_Z.zero_big_int) } :: []) }

(** val bank_example :
    ((ws_module * typeidx) * value_type list) * ws_basic_instruction list **)

let bank_example =
  (((bank_module, (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), []),
    bank_prog)

(** val adv_example :
    ((ws_module * typeidx) * value_type list) * ws_basic_instruction list **)

let adv_example =
  (((env_module, Big_int_Z.zero_big_int), []), env_adv_prog)

(** val full_compile :
    machineParameters -> ws_module -> name -> cerise_pre_component option **)

let full_compile mP m n0 =
  mbind (Obj.magic (fun _ _ -> option_bind)) compile_component
    (Obj.magic compile_module mP m n0)

(** val compile_list :
    machineParameters -> (ws_module * name) list -> cerise_pre_component
    option list **)

let compile_list mP ml =
  map (fun m -> full_compile mP (fst m) (snd m)) ml

(** val list_opt : 'a1 option list -> 'a1 list option **)

let list_opt l_opt =
  fold_right (fun e_opt acc_opt ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun e ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun acc -> Some (e :: acc))
        acc_opt) (Obj.magic e_opt)) (Some []) l_opt

(** val load_test :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> (ws_module * name) list -> (regName * word)
    list * (addr * word) list **)

let load_test mP size_safe_mem start_stack end_stack modules =
  match mbind (Obj.magic (fun _ _ -> option_bind)) (fun comps ->
          mbind (Obj.magic (fun _ _ -> option_bind)) (fun linked ->
            mbind (Obj.magic (fun _ _ -> option_bind)) (fun inst ->
              let (regs, mem1) = init_state mP inst start_stack end_stack in
              Some
              (Obj.magic ((gmap_to_list reg_eq_dec reg_countable regs),
                (sort (gmap_to_list Coq_Nat.eq_dec nat_countable mem1)))))
              (Obj.magic load_in_memory mP linked size_safe_mem))
            (Obj.magic instantiation comps))
          (list_opt (compile_list mP modules)) with
  | Some p -> Obj.magic p
  | None -> ([], [])

(** val loaded_bank_example :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    (regName * word) list * (addr * word) list **)

let loaded_bank_example mP start_stack end_stack =
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
    Big_int_Z.zero_big_int)))))))))))))))))))))))))))))))) start_stack
    end_stack ((bank_module, ('B'::('a'::('n'::('k'::[]))))) :: ((env_module,
    ('E'::('n'::('v'::[])))) :: []))

(** val loaded_stack_example :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    (regName * word) list * (addr * word) list **)

let loaded_stack_example mP start_stack end_stack =
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
    Big_int_Z.zero_big_int)))))))))))))))))))))))))))))))) start_stack
    end_stack ((client_module,
    ('C'::('l'::('i'::('e'::('n'::('t'::[]))))))) :: ((stack_module,
    ('S'::('t'::('a'::('c'::('k'::[])))))) :: []))

(** val loaded_dummy_example :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    (regName * word) list * (addr * word) list **)

let loaded_dummy_example mP start_stack end_stack =
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
    Big_int_Z.zero_big_int)))))))))))))))))))))))))))))))) start_stack
    end_stack ((dummy_module_client,
    ('d'::('u'::('m'::('m'::('y'::[])))))) :: ((dummy_module_lib,
    ('e'::('n'::('v'::[])))) :: []))
