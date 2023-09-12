
type __ = Obj.t
let __ = let rec f _ = Obj.repr f in Obj.repr f

(** val xorb : bool -> bool -> bool **)

let xorb b1 b2 =
  if b1 then if b2 then false else true else b2

(** val negb : bool -> bool **)

let negb = function
| true -> false
| false -> true

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

type comparison =
| Eq
| Lt
| Gt

type compareSpecT =
| CompEqT
| CompLtT
| CompGtT

(** val compareSpec2Type : comparison -> compareSpecT **)

let compareSpec2Type = function
| Eq -> CompEqT
| Lt -> CompLtT
| Gt -> CompGtT

type 'a compSpecT = compareSpecT

(** val compSpec2Type : 'a1 -> 'a1 -> comparison -> 'a1 compSpecT **)

let compSpec2Type _ _ =
  compareSpec2Type

(** val id : __ -> __ **)

let id x =
  x

type 'a sig0 = 'a
  (* singleton inductive, whose constructor was exist *)

type ('a, 'p) sigT =
| ExistT of 'a * 'p

(** val projT1 : ('a1, 'a2) sigT -> 'a1 **)

let projT1 = function
| ExistT (a, _) -> a

(** val projT2 : ('a1, 'a2) sigT -> 'a2 **)

let projT2 = function
| ExistT (_, h) -> h



type uint =
| Nil
| D0 of uint
| D1 of uint
| D2 of uint
| D3 of uint
| D4 of uint
| D5 of uint
| D6 of uint
| D7 of uint
| D8 of uint
| D9 of uint

type signed_int =
| Pos of uint
| Neg of uint

(** val nzhead : uint -> uint **)

let rec nzhead d = match d with
| D0 d0 -> nzhead d0
| _ -> d

(** val unorm : uint -> uint **)

let unorm d =
  match nzhead d with
  | Nil -> D0 Nil
  | x -> x

(** val norm : signed_int -> signed_int **)

let norm = function
| Pos d0 -> Pos (unorm d0)
| Neg d0 -> (match nzhead d0 with
             | Nil -> Pos (D0 Nil)
             | x -> Neg x)

(** val revapp : uint -> uint -> uint **)

let rec revapp d d' =
  match d with
  | Nil -> d'
  | D0 d0 -> revapp d0 (D0 d')
  | D1 d0 -> revapp d0 (D1 d')
  | D2 d0 -> revapp d0 (D2 d')
  | D3 d0 -> revapp d0 (D3 d')
  | D4 d0 -> revapp d0 (D4 d')
  | D5 d0 -> revapp d0 (D5 d')
  | D6 d0 -> revapp d0 (D6 d')
  | D7 d0 -> revapp d0 (D7 d')
  | D8 d0 -> revapp d0 (D8 d')
  | D9 d0 -> revapp d0 (D9 d')

(** val rev : uint -> uint **)

let rev d =
  revapp d Nil

module Little =
 struct
  (** val succ : uint -> uint **)

  let rec succ = function
  | Nil -> D1 Nil
  | D0 d0 -> D1 d0
  | D1 d0 -> D2 d0
  | D2 d0 -> D3 d0
  | D3 d0 -> D4 d0
  | D4 d0 -> D5 d0
  | D5 d0 -> D6 d0
  | D6 d0 -> D7 d0
  | D7 d0 -> D8 d0
  | D8 d0 -> D9 d0
  | D9 d0 -> D0 (succ d0)
 end

type uint0 =
| Nil0
| D10 of uint0
| D11 of uint0
| D12 of uint0
| D13 of uint0
| D14 of uint0
| D15 of uint0
| D16 of uint0
| D17 of uint0
| D18 of uint0
| D19 of uint0
| Da of uint0
| Db of uint0
| Dc of uint0
| Dd of uint0
| De of uint0
| Df of uint0

type signed_int0 =
| Pos0 of uint0
| Neg0 of uint0

(** val nzhead0 : uint0 -> uint0 **)

let rec nzhead0 d = match d with
| D10 d0 -> nzhead0 d0
| _ -> d

(** val unorm0 : uint0 -> uint0 **)

let unorm0 d =
  match nzhead0 d with
  | Nil0 -> D10 Nil0
  | x -> x

(** val norm0 : signed_int0 -> signed_int0 **)

let norm0 = function
| Pos0 d0 -> Pos0 (unorm0 d0)
| Neg0 d0 -> (match nzhead0 d0 with
              | Nil0 -> Pos0 (D10 Nil0)
              | x -> Neg0 x)

(** val revapp0 : uint0 -> uint0 -> uint0 **)

let rec revapp0 d d' =
  match d with
  | Nil0 -> d'
  | D10 d0 -> revapp0 d0 (D10 d')
  | D11 d0 -> revapp0 d0 (D11 d')
  | D12 d0 -> revapp0 d0 (D12 d')
  | D13 d0 -> revapp0 d0 (D13 d')
  | D14 d0 -> revapp0 d0 (D14 d')
  | D15 d0 -> revapp0 d0 (D15 d')
  | D16 d0 -> revapp0 d0 (D16 d')
  | D17 d0 -> revapp0 d0 (D17 d')
  | D18 d0 -> revapp0 d0 (D18 d')
  | D19 d0 -> revapp0 d0 (D19 d')
  | Da d0 -> revapp0 d0 (Da d')
  | Db d0 -> revapp0 d0 (Db d')
  | Dc d0 -> revapp0 d0 (Dc d')
  | Dd d0 -> revapp0 d0 (Dd d')
  | De d0 -> revapp0 d0 (De d')
  | Df d0 -> revapp0 d0 (Df d')

(** val rev0 : uint0 -> uint0 **)

let rev0 d =
  revapp0 d Nil0

module Coq_Little =
 struct
  (** val succ : uint0 -> uint0 **)

  let rec succ = function
  | Nil0 -> D11 Nil0
  | D10 d0 -> D11 d0
  | D11 d0 -> D12 d0
  | D12 d0 -> D13 d0
  | D13 d0 -> D14 d0
  | D14 d0 -> D15 d0
  | D15 d0 -> D16 d0
  | D16 d0 -> D17 d0
  | D17 d0 -> D18 d0
  | D18 d0 -> D19 d0
  | D19 d0 -> Da d0
  | Da d0 -> Db d0
  | Db d0 -> Dc d0
  | Dc d0 -> Dd d0
  | Dd d0 -> De d0
  | De d0 -> Df d0
  | Df d0 -> D10 (succ d0)
 end

type uint1 =
| UIntDecimal of uint
| UIntHexadecimal of uint0

type signed_int1 =
| IntDecimal of signed_int
| IntHexadecimal of signed_int0

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

(** val max : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

let rec max = Big_int_Z.max_big_int

(** val min : Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

let rec min = Big_int_Z.min_big_int

type reflect =
| ReflectT
| ReflectF

(** val iff_reflect : bool -> reflect **)

let iff_reflect = function
| true -> ReflectT
| false -> ReflectF

(** val compose : ('a2 -> 'a3) -> ('a1 -> 'a2) -> 'a1 -> 'a3 **)

let compose g f x =
  g (f x)

(** val flip : ('a1 -> 'a2 -> 'a3) -> 'a2 -> 'a1 -> 'a3 **)

let flip f x y =
  f y x

module Nat =
 struct
  type t = Big_int_Z.big_int

  (** val zero : Big_int_Z.big_int **)

  let zero =
    Big_int_Z.zero_big_int

  (** val one : Big_int_Z.big_int **)

  let one =
    Big_int_Z.succ_big_int Big_int_Z.zero_big_int

  (** val two : Big_int_Z.big_int **)

  let two =
    Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)

  (** val succ : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let succ x =
    Big_int_Z.succ_big_int x

  (** val pred : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let pred n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> n0)
      (fun u -> u)
      n0

  (** val add :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec add n0 m =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> m)
      (fun p -> Big_int_Z.succ_big_int (add p m))
      n0

  (** val double : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let double n0 =
    add n0 n0

  (** val mul :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec mul n0 m =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun p -> add m (mul p m))
      n0

  (** val sub :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec sub n0 m =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> n0)
      (fun k ->
      (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ -> n0)
        (fun l -> sub k l)
        m)
      n0

  (** val eqb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let rec eqb = Big_int_Z.eq_big_int

  (** val leb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let rec leb = Big_int_Z.le_big_int

  (** val ltb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let ltb n0 m =
    leb (Big_int_Z.succ_big_int n0) m

  (** val compare : Big_int_Z.big_int -> Big_int_Z.big_int -> comparison **)

  let rec compare = (fun x y -> let s = Big_int_Z.compare_big_int x y in
  if s = 0 then Eq else if s < 0 then Lt else Gt)

  (** val max :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec max n0 m =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> m)
      (fun n' ->
      (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ -> n0)
        (fun m' -> Big_int_Z.succ_big_int (max n' m'))
        m)
      n0

  (** val min :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec min n0 m =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun n' ->
      (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ -> Big_int_Z.zero_big_int)
        (fun m' -> Big_int_Z.succ_big_int (min n' m'))
        m)
      n0

  (** val even : Big_int_Z.big_int -> bool **)

  let rec even n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> true)
      (fun n1 ->
      (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ -> false)
        (fun n' -> even n')
        n1)
      n0

  (** val odd : Big_int_Z.big_int -> bool **)

  let odd n0 =
    negb (even n0)

  (** val pow :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec pow n0 m =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
      (fun m0 -> mul n0 (pow n0 m0))
      m

  (** val tail_add :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec tail_add n0 m =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> m)
      (fun n1 -> tail_add n1 (Big_int_Z.succ_big_int m))
      n0

  (** val tail_addmul :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
      Big_int_Z.big_int **)

  let rec tail_addmul r0 n0 m =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> r0)
      (fun n1 -> tail_addmul (tail_add m r0) n1 m)
      n0

  (** val tail_mul :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let tail_mul n0 m =
    tail_addmul Big_int_Z.zero_big_int n0 m

  (** val of_uint_acc : uint -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec of_uint_acc d acc =
    match d with
    | Nil -> acc
    | D0 d0 ->
      of_uint_acc d0
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))) acc)
    | D1 d0 ->
      of_uint_acc d0 (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))) acc))
    | D2 d0 ->
      of_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))) acc)))
    | D3 d0 ->
      of_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))) acc))))
    | D4 d0 ->
      of_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))) acc)))))
    | D5 d0 ->
      of_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))) acc))))))
    | D6 d0 ->
      of_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))) acc)))))))
    | D7 d0 ->
      of_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))) acc))))))))
    | D8 d0 ->
      of_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))) acc)))))))))
    | D9 d0 ->
      of_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))) acc))))))))))

  (** val of_uint : uint -> Big_int_Z.big_int **)

  let of_uint d =
    of_uint_acc d Big_int_Z.zero_big_int

  (** val of_hex_uint_acc :
      uint0 -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec of_hex_uint_acc d acc =
    match d with
    | Nil0 -> acc
    | D10 d0 ->
      of_hex_uint_acc d0
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc)
    | D11 d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc))
    | D12 d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc)))
    | D13 d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc))))
    | D14 d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc)))))
    | D15 d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc))))))
    | D16 d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc)))))))
    | D17 d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc))))))))
    | D18 d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc)))))))))
    | D19 d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc))))))))))
    | Da d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc)))))))))))
    | Db d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc))))))))))))
    | Dc d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc)))))))))))))
    | Dd d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc))))))))))))))
    | De d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc)))))))))))))))
    | Df d0 ->
      of_hex_uint_acc d0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int
        (tail_mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))))))))))))))) acc))))))))))))))))

  (** val of_hex_uint : uint0 -> Big_int_Z.big_int **)

  let of_hex_uint d =
    of_hex_uint_acc d Big_int_Z.zero_big_int

  (** val of_num_uint : uint1 -> Big_int_Z.big_int **)

  let of_num_uint = function
  | UIntDecimal d0 -> of_uint d0
  | UIntHexadecimal d0 -> of_hex_uint d0

  (** val to_little_uint : Big_int_Z.big_int -> uint -> uint **)

  let rec to_little_uint n0 acc =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> acc)
      (fun n1 -> to_little_uint n1 (Little.succ acc))
      n0

  (** val to_uint : Big_int_Z.big_int -> uint **)

  let to_uint n0 =
    rev (to_little_uint n0 (D0 Nil))

  (** val to_little_hex_uint : Big_int_Z.big_int -> uint0 -> uint0 **)

  let rec to_little_hex_uint n0 acc =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> acc)
      (fun n1 -> to_little_hex_uint n1 (Coq_Little.succ acc))
      n0

  (** val to_hex_uint : Big_int_Z.big_int -> uint0 **)

  let to_hex_uint n0 =
    rev0 (to_little_hex_uint n0 (D10 Nil0))

  (** val to_num_uint : Big_int_Z.big_int -> uint1 **)

  let to_num_uint n0 =
    UIntDecimal (to_uint n0)

  (** val to_num_hex_uint : Big_int_Z.big_int -> uint1 **)

  let to_num_hex_uint n0 =
    UIntHexadecimal (to_hex_uint n0)

  (** val of_int : signed_int -> Big_int_Z.big_int option **)

  let of_int d =
    match norm d with
    | Pos u -> Some (of_uint u)
    | Neg _ -> None

  (** val of_hex_int : signed_int0 -> Big_int_Z.big_int option **)

  let of_hex_int d =
    match norm0 d with
    | Pos0 u -> Some (of_hex_uint u)
    | Neg0 _ -> None

  (** val of_num_int : signed_int1 -> Big_int_Z.big_int option **)

  let of_num_int = function
  | IntDecimal d0 -> of_int d0
  | IntHexadecimal d0 -> of_hex_int d0

  (** val to_int : Big_int_Z.big_int -> signed_int **)

  let to_int n0 =
    Pos (to_uint n0)

  (** val to_hex_int : Big_int_Z.big_int -> signed_int0 **)

  let to_hex_int n0 =
    Pos0 (to_hex_uint n0)

  (** val to_num_int : Big_int_Z.big_int -> signed_int1 **)

  let to_num_int n0 =
    IntDecimal (to_int n0)

  (** val divmod :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
      Big_int_Z.big_int -> Big_int_Z.big_int * Big_int_Z.big_int **)

  let rec divmod x y q u =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> (q, u))
      (fun x' ->
      (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ -> divmod x' y (Big_int_Z.succ_big_int q) y)
        (fun u' -> divmod x' y q u')
        u)
      x

  (** val div :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let div x y =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> y)
      (fun y' -> fst (divmod x y' Big_int_Z.zero_big_int y'))
      y

  (** val modulo :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let modulo x y =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> x)
      (fun y' -> sub y' (snd (divmod x y' Big_int_Z.zero_big_int y')))
      y

  (** val gcd :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec gcd a b =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> b)
      (fun a' ->
      gcd (modulo b (Big_int_Z.succ_big_int a')) (Big_int_Z.succ_big_int a'))
      a

  (** val square : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let square n0 =
    mul n0 n0

  (** val sqrt_iter :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
      Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec sqrt_iter k p q r0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> p)
      (fun k' ->
      (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ ->
        sqrt_iter k' (Big_int_Z.succ_big_int p) (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int q)) (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int q)))
        (fun r' -> sqrt_iter k' p q r')
        r0)
      k

  (** val sqrt : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let sqrt n0 =
    sqrt_iter n0 Big_int_Z.zero_big_int Big_int_Z.zero_big_int
      Big_int_Z.zero_big_int

  (** val log2_iter :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
      Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec log2_iter k p q r0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> p)
      (fun k' ->
      (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ ->
        log2_iter k' (Big_int_Z.succ_big_int p) (Big_int_Z.succ_big_int q) q)
        (fun r' -> log2_iter k' p (Big_int_Z.succ_big_int q) r')
        r0)
      k

  (** val log2 : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let log2 n0 =
    log2_iter (pred n0) Big_int_Z.zero_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int) Big_int_Z.zero_big_int

  (** val iter : Big_int_Z.big_int -> ('a1 -> 'a1) -> 'a1 -> 'a1 **)

  let rec iter n0 f x =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> x)
      (fun n1 -> f (iter n1 f x))
      n0

  (** val div2 : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec div2 = (fun n -> Big_int_Z.div_big_int n (Big_int_Z.big_int_of_int 2))

  (** val testbit : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let rec testbit a n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> odd a)
      (fun n1 -> testbit (div2 a) n1)
      n0

  (** val shiftl :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec shiftl a n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> a)
      (fun n1 -> double (shiftl a n1))
      n0

  (** val shiftr :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec shiftr a n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> a)
      (fun n1 -> div2 (shiftr a n1))
      n0

  (** val bitwise :
      (bool -> bool -> bool) -> Big_int_Z.big_int -> Big_int_Z.big_int ->
      Big_int_Z.big_int -> Big_int_Z.big_int **)

  let rec bitwise op n0 a b =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun n' ->
      add
        (if op (odd a) (odd b)
         then Big_int_Z.succ_big_int Big_int_Z.zero_big_int
         else Big_int_Z.zero_big_int)
        (mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)) (bitwise op n' (div2 a) (div2 b))))
      n0

  (** val coq_land :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let coq_land a b =
    bitwise (&&) a a b

  (** val coq_lor :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let coq_lor a b =
    bitwise (||) (max a b) a b

  (** val ldiff :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let ldiff a b =
    bitwise (fun b0 b' -> (&&) b0 (negb b')) a a b

  (** val coq_lxor :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let coq_lxor a b =
    bitwise xorb (max a b) a b

  (** val recursion :
      'a1 -> (Big_int_Z.big_int -> 'a1 -> 'a1) -> Big_int_Z.big_int -> 'a1 **)

  let rec recursion x f0 n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> x)
      (fun n1 -> f0 n1 (recursion x f0 n1))
      n0

  (** val eq_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let rec eq_dec = Big_int_Z.eq_big_int

  (** val leb_spec0 : Big_int_Z.big_int -> Big_int_Z.big_int -> reflect **)

  let leb_spec0 x y =
    iff_reflect (leb x y)

  (** val ltb_spec0 : Big_int_Z.big_int -> Big_int_Z.big_int -> reflect **)

  let ltb_spec0 x y =
    iff_reflect (ltb x y)

  module Private_OrderTac =
   struct
    module IsTotal =
     struct
     end

    module Tac =
     struct
     end
   end

  module Private_Tac =
   struct
   end

  module Private_Dec =
   struct
    (** val max_case_strong :
        Big_int_Z.big_int -> Big_int_Z.big_int -> (Big_int_Z.big_int ->
        Big_int_Z.big_int -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1)
        -> 'a1 **)

    let max_case_strong n0 m compat hl hr =
      let c = compSpec2Type n0 m (compare n0 m) in
      (match c with
       | CompGtT -> compat n0 (max n0 m) __ (hl __)
       | _ -> compat m (max n0 m) __ (hr __))

    (** val max_case :
        Big_int_Z.big_int -> Big_int_Z.big_int -> (Big_int_Z.big_int ->
        Big_int_Z.big_int -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)

    let max_case n0 m x x0 x1 =
      max_case_strong n0 m x (fun _ -> x0) (fun _ -> x1)

    (** val max_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

    let max_dec n0 m =
      max_case n0 m (fun _ _ _ h0 -> h0) true false

    (** val min_case_strong :
        Big_int_Z.big_int -> Big_int_Z.big_int -> (Big_int_Z.big_int ->
        Big_int_Z.big_int -> __ -> 'a1 -> 'a1) -> (__ -> 'a1) -> (__ -> 'a1)
        -> 'a1 **)

    let min_case_strong n0 m compat hl hr =
      let c = compSpec2Type n0 m (compare n0 m) in
      (match c with
       | CompGtT -> compat m (min n0 m) __ (hr __)
       | _ -> compat n0 (min n0 m) __ (hl __))

    (** val min_case :
        Big_int_Z.big_int -> Big_int_Z.big_int -> (Big_int_Z.big_int ->
        Big_int_Z.big_int -> __ -> 'a1 -> 'a1) -> 'a1 -> 'a1 -> 'a1 **)

    let min_case n0 m x x0 x1 =
      min_case_strong n0 m x (fun _ -> x0) (fun _ -> x1)

    (** val min_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

    let min_dec n0 m =
      min_case n0 m (fun _ _ _ h0 -> h0) true false
   end

  (** val max_case_strong :
      Big_int_Z.big_int -> Big_int_Z.big_int -> (__ -> 'a1) -> (__ -> 'a1) ->
      'a1 **)

  let max_case_strong n0 m x x0 =
    Private_Dec.max_case_strong n0 m (fun _ _ _ x1 -> x1) x x0

  (** val max_case :
      Big_int_Z.big_int -> Big_int_Z.big_int -> 'a1 -> 'a1 -> 'a1 **)

  let max_case n0 m x x0 =
    max_case_strong n0 m (fun _ -> x) (fun _ -> x0)

  (** val max_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let max_dec =
    Private_Dec.max_dec

  (** val min_case_strong :
      Big_int_Z.big_int -> Big_int_Z.big_int -> (__ -> 'a1) -> (__ -> 'a1) ->
      'a1 **)

  let min_case_strong n0 m x x0 =
    Private_Dec.min_case_strong n0 m (fun _ _ _ x1 -> x1) x x0

  (** val min_case :
      Big_int_Z.big_int -> Big_int_Z.big_int -> 'a1 -> 'a1 -> 'a1 **)

  let min_case n0 m x x0 =
    min_case_strong n0 m (fun _ -> x) (fun _ -> x0)

  (** val min_dec : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let min_dec =
    Private_Dec.min_dec

  module Private_Parity =
   struct
   end

  module Private_NZPow =
   struct
   end

  module Private_NZSqrt =
   struct
   end

  (** val sqrt_up : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let sqrt_up a =
    match compare Big_int_Z.zero_big_int a with
    | Lt -> Big_int_Z.succ_big_int (sqrt (pred a))
    | _ -> Big_int_Z.zero_big_int

  (** val log2_up : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let log2_up a =
    match compare (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) a with
    | Lt -> Big_int_Z.succ_big_int (log2 (pred a))
    | _ -> Big_int_Z.zero_big_int

  module Private_NZDiv =
   struct
   end

  (** val lcm :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let lcm a b =
    mul a (div b (gcd a b))

  (** val eqb_spec : Big_int_Z.big_int -> Big_int_Z.big_int -> reflect **)

  let eqb_spec x y =
    iff_reflect (eqb x y)

  (** val b2n : bool -> Big_int_Z.big_int **)

  let b2n = function
  | true -> Big_int_Z.succ_big_int Big_int_Z.zero_big_int
  | false -> Big_int_Z.zero_big_int

  (** val setbit :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let setbit a n0 =
    coq_lor a (shiftl (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) n0)

  (** val clearbit :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let clearbit a n0 =
    ldiff a (shiftl (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) n0)

  (** val ones : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let ones n0 =
    pred (shiftl (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) n0)

  (** val lnot :
      Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int **)

  let lnot a n0 =
    coq_lxor a (ones n0)

  (** val coq_Even_Odd_dec : Big_int_Z.big_int -> bool **)

  let rec coq_Even_Odd_dec n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> true)
      (fun n1 -> if coq_Even_Odd_dec n1 then false else true)
      n0

  type coq_EvenT = Big_int_Z.big_int

  type coq_OddT = Big_int_Z.big_int

  (** val coq_EvenT_0 : coq_EvenT **)

  let coq_EvenT_0 =
    Big_int_Z.zero_big_int

  (** val coq_EvenT_2 : Big_int_Z.big_int -> coq_EvenT -> coq_EvenT **)

  let coq_EvenT_2 _ h0 =
    Big_int_Z.succ_big_int h0

  (** val coq_OddT_1 : coq_OddT **)

  let coq_OddT_1 =
    Big_int_Z.zero_big_int

  (** val coq_OddT_2 : Big_int_Z.big_int -> coq_OddT -> coq_OddT **)

  let coq_OddT_2 _ h0 =
    Big_int_Z.succ_big_int h0

  (** val coq_EvenT_S_OddT : Big_int_Z.big_int -> coq_EvenT -> coq_OddT **)

  let coq_EvenT_S_OddT _ h =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> assert false (* absurd case *))
      (fun n0 -> n0)
      h

  (** val coq_OddT_S_EvenT : Big_int_Z.big_int -> coq_OddT -> coq_EvenT **)

  let coq_OddT_S_EvenT _ h =
    h

  (** val even_EvenT : Big_int_Z.big_int -> coq_EvenT **)

  let rec even_EvenT n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> coq_EvenT_0)
      (fun n1 ->
      (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ -> assert false (* absurd case *))
        (fun n2 -> let he = even_EvenT n2 in coq_EvenT_2 n2 he)
        n1)
      n0

  (** val odd_OddT : Big_int_Z.big_int -> coq_OddT **)

  let rec odd_OddT n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> assert false (* absurd case *))
      (fun n1 ->
      (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ -> coq_OddT_1)
        (fun n2 -> let he = odd_OddT n2 in coq_OddT_2 n2 he)
        n1)
      n0

  (** val coq_Even_EvenT : Big_int_Z.big_int -> coq_EvenT **)

  let coq_Even_EvenT =
    even_EvenT

  (** val coq_Odd_OddT : Big_int_Z.big_int -> coq_OddT **)

  let coq_Odd_OddT =
    odd_OddT

  (** val coq_EvenT_OddT_dec :
      Big_int_Z.big_int -> (coq_EvenT, coq_OddT) sum **)

  let coq_EvenT_OddT_dec n0 =
    if even n0 then Inl (even_EvenT n0) else Inr (odd_OddT n0)

  (** val coq_OddT_EvenT_rect :
      (Big_int_Z.big_int -> coq_EvenT -> 'a2 -> 'a1) -> 'a2 ->
      (Big_int_Z.big_int -> coq_OddT -> 'a1 -> 'a2) -> Big_int_Z.big_int ->
      coq_OddT -> 'a1 **)

  let rec coq_OddT_EvenT_rect hQP hQ0 hPQ n0 h =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ ->
      (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ -> assert false (* absurd case *))
        (fun _ -> assert false (* absurd case *))
        h)
      (fun n1 ->
      (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ -> hQP Big_int_Z.zero_big_int coq_EvenT_0 hQ0)
        (fun n2 ->
        let hES = coq_OddT_S_EvenT (Big_int_Z.succ_big_int n2) h in
        let hO = coq_EvenT_S_OddT n2 hES in
        hQP (Big_int_Z.succ_big_int n2) hES
          (hPQ n2 hO (coq_OddT_EvenT_rect hQP hQ0 hPQ n2 hO)))
        n1)
      n0

  (** val coq_EvenT_OddT_rect :
      (Big_int_Z.big_int -> coq_EvenT -> 'a2 -> 'a1) -> 'a2 ->
      (Big_int_Z.big_int -> coq_OddT -> 'a1 -> 'a2) -> Big_int_Z.big_int ->
      coq_EvenT -> 'a2 **)

  let coq_EvenT_OddT_rect hQP hQ0 hPQ n0 hES =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> hQ0)
      (fun n1 ->
      let hO = coq_EvenT_S_OddT n1 hES in
      hPQ n1 hO (coq_OddT_EvenT_rect hQP hQ0 hPQ n1 hO))
      n0
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

  (** val eqb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let rec eqb p q =
    (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun q0 -> eqb p0 q0)
        (fun _ -> false)
        (fun _ -> false)
        q)
      (fun p0 ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun _ -> false)
        (fun q0 -> eqb p0 q0)
        (fun _ -> false)
        q)
      (fun _ ->
      (fun f2p1 f2p f1 p ->
  if Big_int_Z.le_big_int p Big_int_Z.unit_big_int then f1 () else
  let (q,r) = Big_int_Z.quomod_big_int p (Big_int_Z.big_int_of_int 2) in
  if Big_int_Z.eq_big_int r Big_int_Z.zero_big_int then f2p q else f2p1 q)
        (fun _ -> false)
        (fun _ -> false)
        (fun _ -> true)
        q)
      p

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

(** val rev1 : 'a1 list -> 'a1 list **)

let rec rev1 = function
| [] -> []
| x :: l' -> app (rev1 l') (x :: [])

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
| a :: t0 -> (f a) :: (map f t0)

(** val fold_right : ('a2 -> 'a1 -> 'a1) -> 'a1 -> 'a2 list -> 'a1 **)

let rec fold_right f a0 = function
| [] -> a0
| b :: t0 -> f b (fold_right f a0 t0)

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

  (** val eqb : Big_int_Z.big_int -> Big_int_Z.big_int -> bool **)

  let eqb = Big_int_Z.eq_big_int

  (** val of_nat : Big_int_Z.big_int -> Big_int_Z.big_int **)

  let of_nat n0 =
    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
      (fun _ -> Big_int_Z.zero_big_int)
      (fun n1 -> (Pos.of_succ_nat n1))
      n0
 end

(** val zero0 : char **)

let zero0 = '\000'

(** val shift : bool -> char -> char **)

let shift = fun b c -> Char.chr (((Char.code c) lsl 1) land 255 + if b then 1 else 0)



(** val append : char list -> char list -> char list **)

let rec append s1 s2 =
  match s1 with
  | [] -> s2
  | c::s1' -> c::(append s1' s2)

module Coq_Nat = Nat

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

module Coq0_Nat =
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
| [] -> zero0
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

let rec pto_list_raw j t0 acc =
  match t0 with
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

type errorMsg = char list
  (* singleton inductive, whose constructor was Msg *)

type 'a error =
| Error of errorMsg
| Ok of 'a

(** val error_bind : (__ -> __ error) -> __ error -> __ error **)

let error_bind f = function
| Error e -> Error e
| Ok x -> f x

(** val list_error : 'a1 error list -> 'a1 list error **)

let list_error l_opt =
  fold_right (fun e_opt acc_opt ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun e ->
      mbind (Obj.magic (fun _ _ -> error_bind)) (fun acc -> Ok (e :: acc))
        acc_opt) (Obj.magic e_opt)) (Ok []) l_opt

(** val update_nth_error :
    'a1 list -> Big_int_Z.big_int -> 'a1 -> 'a1 list option **)

let rec update_nth_error l n0 e =
  (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
    (fun _ -> match l with
              | [] -> None
              | _ :: l0 -> Some (e :: l0))
    (fun n1 ->
    match l with
    | [] -> None
    | x :: l' ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun upd_l -> Some
        (x :: upd_l)) (update_nth_error l' n1 e))
    n0

(** val list_to_gmap :
    ('a2, 'a2) relDecision -> 'a2 countable -> ('a2, 'a2) relDecision -> 'a1
    list -> 'a2 -> ('a2 -> 'a2) -> ('a2, 'a1) gmap **)

let rec list_to_gmap eqDecision0 h eqDecision1 l k incr =
  match l with
  | [] -> gmap_empty eqDecision0 h
  | h0 :: t0 ->
    insert0 (map_insert (gmap_partial_alter eqDecision0 h)) k h0
      (list_to_gmap eqDecision0 h eqDecision1 t0 (incr k) incr)

type nbar =
| Finite of Big_int_Z.big_int
| P_infty

(** val nbar_ltb : nbar -> nbar -> bool **)

let nbar_ltb x y =
  match x with
  | Finite x0 ->
    (match y with
     | Finite y0 -> Coq_Nat.ltb x0 y0
     | P_infty -> true)
  | P_infty -> false

(** val nbar_leb : nbar -> nbar -> bool **)

let nbar_leb x y =
  match x with
  | Finite x0 ->
    (match y with
     | Finite y0 -> Coq_Nat.leb x0 y0
     | P_infty -> true)
  | P_infty -> (match y with
                | Finite _ -> false
                | P_infty -> true)

(** val nbar_plus : nbar -> nbar -> nbar **)

let nbar_plus x y =
  match x with
  | Finite x' ->
    (match y with
     | Finite y' -> Finite (add x' y')
     | P_infty -> P_infty)
  | P_infty -> P_infty

(** val nbar_minus' : nbar -> nbar -> nbar option **)

let nbar_minus' x y =
  match x with
  | Finite x' ->
    (match y with
     | Finite y' -> Some (Finite (sub x' y'))
     | P_infty -> None)
  | P_infty ->
    (match y with
     | Finite _ -> Some P_infty
     | P_infty -> Some (Finite Big_int_Z.zero_big_int))

(** val nbar_minus : nbar -> nbar -> nbar **)

let nbar_minus x y =
  match nbar_minus' x y with
  | Some z0 -> z0
  | None -> Finite Big_int_Z.zero_big_int

(** val nbar_eqb : nbar -> nbar -> bool **)

let nbar_eqb x y =
  match x with
  | Finite z1 -> (match y with
                  | Finite z2 -> Nat.eqb z1 z2
                  | P_infty -> false)
  | P_infty -> (match y with
                | Finite _ -> false
                | P_infty -> true)

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

type sealPerms = bool * bool

type addr = Big_int_Z.big_int

type oType = Big_int_Z.big_int

type sealable =
| SCap of (perm * locality) * addr * nbar * addr
| SSealRange of (sealPerms * locality) * oType * oType * oType

type word =
| WInt of Big_int_Z.big_int
| WSealable of sealable
| WSealed of oType * sealable

type cerise_instruction =
| Fail
| Halt
| Jmp of regName
| Jnz of regName * regName
| Mov of regName * (Big_int_Z.big_int, regName) sum
| Load of regName * regName
| Store of regName * (Big_int_Z.big_int, regName) sum
| Lt0 of regName * (Big_int_Z.big_int, regName) sum
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
| GetL of regName * regName
| GetB of regName * regName
| GetE of regName * regName
| GetA of regName * regName
| GetP of regName * regName
| GetWType of regName * regName
| GetOType of regName * regName
| Seal of regName * regName * regName
| UnSeal of regName * regName * regName
| Invoke of regName * regName
| LoadU of regName * regName * (Big_int_Z.big_int, regName) sum
| StoreU of regName * (Big_int_Z.big_int, regName) sum
   * (Big_int_Z.big_int, regName) sum
| PromoteU of regName

(** val nop : cerise_instruction **)

let nop =
  Mov ((R Big_int_Z.zero_big_int), (Inr (R Big_int_Z.zero_big_int)))

type symbols = char list

(** val symbols_encode : char list -> char list -> symbols **)

let symbols_encode module_name symbol_name =
  append (append module_name ('.'::[])) symbol_name

type cerise_function = cerise_instruction list

type section =
| Code
| Data

type section_offset = section * Big_int_Z.big_int

type abstract_word = (word, symbols) sum

type cerise_linkable_object = { c_code : abstract_word list;
                                c_data : abstract_word list;
                                c_main : section_offset option;
                                c_exports : (symbols, section_offset) gmap;
                                c_imports : (symbols, Big_int_Z.big_int) gmap;
                                c_init : (symbols, (Big_int_Z.big_int * word)
                                         list) gmap }

type cerise_executable_object = { segment : word list; main : word }

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
             | R n1 -> Coq0_Nat.eq_dec n0 n1
             | _ -> false)

(** val reg_countable : regName countable **)

let reg_countable =
  { encode = (fun r0 ->
    (sum_countable (sum_eq_dec unit_eq_dec unit_eq_dec)
      (sum_countable unit_eq_dec unit_countable unit_eq_dec unit_countable)
      Coq0_Nat.eq_dec nat_countable).encode
      (match r0 with
       | PC -> Inl (Inl ())
       | STK -> Inl (Inr ())
       | R n0 -> Inr n0)); decode = (fun n0 ->
    match (sum_countable (sum_eq_dec unit_eq_dec unit_eq_dec)
            (sum_countable unit_eq_dec unit_countable unit_eq_dec
              unit_countable) Coq0_Nat.eq_dec nat_countable).decode n0 with
    | Some y ->
      (match y with
       | Inl y0 -> (match y0 with
                    | Inl _ -> Some PC
                    | Inr _ -> Some STK)
       | Inr n1 -> Some (R n1))
    | None -> None) }

(** val reg_insert : (regName, word, (regName, word) gmap empty) insert **)

let reg_insert =
  map_insert (gmap_partial_alter reg_eq_dec reg_countable)

(** val get_reg_num : regName -> Big_int_Z.big_int **)

let get_reg_num = function
| R n0 -> n0
| _ -> Big_int_Z.zero_big_int

(** val get_zreg_num :
    (Big_int_Z.big_int, regName) sum -> Big_int_Z.big_int **)

let get_zreg_num = function
| Inl _ -> Big_int_Z.zero_big_int
| Inr r1 -> get_reg_num r1

(** val max_reg_instr : cerise_instruction -> Big_int_Z.big_int **)

let max_reg_instr = function
| Jmp r0 -> get_reg_num r0
| Jnz (r1, r2) -> max (get_reg_num r1) (get_reg_num r2)
| Mov (r1, zr2) -> max (get_reg_num r1) (get_zreg_num zr2)
| Load (r1, r2) -> max (get_reg_num r1) (get_reg_num r2)
| Store (r1, zr2) -> max (get_reg_num r1) (get_zreg_num zr2)
| Lt0 (r1, zr2, zr3) ->
  let tmp = max (get_zreg_num zr2) (get_zreg_num zr3) in
  max (get_reg_num r1) tmp
| Add (r1, zr2, zr3) ->
  let tmp = max (get_zreg_num zr2) (get_zreg_num zr3) in
  max (get_reg_num r1) tmp
| Sub (r1, zr2, zr3) ->
  let tmp = max (get_zreg_num zr2) (get_zreg_num zr3) in
  max (get_reg_num r1) tmp
| Mul (r1, zr2, zr3) ->
  let tmp = max (get_zreg_num zr2) (get_zreg_num zr3) in
  max (get_reg_num r1) tmp
| Rem (r1, zr2, zr3) ->
  let tmp = max (get_zreg_num zr2) (get_zreg_num zr3) in
  max (get_reg_num r1) tmp
| Div (r1, zr2, zr3) ->
  let tmp = max (get_zreg_num zr2) (get_zreg_num zr3) in
  max (get_reg_num r1) tmp
| Lea (r1, zr2) -> max (get_reg_num r1) (get_zreg_num zr2)
| Restrict (r1, zr2) -> max (get_reg_num r1) (get_zreg_num zr2)
| Subseg (r1, zr2, zr3) ->
  let tmp = max (get_zreg_num zr2) (get_zreg_num zr3) in
  max (get_reg_num r1) tmp
| GetL (r1, r2) -> max (get_reg_num r1) (get_reg_num r2)
| GetB (r1, r2) -> max (get_reg_num r1) (get_reg_num r2)
| GetE (r1, r2) -> max (get_reg_num r1) (get_reg_num r2)
| GetA (r1, r2) -> max (get_reg_num r1) (get_reg_num r2)
| GetP (r1, r2) -> max (get_reg_num r1) (get_reg_num r2)
| GetWType (r1, r2) -> max (get_reg_num r1) (get_reg_num r2)
| GetOType (r1, r2) -> max (get_reg_num r1) (get_reg_num r2)
| Seal (r1, r2, r3) ->
  let tmp = max (get_reg_num r2) (get_reg_num r3) in max (get_reg_num r1) tmp
| UnSeal (r1, r2, r3) ->
  let tmp = max (get_reg_num r2) (get_reg_num r3) in max (get_reg_num r1) tmp
| Invoke (r1, r2) -> max (get_reg_num r1) (get_reg_num r2)
| LoadU (r1, r2, zr3) ->
  let tmp = max (get_reg_num r2) (get_zreg_num zr3) in
  max (get_reg_num r1) tmp
| StoreU (r1, zr2, zr3) ->
  let tmp = max (get_zreg_num zr2) (get_zreg_num zr3) in
  max (get_reg_num r1) tmp
| PromoteU r0 -> get_reg_num r0
| _ -> Big_int_Z.zero_big_int

(** val executeAllowed : perm -> bool **)

let executeAllowed = function
| RX -> true
| E -> true
| RWX -> true
| RWLX -> true
| _ -> false

(** val list_to_mem : 'a1 list -> addr -> (addr, 'a1) gmap **)

let list_to_mem l a =
  list_to_gmap Coq0_Nat.eq_dec nat_countable Coq0_Nat.eq_dec l a (fun a0 ->
    add a0 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))

(** val insert1 : (addr * 'a1) -> (addr * 'a1) list -> (addr * 'a1) list **)

let rec insert1 x l = match l with
| [] -> x :: []
| h :: t0 -> if Nat.ltb (fst h) (fst x) then h :: (insert1 x t0) else x :: l

(** val sort : (addr * 'a1) list -> (addr * 'a1) list **)

let rec sort = function
| [] -> []
| h :: t0 -> insert1 h (sort t0)

(** val shift_cap : sealable -> Big_int_Z.big_int -> sealable **)

let shift_cap c n0 =
  match c with
  | SCap (p0, b, e, a) ->
    SCap (p0, (add b n0), (nbar_plus e (Finite n0)), (add a n0))
  | SSealRange (_, _, _, _) -> c

(** val shift_word : word -> Big_int_Z.big_int -> word **)

let shift_word w n0 =
  match w with
  | WInt _ -> w
  | WSealable sb ->
    (match sb with
     | SCap (p0, b, e, a) -> WSealable (shift_cap (SCap (p0, b, e, a)) n0)
     | SSealRange (_, _, _, _) -> w)
  | WSealed (ot, s) ->
    (match s with
     | SCap (p0, b, e, a) -> WSealed (ot, (shift_cap (SCap (p0, b, e, a)) n0))
     | SSealRange (_, _, _, _) -> w)

(** val shift_segment :
    (addr, word) gmap -> Big_int_Z.big_int -> (addr, word) gmap **)

let shift_segment m n0 =
  let shift_addr =
    kmap (fun _ ->
      map_insert (gmap_partial_alter Coq0_Nat.eq_dec nat_countable))
      (fun _ -> gmap_empty Coq0_Nat.eq_dec nat_countable)
      (Obj.magic (fun _ -> gmap_to_list Coq0_Nat.eq_dec nat_countable))
      (fun a -> add a n0) m
  in
  fmap (Obj.magic (fun _ _ -> gmap_fmap Coq0_Nat.eq_dec nat_countable))
    (fun w -> shift_word w n0) (Obj.magic shift_addr)

(** val compute_func_closure :
    addr -> addr -> 'a1 list list -> ((word * word) * word) list ->
    ((word * word) * word) list error **)

let compute_func_closure frm_b frm_e functions func_closures =
  let b = Big_int_Z.zero_big_int in
  let e = length (concat functions) in
  let rec compute_func_closure' functions0 func_closures0 entry_point =
    match functions0 with
    | [] ->
      (match func_closures0 with
       | [] -> Ok []
       | _ :: _ ->
         Error
           ('['::('a'::('d'::('d'::('r'::('_'::('m'::('a'::('p'::('_'::('c'::('o'::('m'::('m'::('o'::('n'::('.'::('v'::(']'::(' '::('['::('c'::('o'::('m'::('p'::('u'::('t'::('e'::('_'::('f'::('u'::('n'::('c'::('_'::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(']'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('t'::('y'::('p'::('e'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::('.'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
    | f :: fs ->
      (match func_closures0 with
       | [] ->
         Error
           ('['::('a'::('d'::('d'::('r'::('_'::('m'::('a'::('p'::('_'::('c'::('o'::('m'::('m'::('o'::('n'::('.'::('v'::(']'::(' '::('['::('c'::('o'::('m'::('p'::('u'::('t'::('e'::('_'::('f'::('u'::('n'::('c'::('_'::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(']'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('t'::('y'::('p'::('e'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::('.'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
       | fc :: fcs ->
         let (p, type_f) = fc in
         let (se, sd) = p in
         (match se with
          | WSealed (ot1, _) ->
            (match sd with
             | WSealed (ot2, _) ->
               if negb (Nat.eqb ot1 ot2)
               then Error
                      ('['::('a'::('d'::('d'::('r'::('_'::('m'::('a'::('p'::('_'::('c'::('o'::('m'::('m'::('o'::('n'::('.'::('v'::(']'::(' '::('['::('c'::('o'::('m'::('p'::('u'::('t'::('e'::('_'::('f'::('u'::('n'::('c'::('_'::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(']'::(' '::('s'::('a'::('m'::('e'::(' '::('o'::('t'::('y'::('p'::('e'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::('.'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
               else let next_entry_point = add entry_point (length f) in
                    mbind (Obj.magic (fun _ _ -> error_bind)) (fun acc ->
                      let sealed_entry = WSealed (ot1, (SCap ((RX, Global),
                        b, (Finite e), entry_point)))
                      in
                      let sealed_data = WSealed (ot2, (SCap ((RO, Global),
                        frm_b, (Finite frm_e), frm_b)))
                      in
                      Ok (((sealed_entry, sealed_data), type_f) :: acc))
                      (compute_func_closure' fs fcs next_entry_point)
             | _ ->
               Error
                 ('['::('a'::('d'::('d'::('r'::('_'::('m'::('a'::('p'::('_'::('c'::('o'::('m'::('m'::('o'::('n'::('.'::('v'::(']'::(' '::('['::('c'::('o'::('m'::('p'::('u'::('t'::('e'::('_'::('f'::('u'::('n'::('c'::('_'::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(']'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::('.'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          | _ ->
            Error
              ('['::('a'::('d'::('d'::('r'::('_'::('m'::('a'::('p'::('_'::('c'::('o'::('m'::('m'::('o'::('n'::('.'::('v'::(']'::(' '::('['::('c'::('o'::('m'::('p'::('u'::('t'::('e'::('_'::('f'::('u'::('n'::('c'::('_'::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(']'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::(' '::('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::('.'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  in compute_func_closure' functions func_closures Big_int_Z.zero_big_int

type handle = { base : Big_int_Z.big_int; offset : Big_int_Z.big_int;
                bound : Big_int_Z.big_int; valid : bool;
                id0 : Big_int_Z.big_int }

type immediate = Big_int_Z.big_int

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
| BI_unreachable
| BI_nop
| BI_drop
| BI_select
| BI_block of result_type * ws_basic_instruction list
| BI_loop of result_type * ws_basic_instruction list
| BI_if of result_type * ws_basic_instruction list * ws_basic_instruction list
| BI_br of immediate
| BI_br_if of immediate
| BI_return
| BI_call of immediate
| BI_call_indirect of immediate
| BI_get_local of immediate
| BI_set_local of immediate
| BI_tee_local of immediate
| BI_get_global of immediate
| BI_set_global of immediate
| BI_load of value_type
| BI_store of value_type
| BI_segload of value_type
| BI_segstore of value_type
| BI_slice
| BI_segalloc
| BI_handleadd
| BI_segfree
| BI_current_memory
| BI_grow_memory
| BI_const of value
| BI_binop of value_type * binop
| BI_testop of value_type
| BI_relop of value_type * relop

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

let tt_limits t0 =
  t0

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

type module_element = { modelem_table : tableidx;
                        modelem_offset : Big_int_Z.big_int;
                        modelem_init : funcidx list }

type module_glob = { modglob_type : global_type; modglob_init : value }

type ws_module = { mod_types : function_type list;
                   mod_funcs : module_func list;
                   mod_tables : module_table list;
                   mod_mems : memory_type list;
                   mod_globals : module_glob list;
                   mod_elem : module_element list;
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

type extern_t =
| ET_func of function_type
| ET_tab of table_type
| ET_mem of memory_type
| ET_glob of global_type

type labeled_instr =
| Label of Big_int_Z.big_int list
| BInstr of cerise_instruction
| Br_Jmp of Big_int_Z.big_int list
| Br_Jnz of Big_int_Z.big_int list * Big_int_Z.big_int

(** val instrs : cerise_function -> labeled_instr list **)

let instrs li =
  map (fun i -> BInstr i) li

type labeled_function = labeled_instr list

(** val max_reg_instr0 : labeled_instr -> Big_int_Z.big_int **)

let max_reg_instr0 = function
| BInstr i' -> max_reg_instr i'
| Br_Jnz (_, r0) -> r0
| _ -> Big_int_Z.zero_big_int

(** val max_reg : labeled_instr list -> Big_int_Z.big_int **)

let rec max_reg = function
| [] -> Big_int_Z.zero_big_int
| i :: p' -> max (max_reg_instr0 i) (max_reg p')

type labeled_data = { l_data_frame : abstract_word list;
                      l_data_func_closures : ((word * word) * word) list;
                      l_data_section : abstract_word list }

type labeled_cerise_component = { l_code : labeled_function list;
                                  l_data : labeled_data;
                                  l_main : section_offset option;
                                  l_exports : (symbols, section_offset) gmap;
                                  l_imports : (symbols, Big_int_Z.big_int)
                                              gmap;
                                  l_init : (symbols,
                                           (Big_int_Z.big_int * word) list)
                                           gmap }

type machineParameters = { decodeInstr : (Big_int_Z.big_int ->
                                         cerise_instruction);
                           encodeInstr : (cerise_instruction ->
                                         Big_int_Z.big_int);
                           encodePerm : (perm -> Big_int_Z.big_int);
                           encodeLoc : (locality -> Big_int_Z.big_int);
                           decodePermPair : (Big_int_Z.big_int ->
                                            perm * locality);
                           encodePermPair : ((perm * locality) ->
                                            Big_int_Z.big_int);
                           decodeSealPermPair : (Big_int_Z.big_int ->
                                                sealPerms * locality);
                           encodeSealPermPair : ((sealPerms * locality) ->
                                                Big_int_Z.big_int);
                           encodeSealPerms : (sealPerms -> Big_int_Z.big_int);
                           decodeSealPerms : (Big_int_Z.big_int -> sealPerms);
                           encodeWordType : (word -> Big_int_Z.big_int);
                           decodeWordType : (Big_int_Z.big_int -> word) }

(** val encodeInstrW : machineParameters -> cerise_instruction -> word **)

let encodeInstrW h i =
  WInt (h.encodeInstr i)

(** val encodeInstrsW : machineParameters -> cerise_function -> word list **)

let encodeInstrsW h =
  map (encodeInstrW h)

(** val wt_cap : word **)

let wt_cap =
  WSealable (SCap ((O, Global), Big_int_Z.zero_big_int, (Finite
    Big_int_Z.zero_big_int), Big_int_Z.zero_big_int))

(** val wt_sealrange : word **)

let wt_sealrange =
  WSealable (SSealRange (((false, false), Global), Big_int_Z.zero_big_int,
    Big_int_Z.zero_big_int, Big_int_Z.zero_big_int))

(** val wt_sealed : word **)

let wt_sealed =
  WSealed (Big_int_Z.zero_big_int, (SCap ((O, Global),
    Big_int_Z.zero_big_int, (Finite Big_int_Z.zero_big_int),
    Big_int_Z.zero_big_int)))

(** val wt_int : word **)

let wt_int =
  WInt Big_int_Z.zero_big_int

type compilerParameters = { encode_function_type : (function_type ->
                                                   Big_int_Z.big_int);
                            decode_function_type : (Big_int_Z.big_int ->
                                                   function_type);
                            encode_extern_t : (extern_t -> Big_int_Z.big_int);
                            decode_extern_t : (Big_int_Z.big_int -> extern_t
                                              option); otype_stack : 
                            oType; otype_lin_mem : oType;
                            otype_global : oType; otype_safe_mem : oType;
                            otype_module : oType;
                            page_size : Big_int_Z.big_int;
                            max_size_linear_memory : Big_int_Z.big_int;
                            max_size_indirection_table : Big_int_Z.big_int;
                            max_size_safe_mem : Big_int_Z.big_int;
                            common_safe_memory_symbol : symbols;
                            common_linking_table_symbol : symbols }

(** val r_stk : regName **)

let r_stk =
  STK

(** val eq_instrs :
    regName -> regName -> regName -> regName -> cerise_function **)

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
    regName -> regName -> regName -> regName -> cerise_function **)

let neq_instrs res r1 r2 tmp =
  (Sub (res, (Inr r2), (Inr r1))) :: ((Mov (tmp, (Inr PC))) :: ((Lea (tmp,
    (Inl (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))) :: ((Jnz (tmp, res)) :: ((Mov (res, (Inl
    Big_int_Z.zero_big_int))) :: ((Lea (tmp, (Inl
    Big_int_Z.unit_big_int))) :: ((Jmp tmp) :: ((Mov (res, (Inl
    Big_int_Z.unit_big_int))) :: ((Mov (tmp, (Inl
    Big_int_Z.zero_big_int))) :: []))))))))

(** val eqz_instrs : regName -> regName -> regName -> cerise_function **)

let eqz_instrs res r0 tmp =
  (Mov (tmp, (Inr PC))) :: ((Lea (tmp, (Inl (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))) :: ((Jnz (tmp, r0)) :: ((Mov (res, (Inl
    Big_int_Z.unit_big_int))) :: ((Lea (tmp, (Inl
    Big_int_Z.unit_big_int))) :: ((Jmp tmp) :: ((Mov (res, (Inl
    Big_int_Z.zero_big_int))) :: ((Mov (tmp, (Inl
    Big_int_Z.zero_big_int))) :: [])))))))

(** val ge_instrs :
    regName -> regName -> regName -> regName -> cerise_function **)

let ge_instrs res r1 r2 tmp =
  (Lt0 (res, (Inr r2), (Inr r1))) :: ((Mov (tmp, (Inr PC))) :: ((Lea (tmp,
    (Inl (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))) :: ((Jnz (tmp, res)) :: ((Mov (res, (Inl
    Big_int_Z.zero_big_int))) :: ((Lea (tmp, (Inl
    Big_int_Z.unit_big_int))) :: ((Jmp tmp) :: ((Mov (res, (Inl
    Big_int_Z.unit_big_int))) :: ((Mov (tmp, (Inl
    Big_int_Z.zero_big_int))) :: []))))))))

(** val rclear_instrs : regName list -> cerise_function **)

let rclear_instrs r0 =
  map (fun r_i -> Mov (r_i, (Inl Big_int_Z.zero_big_int))) r0

(** val push_instrs :
    (Big_int_Z.big_int, regName) sum list -> cerise_function **)

let push_instrs _UU03c1_s =
  map (fun _UU03c1_ -> StoreU (r_stk, (Inl Big_int_Z.zero_big_int),
    _UU03c1_)) _UU03c1_s

(** val pop_instrs : regName -> cerise_function **)

let pop_instrs r0 =
  (LoadU (r0, r_stk, (Inl (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int)))) :: ((Lea (r_stk, (Inl (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int)))) :: [])

(** val push_env_instrs : cerise_function **)

let push_env_instrs =
  push_instrs
    (map (fun r0 -> Inr r0)
      (list_difference reg_eq_dec all_registers (PC :: (r_stk :: []))))

(** val pop_env_instrs : cerise_function **)

let pop_env_instrs =
  foldl (fun b r0 -> app (pop_instrs r0) b) []
    (list_difference reg_eq_dec all_registers (PC :: (r_stk :: [])))

(** val activation_record :
    machineParameters -> regName -> cerise_function **)

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
    machineParameters -> regName -> cerise_function **)

let call_instrs_prologue_1 h rtmp =
  app (push_instrs ((Inl Big_int_Z.zero_big_int) :: []))
    (app (push_instrs ((Inl Big_int_Z.zero_big_int) :: []))
      (app push_env_instrs
        (app (push_instrs ((Inr r_stk) :: [])) (activation_record h rtmp))))

(** val call_instrs_prologue :
    machineParameters -> regName list -> regName -> cerise_function **)

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
    machineParameters -> regName -> regName -> regName list -> cerise_function **)

let call_instrs h rcode rdata rargs =
  let len_act =
    Z.of_nat
      (length
        (activation_record h (R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))))
  in
  let len_args = Z.of_nat (length rargs) in
  let offset_ret_val =
    Z.add
      (Z.add (Z.add (Z.add len_args len_act) Big_int_Z.unit_big_int)
        (Z.of_nat (length push_env_instrs))) (Big_int_Z.mult_int_big_int 2
      Big_int_Z.unit_big_int)
  in
  app
    (call_instrs_prologue h rargs (R (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))
    (app (push_instrs (map (fun x -> Inr x) (rev1 rargs)))
      (app ((Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inr
        r_stk))) :: ((GetA ((R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))) :: ((Sub ((R
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))), (Inr (R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (Inl
        (Z.of_nat (length rargs))))) :: ((GetA ((R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))))))) :: ((Subseg ((R
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inr (R
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))), (Inr (R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))))) :: [])))))
        (app ((Mov ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr
          r_stk))) :: ((Lea ((R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)), (Inl (Z.opp offset_ret_val)))) :: ((GetA
          ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))), (R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))) :: ((GetA ((R (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))) :: ((Add ((R (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))), (Inr (R (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))))), (Inl
          Big_int_Z.unit_big_int))) :: ((Subseg ((R (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)), (Inr (R (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (Inr (R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: []))))))
          (app ((Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int))), (Inr r_stk))) :: ((PromoteU (R
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int)))) :: ((Lea ((R (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
            (Z.opp (Z.add len_act len_args))))) :: ((Restrict ((R
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int))), (Inl
            (h.encodePermPair (E, Directed))))) :: []))))
            (app ((GetA ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
              (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
              Big_int_Z.zero_big_int))))), r_stk)) :: ((GetE ((R
              (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
              (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
              r_stk)) :: ((Subseg (r_stk, (Inr (R (Big_int_Z.succ_big_int
              (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
              (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inr (R
              (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
              (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: [])))
              (app
                (push_instrs ((Inr (R (Big_int_Z.succ_big_int
                  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: []))
                (app
                  (push_instrs ((Inr (R (Big_int_Z.succ_big_int
                    Big_int_Z.zero_big_int))) :: []))
                  (app
                    (push_instrs ((Inr (R (Big_int_Z.succ_big_int
                      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                      Big_int_Z.zero_big_int))))))) :: []))
                    (app ((Mov ((R Big_int_Z.zero_big_int), (Inr
                      rdata))) :: [])
                      (app
                        (rclear_instrs
                          (list_difference reg_eq_dec all_registers
                            (PC :: ((R
                            Big_int_Z.zero_big_int) :: (rcode :: (r_stk :: []))))))
                        (app ((Invoke (rcode, (R
                          Big_int_Z.zero_big_int))) :: []) pop_env_instrs)))))))))))

(** val reqloc_instrs :
    machineParameters -> regName -> Big_int_Z.big_int -> cerise_instruction
    list **)

let reqloc_instrs h r0 z0 =
  let r1 = R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
  let r2 = R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))
  in
  (GetWType (r1, r0)) :: ((Sub (r1, (Inr r1), (Inl
  (h.encodeWordType wt_cap)))) :: ((Mov (r2, (Inr PC))) :: ((Lea (r2, (Inl
  (Big_int_Z.mult_int_big_int 2
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))))) :: ((Jnz (r2,
  r1)) :: ((GetL (r1, r0)) :: ((Sub (r1, (Inr r1), (Inl z0))) :: ((Mov (r2,
  (Inr PC))) :: ((Lea (r2, (Inl
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))) :: ((Jnz (r2,
  r1)) :: ((Mov (r1, (Inl Big_int_Z.zero_big_int))) :: ((Mov (r2, (Inl
  Big_int_Z.zero_big_int))) :: ((Lea (PC, (Inl
  Big_int_Z.unit_big_int))) :: (Fail :: [])))))))))))))

(** val dyn_typecheck_instrs :
    machineParameters -> regName -> value_type -> cerise_function **)

let dyn_typecheck_instrs h r0 vtype =
  let tmp_reg = R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
  let tmp_jmp = R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))
  in
  (match vtype with
   | T_int ->
     (GetWType (tmp_reg, r0)) :: ((Sub (tmp_reg, (Inr tmp_reg), (Inl
       (h.encodeWordType wt_int)))) :: ((Mov (tmp_jmp, (Inr PC))) :: ((Lea
       (tmp_jmp, (Inl (Big_int_Z.mult_int_big_int 2
       ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
       Big_int_Z.unit_big_int))))) :: ((Jnz (tmp_jmp, tmp_reg)) :: ((Mov
       (tmp_jmp, (Inl Big_int_Z.zero_big_int))) :: ((Mov (tmp_reg, (Inl
       Big_int_Z.zero_big_int))) :: ((Lea (PC, (Inl
       Big_int_Z.unit_big_int))) :: (Fail :: []))))))))
   | T_handle -> reqloc_instrs h r0 (h.encodeLoc Global))

(** val len_dyn_typecheck :
    machineParameters -> value_type -> Big_int_Z.big_int **)

let len_dyn_typecheck h v_type =
  Z.of_nat (length (dyn_typecheck_instrs h PC v_type))

(** val entry_point_malloc_safe_mem : Big_int_Z.big_int **)

let entry_point_malloc_safe_mem =
  Big_int_Z.succ_big_int Big_int_Z.zero_big_int

(** val malloc_subroutine_instrs : cerise_function **)

let malloc_subroutine_instrs =
  let offset_sealing_cap = Z.opp (Z.of_nat entry_point_malloc_safe_mem) in
  (Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (Inr PC))) :: ((Lea ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inl
  offset_sealing_cap))) :: ((Load ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))) :: ((UnSeal ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))) :: ((Lt0 ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (Inl Big_int_Z.zero_big_int), (Inr (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (Inr PC))) :: ((Lea ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (Inl
  (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
  Big_int_Z.unit_big_int))))) :: ((Jnz ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: (Fail :: ((Load
  ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))) :: ((GetA ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))) :: ((Lea ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (Inr (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))) :: ((Store ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))) :: ((Add ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (Inr (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))) :: ((Subseg ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((Sub ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inr (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inr (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))) :: ((Lea ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  Big_int_Z.zero_big_int))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))), (Inl Big_int_Z.zero_big_int))) :: ((Jmp (R
  Big_int_Z.zero_big_int)) :: []))))))))))))))))))))))

(** val malloc_safe_mem : cerise_function **)

let malloc_safe_mem =
  malloc_subroutine_instrs

(** val z_page_size : compilerParameters -> Big_int_Z.big_int **)

let z_page_size cP =
  Z.of_nat cP.page_size

(** val entry_point_load_lin_mem : Big_int_Z.big_int **)

let entry_point_load_lin_mem =
  Big_int_Z.succ_big_int Big_int_Z.zero_big_int

(** val load_lin_mem : cerise_function **)

let load_lin_mem =
  let offset_sealing_cap = Z.opp (Z.of_nat entry_point_load_lin_mem) in
  let offset_mem_cap = Big_int_Z.unit_big_int in
  (Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (Inr PC))) :: ((Lea ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))), (Inl offset_sealing_cap))) :: ((Load ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))) :: ((UnSeal ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))) :: ((Lea ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (Inl offset_mem_cap))) :: ((Load ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))) :: ((Lea ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (Inr (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))) :: ((Load ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (Inl Big_int_Z.zero_big_int))) :: ((Jmp (R
  Big_int_Z.zero_big_int)) :: [])))))))))

(** val entry_point_store_lin_mem : Big_int_Z.big_int **)

let entry_point_store_lin_mem =
  add entry_point_load_lin_mem (length load_lin_mem)

(** val store_lin_mem : machineParameters -> cerise_function **)

let store_lin_mem h =
  let offset_seal_cap =
    Z.opp
      (Z.add
        (Z.add (Z.of_nat entry_point_store_lin_mem)
          (len_dyn_typecheck h T_int)) (Big_int_Z.mult_int_big_int 2
        (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))
  in
  let offset_mem_cap = Big_int_Z.unit_big_int in
  app ((Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))),
    (r (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))),
    (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))) :: []))
    (app
      (dyn_typecheck_instrs h (R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))) T_int) ((Mov ((R (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)),
      (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
      (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))) :: ((Mov ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inr
      PC))) :: ((Lea ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))), (Inl offset_seal_cap))) :: ((Load ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: ((UnSeal ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (R
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Lea ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inl
      offset_mem_cap))) :: ((Load ((R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))), (R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))))) :: ((Lea ((R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((Store ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inr (R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))))) :: ((Mov ((R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))), (Inl Big_int_Z.zero_big_int))) :: ((Mov
      ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))))), (Inl Big_int_Z.zero_big_int))) :: ((Jmp
      (R Big_int_Z.zero_big_int)) :: []))))))))))))))

(** val entry_point_grow_lin_mem : machineParameters -> Big_int_Z.big_int **)

let entry_point_grow_lin_mem h =
  add entry_point_store_lin_mem (length (store_lin_mem h))

(** val grow_lin_mem :
    machineParameters -> compilerParameters -> cerise_function **)

let grow_lin_mem h cP =
  let offset_seal_cap = Z.opp (Z.of_nat (entry_point_grow_lin_mem h)) in
  (Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (Inr PC))) :: ((Lea ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))), (Inl offset_seal_cap))) :: ((Load ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))) :: ((UnSeal ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))) :: ((Load ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))) :: ((GetE ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: ((GetA ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))) :: ((Mul ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (Inl (z_page_size cP)))) :: ((Add ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))))), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((Mov ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr PC))) :: ((Lea ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
  (Big_int_Z.mult_int_big_int 2
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  Big_int_Z.unit_big_int))))) :: ((Lt0 ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inr (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))))), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))) :: ((Jnz ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))))) :: ((Lea ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (Inl (Big_int_Z.mult_int_big_int 2
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  Big_int_Z.unit_big_int)))))) :: ((Jmp (R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))) :: ((GetB ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: ((GetA ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))) :: ((Sub ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))))), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))) :: ((Div ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  (z_page_size cP)))) :: ((Lea ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((GetA ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
  (R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: ((Store ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))),
  (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: ((GetB ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))) :: ((GetA ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))) :: ((Subseg ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))), (Inr (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))))))) :: ((Sub ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))), (Inr (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))))))) :: ((Lea ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))) :: ((Lea ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))), (Inl Big_int_Z.unit_big_int))) :: ((Store ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))),
  (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))) :: ((Mov ((R
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
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  Big_int_Z.zero_big_int))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))))), (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))))), (Inl Big_int_Z.zero_big_int))) :: ((Jmp (R
  Big_int_Z.zero_big_int)) :: []))))))))))))))))))))))))))))))))))))))

(** val entry_point_current_lin_mem :
    machineParameters -> compilerParameters -> Big_int_Z.big_int **)

let entry_point_current_lin_mem h cP =
  add (entry_point_grow_lin_mem h) (length (grow_lin_mem h cP))

(** val current_lin_mem :
    machineParameters -> compilerParameters -> cerise_function **)

let current_lin_mem h cP =
  let offset_seal_cap = Z.opp (Z.of_nat (entry_point_current_lin_mem h cP)) in
  let offset_mem_cap = Big_int_Z.unit_big_int in
  (Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))), (Inr PC))) :: ((Lea ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  offset_seal_cap))) :: ((Load ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))) :: ((UnSeal ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Lea ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
  offset_mem_cap))) :: ((Load ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))) :: ((GetB ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((GetE ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))) :: ((Sub ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))), (Inr (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((Div ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  (z_page_size cP)))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  Big_int_Z.zero_big_int))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
  (Inl Big_int_Z.zero_big_int))) :: ((Jmp (R
  Big_int_Z.zero_big_int)) :: []))))))))))))

(** val entry_point_load_global : Big_int_Z.big_int **)

let entry_point_load_global =
  Big_int_Z.succ_big_int Big_int_Z.zero_big_int

(** val load_global : cerise_function **)

let load_global =
  let offset_seal_cap = Z.opp (Z.of_nat entry_point_load_global) in
  (Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))), (Inr PC))) :: ((Lea ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  offset_seal_cap))) :: ((Load ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))) :: ((UnSeal ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Load ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
  (Inl Big_int_Z.zero_big_int))) :: ((Jmp (R
  Big_int_Z.zero_big_int)) :: []))))))

(** val entry_point_store_global : Big_int_Z.big_int **)

let entry_point_store_global =
  add entry_point_load_global (length load_global)

(** val store_global : machineParameters -> cerise_function **)

let store_global h =
  let offset_seal_cap = Z.opp (Z.of_nat entry_point_store_global) in
  app ((Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (Inr PC))) :: ((Lea
    ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (Inl
    offset_seal_cap))) :: ((Load ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))) :: ((UnSeal ((R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) :: []))))
    (app ((Lea ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
      (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))) :: ((Load ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))), (R (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))), (Inr PC))) :: ((Lea ((R
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (Inl
      (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
      Big_int_Z.unit_big_int))))) :: ((Jnz ((R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))))), (R (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int)))))) :: (Fail :: []))))))
      (app ((Lea ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
        (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: ((Load ((R
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (R
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov ((R
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))), (Inr PC))) :: ((Lea ((R
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))), (Inl
        (Z.add
          (Z.add
            (Z.add (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)
              (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))
            (len_dyn_typecheck h T_int)) (Big_int_Z.mult_int_big_int 2
          Big_int_Z.unit_big_int))))) :: ((Jnz ((R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))), (R
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: [])))))
        (app ((Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
          (r (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov ((R
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int))))),
          (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int))))) :: []))
          (app
            (dyn_typecheck_instrs h (R (Big_int_Z.succ_big_int
              (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
              (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) T_int)
            (app ((Lea (PC, (Inl
              (Z.add (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)
                (len_dyn_typecheck h T_handle))))) :: [])
              (app ((Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
                (r (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov
                ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                Big_int_Z.zero_big_int))))),
                (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                  Big_int_Z.zero_big_int))))) :: []))
                (app
                  (dyn_typecheck_instrs h (R (Big_int_Z.succ_big_int
                    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
                    T_handle) ((Lea ((R (Big_int_Z.succ_big_int
                  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                  Big_int_Z.zero_big_int)))), (Inl (Big_int_Z.minus_big_int
                  Big_int_Z.unit_big_int)))) :: ((Store ((R
                  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))),
                  (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                    Big_int_Z.zero_big_int))))))) :: ((Mov ((R
                  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
                  Big_int_Z.zero_big_int))) :: ((Mov ((R
                  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                  Big_int_Z.zero_big_int))), (Inl
                  Big_int_Z.zero_big_int))) :: ((Mov ((R
                  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))), (Inl
                  Big_int_Z.zero_big_int))) :: ((Mov ((R
                  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                  Big_int_Z.zero_big_int))))), (Inl
                  Big_int_Z.zero_big_int))) :: ((Jmp (R
                  Big_int_Z.zero_big_int)) :: []))))))))))))))

(** val entry_point_get_type_global :
    machineParameters -> Big_int_Z.big_int **)

let entry_point_get_type_global h =
  add entry_point_store_global (length (store_global h))

(** val get_type_global : machineParameters -> cerise_function **)

let get_type_global h =
  let offset_seal_cap = Z.opp (Z.of_nat (entry_point_get_type_global h)) in
  (Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))), (Inr PC))) :: ((Lea ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  offset_seal_cap))) :: ((Load ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))) :: ((UnSeal ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Lea ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
  Big_int_Z.unit_big_int))) :: ((Load ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  Big_int_Z.zero_big_int))) :: ((Jmp (R Big_int_Z.zero_big_int)) :: [])))))))

(** val entry_point_get_mut_global :
    machineParameters -> Big_int_Z.big_int **)

let entry_point_get_mut_global h =
  add (entry_point_get_type_global h) (length (get_type_global h))

(** val get_mut_global : machineParameters -> cerise_function **)

let get_mut_global h =
  let offset_seal_cap = Z.opp (Z.of_nat (entry_point_get_mut_global h)) in
  (Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))), (Inr PC))) :: ((Lea ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inl
  offset_seal_cap))) :: ((Load ((R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))))) :: ((UnSeal ((R (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Lea ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
  (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))) :: ((Load ((R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (R
  (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
  (Inl Big_int_Z.zero_big_int))) :: ((Jmp (R
  Big_int_Z.zero_big_int)) :: [])))))))

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

type frame = { idx_imports_functions : Big_int_Z.big_int;
               idx_defined_functions : Big_int_Z.big_int;
               idx_imports_lin_mem : Big_int_Z.big_int;
               idx_defined_lin_mem : Big_int_Z.big_int;
               idx_imports_globals : Big_int_Z.big_int;
               idx_defined_globals : Big_int_Z.big_int;
               idx_imports_itable : Big_int_Z.big_int;
               idx_defined_itable : Big_int_Z.big_int;
               idx_safe_mem : Big_int_Z.big_int;
               idx_linking_table : Big_int_Z.big_int }

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
       | ID_global _ -> Big_int_Z.succ_big_int Big_int_Z.zero_big_int
       | _ -> Big_int_Z.zero_big_int) (len_imports_globals' imports')
  in len_imports_globals' module0.mod_imports

(** val len_defined_globals : ws_module -> Big_int_Z.big_int **)

let len_defined_globals module0 =
  length module0.mod_globals

(** val len_imports_lin_mems : ws_module -> Big_int_Z.big_int **)

let len_imports_lin_mems module0 =
  let rec len_imports_lin_mem' = function
  | [] -> Big_int_Z.zero_big_int
  | import :: imports' ->
    (match import.imp_desc with
     | ID_mem _ -> Big_int_Z.succ_big_int Big_int_Z.zero_big_int
     | _ -> len_imports_lin_mem' imports')
  in len_imports_lin_mem' module0.mod_imports

(** val len_defined_lin_mems : ws_module -> Big_int_Z.big_int **)

let len_defined_lin_mems module0 =
  length module0.mod_mems

(** val len_defined_itables : ws_module -> Big_int_Z.big_int **)

let len_defined_itables module0 =
  length module0.mod_tables

(** val len_imports_itables : ws_module -> Big_int_Z.big_int **)

let len_imports_itables module0 =
  let rec len_imports_itable' = function
  | [] -> Big_int_Z.zero_big_int
  | import :: imports' ->
    (match import.imp_desc with
     | ID_table _ -> Big_int_Z.succ_big_int Big_int_Z.zero_big_int
     | _ -> len_imports_itable' imports')
  in len_imports_itable' module0.mod_imports

(** val len_safe_mem : Big_int_Z.big_int **)

let len_safe_mem =
  Big_int_Z.succ_big_int Big_int_Z.zero_big_int

(** val len_linking_table : Big_int_Z.big_int **)

let len_linking_table =
  Big_int_Z.succ_big_int Big_int_Z.zero_big_int

(** val define_module_frame : ws_module -> frame **)

let define_module_frame module0 =
  let idx_imports_functions0 = Big_int_Z.zero_big_int in
  let idx_defined_functions0 =
    add idx_imports_functions0 (len_imports_functions module0)
  in
  let idx_imports_lin_mem0 =
    add idx_defined_functions0 (len_defined_functions module0)
  in
  let idx_defined_lin_mem0 =
    add idx_imports_lin_mem0 (len_imports_lin_mems module0)
  in
  let idx_imports_global =
    add idx_defined_lin_mem0 (len_defined_lin_mems module0)
  in
  let idx_defined_globals0 =
    add idx_imports_global (len_imports_globals module0)
  in
  let idx_imports_itable0 =
    add idx_defined_globals0 (len_defined_globals module0)
  in
  let idx_defined_itable0 =
    add idx_imports_itable0 (len_imports_itables module0)
  in
  let idx_safe_mem0 = add idx_defined_itable0 (len_defined_itables module0) in
  let idx_linking_table0 = add idx_safe_mem0 len_safe_mem in
  { idx_imports_functions = idx_imports_functions0; idx_defined_functions =
  idx_defined_functions0; idx_imports_lin_mem = idx_imports_lin_mem0;
  idx_defined_lin_mem = idx_defined_lin_mem0; idx_imports_globals =
  idx_imports_global; idx_defined_globals = idx_defined_globals0;
  idx_imports_itable = idx_imports_itable0; idx_defined_itable =
  idx_defined_itable0; idx_safe_mem = idx_safe_mem0; idx_linking_table =
  idx_linking_table0 }

(** val offset_lin_mem_load : Big_int_Z.big_int **)

let offset_lin_mem_load =
  Big_int_Z.zero_big_int

(** val offset_lin_mem_store : Big_int_Z.big_int **)

let offset_lin_mem_store =
  Big_int_Z.succ_big_int Big_int_Z.zero_big_int

(** val offset_lin_mem_grow : Big_int_Z.big_int **)

let offset_lin_mem_grow =
  Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)

(** val offset_lin_mem_current : Big_int_Z.big_int **)

let offset_lin_mem_current =
  Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))

(** val offset_global_load : Big_int_Z.big_int **)

let offset_global_load =
  Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))

(** val offset_global_store : Big_int_Z.big_int **)

let offset_global_store =
  Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))

(** val offset_safe_mem_malloc : Big_int_Z.big_int **)

let offset_safe_mem_malloc =
  Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))

(** val len_frame : ws_module -> Big_int_Z.big_int **)

let len_frame module0 =
  add
    (add
      (add
        (add
          (add
            (add
              (add
                (add
                  (add (len_imports_functions module0)
                    (len_defined_functions module0))
                  (len_imports_lin_mems module0))
                (len_defined_lin_mems module0)) (len_imports_globals module0))
            (len_defined_globals module0)) (len_imports_itables module0))
        (len_defined_itables module0)) len_safe_mem) len_linking_table

type compilation_state = { regidx : Big_int_Z.big_int;
                           current_scope : scope_state }

(** val new_state : Big_int_Z.big_int -> scope_state -> compilation_state **)

let new_state reg st =
  { regidx = reg; current_scope = st }

(** val add_reg :
    compilation_state -> Big_int_Z.big_int -> compilation_state **)

let add_reg st n0 =
  new_state (add st.regidx n0) st.current_scope

(** val sub_reg :
    compilation_state -> Big_int_Z.big_int -> compilation_state **)

let sub_reg st n0 =
  new_state (sub st.regidx n0) st.current_scope

(** val enter_scope :
    compilation_state -> result_type -> Big_int_Z.big_int -> compilation_state **)

let enter_scope st rt ridx =
  new_state st.regidx (push_scope rt ridx st.current_scope)

(** val leave_scope : compilation_state -> compilation_state option **)

let leave_scope st =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun top_scope ->
    match top_scope.scope_ret_type with
    | [] ->
      Some
        (new_state top_scope.scope_reg_base
          (pop_scope st.current_scope (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int)))
    | _ :: l ->
      (match l with
       | [] ->
         Some
           (new_state
             (add top_scope.scope_reg_base (Big_int_Z.succ_big_int
               Big_int_Z.zero_big_int))
             (pop_scope st.current_scope (Big_int_Z.succ_big_int
               Big_int_Z.zero_big_int)))
       | _ :: _ -> None))
    (Obj.magic getn_scope st.current_scope Big_int_Z.zero_big_int)

type cfg = labeled_instr list * compilation_state

(** val error_msg : char list -> 'a1 error **)

let error_msg s =
  Error
    (append
      ('['::('c'::('e'::('r'::('i'::('s'::('e'::('_'::('l'::('a'::('b'::('e'::('l'::('e'::('d'::('_'::('g'::('e'::('n'::('.'::('v'::(']'::(' '::[])))))))))))))))))))))))
      s)

(** val r_stk0 : regName **)

let r_stk0 =
  STK

(** val r_frame : regName **)

let r_frame =
  R Big_int_Z.zero_big_int

(** val r_tmp : Big_int_Z.big_int **)

let r_tmp =
  Big_int_Z.succ_big_int Big_int_Z.zero_big_int

(** val nB_GENERAL_PURPOSE_REG : Big_int_Z.big_int **)

let nB_GENERAL_PURPOSE_REG =
  Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))

(** val base_reg : Big_int_Z.big_int **)

let base_reg =
  add (add r_tmp nB_GENERAL_PURPOSE_REG) (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)

(** val call_template :
    machineParameters -> regName -> regName -> regName -> labeled_instr list
    -> regName list -> result_type -> result_type -> compilation_state ->
    (labeled_instr list * compilation_state) error **)

let call_template h r_fun r_data r_res prologue args _ ret_type new_state0 =
  let call = instrs (call_instrs h r_fun r_data args) in
  let epilogue1 = (BInstr (Lea (r_stk0, (Inl (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int))))) :: []
  in
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun epilogue2 -> Ok
    ((app prologue (app call (app epilogue1 epilogue2))), new_state0))
    (match ret_type with
     | [] ->
       Ok
         (Obj.magic ((BInstr (Lea (r_stk0, (Inl (Big_int_Z.minus_big_int
           Big_int_Z.unit_big_int))))) :: []))
     | ret_type0 :: l ->
       (match l with
        | [] ->
          Ok
            (Obj.magic instrs
              (app ((LoadU (r_res, r_stk0, (Inl (Big_int_Z.minus_big_int
                Big_int_Z.unit_big_int)))) :: ((Lea (r_stk0, (Inl
                (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: []))
                (dyn_typecheck_instrs h r_res ret_type0)))
        | _ :: _ ->
          error_msg
            ('['::('c'::('a'::('l'::('l'::('_'::('t'::('e'::('m'::('p'::('l'::('a'::('t'::('e'::(']'::(' '::('W'::('a'::('s'::('m'::(' '::('1'::('.'::('0'::(' '::('a'::('l'::('l'::('o'::('w'::('s'::(' '::('a'::('t'::(' '::('m'::('o'::('s'::('t'::(' '::('o'::('n'::('e'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('v'::('a'::('l'::('u'::('e'::('.'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val local_offset : Big_int_Z.big_int **)

let local_offset =
  (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)

(** val prologue_return :
    function_type -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> cerise_function error **)

let prologue_return f_type _ ret off tmp =
  let ret_type = let Tf (_, r0) = f_type in r0 in
  (match ret_type with
   | [] -> Ok []
   | _ :: l ->
     (match l with
      | [] ->
        Ok ((GetB ((R tmp), r_stk0)) :: ((GetA ((R off), r_stk0)) :: ((Sub
          ((R off), (r tmp), (r off))) :: ((Add ((R off), (r off), (Inl
          (Z.sub local_offset Big_int_Z.unit_big_int)))) :: ((LoadU ((R tmp),
          r_stk0, (r off))) :: ((StoreU ((R tmp), (Inl
          Big_int_Z.zero_big_int), (r ret))) :: []))))))
      | _ :: _ ->
        error_msg
          ('['::('p'::('r'::('o'::('l'::('o'::('g'::('u'::('e'::('_'::('r'::('e'::('t'::('u'::('r'::('n'::(']'::(' '::('W'::('a'::('s'::('m'::(' '::('1'::('.'::('0'::(' '::('a'::('l'::('l'::('o'::('w'::('s'::(' '::('a'::('t'::(' '::('m'::('o'::('s'::('t'::(' '::('o'::('n'::('e'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('v'::('a'::('l'::('u'::('e'::('.'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val compile_binstr :
    compilerParameters -> machineParameters -> ws_module -> function_type ->
    ws_basic_instruction -> compilation_state -> cfg error **)

let rec compile_binstr cP h module0 f_type i s =
  let nreg = s.regidx in
  let module_frame = define_module_frame module0 in
  let error_msg3 = fun i0 e ->
    error_msg
      (append
        ('['::('c'::('o'::('m'::('p'::('i'::('l'::('e'::('_'::('b'::('i'::('n'::('s'::('t'::('r'::(']'::(' '::('['::[]))))))))))))))))))
        (append i0 (append (']'::(':'::(' '::[]))) e)))
  in
  (match i with
   | BI_unreachable -> Ok ((instrs (Fail :: [])), s)
   | BI_nop -> Ok ((instrs (nop :: [])), s)
   | BI_drop ->
     Ok ([], (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
   | BI_select ->
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
     let s1 =
       push_scope []
         (sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
         s.current_scope
     in
     let s2 =
       push_scope []
         (sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
         (pop_scope s1 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
     in
     let sf = pop_scope s2 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let new_state0 = new_state s.regidx sf in
     let lbl_v1 = generate_label s1 Big_int_Z.zero_big_int in
     let lbl_v2 = generate_label s2 Big_int_Z.zero_big_int in
     Ok (((Br_Jnz (lbl_v1, c)) :: ((BInstr (Mov ((R res), (Inr (R
     v2))))) :: ((Br_Jmp lbl_v2) :: ((Label lbl_v1) :: ((BInstr (Mov ((R
     res), (Inr (R v1))))) :: ((Label lbl_v2) :: [])))))),
     (sub_reg new_state0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
       Big_int_Z.zero_big_int))))
   | BI_block (rt, body) ->
     let loop_state = enter_scope s rt nreg in
     let lbl_loop =
       generate_label loop_state.current_scope Big_int_Z.zero_big_int
     in
     mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat ->
       let (compiled_body, _) = pat in
       (match leave_scope loop_state with
        | Some new_state0 ->
          Ok ((app compiled_body ((Label lbl_loop) :: [])), new_state0)
        | None ->
          error_msg3
            ('B'::('I'::('_'::('b'::('l'::('o'::('c'::('k'::[]))))))))
            ('c'::('a'::('n'::('n'::('o'::('t'::(' '::('l'::('e'::('a'::('v'::('e'::(' '::('t'::('h'::('e'::(' '::('s'::('c'::('o'::('p'::('e'::('.'::[])))))))))))))))))))))))))
       (foldl (fun acc i0 ->
         mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat ->
           let (instrs_acc, state_acc) = pat in
           mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat0 ->
             let (instrs_comp, state_comp) = pat0 in
             Ok ((app instrs_acc instrs_comp), state_comp))
             (compile_binstr cP h module0 f_type i0 state_acc)) acc) (Ok ([],
         loop_state)) body)
   | BI_loop (rt, body) ->
     let loop_state = enter_scope s rt nreg in
     let lbl_loop =
       generate_label loop_state.current_scope Big_int_Z.zero_big_int
     in
     mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat ->
       let (compiled_body, _) = pat in
       (match leave_scope loop_state with
        | Some new_state0 ->
          Ok (((Label lbl_loop) :: compiled_body), new_state0)
        | None ->
          error_msg3 ('B'::('I'::('_'::('l'::('o'::('o'::('p'::[])))))))
            ('c'::('a'::('n'::('n'::('o'::('t'::(' '::('l'::('e'::('a'::('v'::('e'::(' '::('t'::('h'::('e'::(' '::('s'::('c'::('o'::('p'::('e'::('.'::[])))))))))))))))))))))))))
       (foldl (fun acc i0 ->
         mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat ->
           let (instrs_acc, state_acc) = pat in
           mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat0 ->
             let (instrs_comp, state_comp) = pat0 in
             Ok ((app instrs_acc instrs_comp), state_comp))
             (compile_binstr cP h module0 f_type i0 state_acc)) acc) (Ok ([],
         loop_state)) body)
   | BI_if (rt, body_true, body_false) ->
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
     mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat ->
       let (bodyF, pre_bodyT_state) = pat in
       let bodyT_state = { regidx =
         (sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int));
         current_scope = pre_bodyT_state.current_scope }
       in
       mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat0 ->
         let (bodyT, end_state) = pat0 in
         (match leave_scope end_state with
          | Some new_state0 ->
            Ok
              ((app ((Br_Jnz (lbl_true,
                 (sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: [])
                 (app bodyF
                   (app ((Br_Jmp lbl_end) :: ((Label lbl_true) :: []))
                     (app bodyT ((Label lbl_end) :: []))))), new_state0)
          | None ->
            error_msg3 ('B'::('I'::('_'::('i'::('f'::[])))))
              ('c'::('a'::('n'::('n'::('o'::('t'::(' '::('l'::('e'::('a'::('v'::('e'::(' '::('t'::('h'::('e'::(' '::('s'::('c'::('o'::('p'::('e'::[]))))))))))))))))))))))))
         (foldl (fun acc i0 ->
           mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat0 ->
             let (instrs_acc, state_acc) = pat0 in
             mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat1 ->
               let (instrs_comp, state_comp) = pat1 in
               Ok ((app instrs_acc instrs_comp), state_comp))
               (compile_binstr cP h module0 f_type i0 state_acc)) acc) (Ok
           ([], bodyT_state)) body_true))
       (foldl (fun acc i0 ->
         mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat ->
           let (instrs_acc, state_acc) = pat in
           mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat0 ->
             let (instrs_comp, state_comp) = pat0 in
             Ok ((app instrs_acc instrs_comp), state_comp))
             (compile_binstr cP h module0 f_type i0 state_acc)) acc) (Ok ([],
         bodyF_state)) body_false)
   | BI_br n0 ->
     let lbl = generate_label s.current_scope n0 in
     (match getn_scope s.current_scope n0 with
      | Some scopen ->
        let rt = scopen.scope_ret_type in
        (match rt with
         | [] -> Ok (((Br_Jmp lbl) :: []), s)
         | _ :: l ->
           (match l with
            | [] ->
              let rval =
                sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
              in
              let base0 = scopen.scope_reg_base in
              let code = (BInstr (Mov ((R base0), (Inr (R
                rval))))) :: ((Br_Jmp lbl) :: [])
              in
              Ok (code, s)
            | _ :: _ ->
              error_msg3 ('B'::('I'::('_'::('b'::('r'::[])))))
                ('W'::('a'::('s'::('m'::(' '::('1'::('.'::('0'::(' '::('d'::('i'::('s'::('a'::('l'::('l'::('o'::('w'::('s'::(' '::('m'::('o'::('r'::('e'::(' '::('t'::('h'::('a'::('n'::(' '::('o'::('n'::('e'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('v'::('a'::('l'::('u'::('e'::[])))))))))))))))))))))))))))))))))))))))))))))))
      | None ->
        error_msg3 ('B'::('I'::('_'::('b'::('r'::[])))))
          ('u'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('s'::('c'::('o'::('p'::('e'::[]))))))))))))))
   | BI_br_if n0 ->
     let lbl = generate_label s.current_scope n0 in
     (match getn_scope s.current_scope n0 with
      | Some scopen ->
        let rt = scopen.scope_ret_type in
        (match rt with
         | [] ->
           Ok (((Br_Jnz (lbl,
             (sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: []),
             (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
         | _ :: l ->
           (match l with
            | [] ->
              let rval =
                sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                  Big_int_Z.zero_big_int))
              in
              let base0 = scopen.scope_reg_base in
              let code = (BInstr (Mov ((R r_tmp), (Inr (R
                base0))))) :: ((BInstr (Mov ((R base0), (Inr (R
                rval))))) :: ((Br_Jnz (lbl,
                (sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((BInstr
                (Mov ((R base0), (Inr (R r_tmp))))) :: [])))
              in
              Ok (code,
              (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
            | _ :: _ ->
              error_msg3
                ('B'::('I'::('_'::('b'::('r'::('_'::('i'::('f'::[]))))))))
                ('W'::('a'::('s'::('m'::(' '::('1'::('.'::('0'::(' '::('d'::('i'::('s'::('a'::('l'::('l'::('o'::('w'::('s'::(' '::('m'::('o'::('r'::('e'::(' '::('t'::('h'::('a'::('n'::(' '::('o'::('n'::('e'::(' '::('r'::('e'::('t'::('u'::('r'::('n'::(' '::('v'::('a'::('l'::('u'::('e'::[])))))))))))))))))))))))))))))))))))))))))))))))
      | None ->
        error_msg3 ('B'::('I'::('_'::('b'::('r'::('_'::('i'::('f'::[]))))))))
          ('u'::('n'::('k'::('n'::('o'::('w'::('n'::(' '::('s'::('c'::('o'::('p'::('e'::[]))))))))))))))
   | BI_return ->
     let ret = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let tmp1 = add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let tmp2 =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))
     in
     let jaddr =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))
     in
     mbind (Obj.magic (fun _ _ -> error_bind)) (fun prepare_return_instrs ->
       Ok
       ((instrs
          (app prepare_return_instrs
            (app ((GetB ((R tmp1), r_stk0)) :: ((GetA ((R tmp2),
              r_stk0)) :: ((Sub ((R tmp1), (r tmp1), (r tmp2))) :: ((LoadU
              ((R jaddr), r_stk0, (r tmp1))) :: []))))
              (app
                (rclear_instrs
                  (list_difference reg_eq_dec all_registers (PC :: ((R
                    jaddr) :: (r_stk0 :: []))))) ((Jmp (R jaddr)) :: []))))),
       s)) (Obj.magic prologue_return f_type nreg ret r_tmp tmp2)
   | BI_call i0 ->
     (match get_function_type module0 i0 with
      | Some f ->
        let Tf (arg_type, ret_type) = f in
        let len_arg_type = length arg_type in
        let tmp_fun =
          add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int))))))
        in
        let tmp_data =
          add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))
        in
        let res = sub nreg len_arg_type in
        let args' = seq (sub nreg len_arg_type) len_arg_type in
        let args = map (fun x -> R x) args' in
        let offset_function = add module_frame.idx_imports_functions i0 in
        let prologue =
          instrs ((Mov ((R tmp_fun), (Inr r_frame))) :: ((Lea ((R tmp_fun),
            (Inl (Z.of_nat offset_function)))) :: ((Load ((R tmp_fun), (R
            tmp_fun))) :: ((Mov ((R tmp_data), (Inr (R tmp_fun)))) :: ((Load
            ((R tmp_fun), (R tmp_fun))) :: ((Lea ((R tmp_data), (Inl
            Big_int_Z.unit_big_int))) :: ((Load ((R tmp_data), (R
            tmp_data))) :: [])))))))
        in
        let new_state0 = sub_reg (add_reg s (length ret_type)) len_arg_type in
        call_template h (R tmp_fun) (R tmp_data) (R res) prologue args
          arg_type ret_type new_state0
      | None ->
        error_msg3 ('I'::('_'::('c'::('a'::('l'::('l'::[]))))))
          ('t'::('y'::('p'::('e'::(' '::('o'::('f'::(' '::('t'::('h'::('e'::(' '::('i'::('n'::('d'::('i'::('r'::('e'::('c'::('t'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))))))))
   | BI_call_indirect i0 ->
     (match get_type module0 i0 with
      | Some f ->
        let Tf (arg_type, ret_type) = f in
        let len_arg_type = length arg_type in
        let nfun = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
        let res = sub nfun len_arg_type in
        let tmp_jmp =
          add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int))))
        in
        let tmp_type =
          add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
        in
        let tmp_fun =
          add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int))))))
        in
        let tmp_data =
          add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))
        in
        let args' = seq (sub nfun len_arg_type) len_arg_type in
        let args = map (fun x -> R x) args' in
        let prologue =
          instrs ((Mov ((R tmp_fun), (Inr r_frame))) :: ((Lea ((R tmp_fun),
            (Inl (Z.of_nat module_frame.idx_imports_itable)))) :: ((Load ((R
            tmp_fun), (R tmp_fun))) :: ((Lea ((R tmp_fun),
            (r nfun))) :: ((Load ((R tmp_fun), (R tmp_fun))) :: ((Mov ((R
            tmp_data), (Inr (R tmp_fun)))) :: ((Mov ((R tmp_type), (Inr (R
            tmp_fun)))) :: ((Lea ((R tmp_type), (Inl
            (Big_int_Z.mult_int_big_int 2
            Big_int_Z.unit_big_int)))) :: ((Load ((R tmp_type), (R
            tmp_type))) :: ((Sub ((R tmp_type), (Inr (R tmp_type)), (Inl
            (cP.encode_function_type (Tf (arg_type, ret_type)))))) :: ((Mov
            ((R tmp_jmp), (Inr PC))) :: ((Lea ((R tmp_jmp), (Inl
            ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
            (Big_int_Z.mult_int_big_int 2
            Big_int_Z.unit_big_int))))) :: ((Jnz ((R tmp_jmp), (R
            tmp_type))) :: ((Lea ((R tmp_jmp), (Inl
            Big_int_Z.unit_big_int))) :: ((Jmp (R
            tmp_jmp)) :: (Fail :: ((Load ((R tmp_fun), (R tmp_fun))) :: ((Lea
            ((R tmp_data), (Inl Big_int_Z.unit_big_int))) :: ((Load ((R
            tmp_data), (R tmp_data))) :: [])))))))))))))))))))
        in
        let new_state0 =
          sub_reg (add_reg s (length ret_type))
            (add len_arg_type (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
        in
        call_template h (R tmp_fun) (R tmp_data) (R res) prologue args
          arg_type ret_type new_state0
      | None ->
        error_msg3
          ('B'::('I'::('_'::('c'::('a'::('l'::('l'::('_'::('i'::('n'::('d'::('i'::('r'::('e'::('c'::('t'::[]))))))))))))))))
          ('t'::('y'::('p'::('e'::(' '::('o'::('f'::(' '::('t'::('h'::('e'::(' '::('i'::('n'::('d'::('i'::('r'::('e'::('c'::('t'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))))))))
   | BI_get_local i0 ->
     let off = add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     Ok
     ((instrs ((GetB ((R r_tmp), r_stk0)) :: ((Add ((R r_tmp), (r r_tmp),
        (Inl (Z.of_nat i0)))) :: ((GetA ((R off), r_stk0)) :: ((Sub ((R off),
        (r r_tmp), (r off))) :: ((Add ((R off), (r off), (Inl
        local_offset))) :: ((LoadU ((R nreg), r_stk0, (r off))) :: []))))))),
     (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
   | BI_set_local i0 ->
     let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let off = add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     Ok
     ((instrs ((GetB ((R r_tmp), r_stk0)) :: ((Add ((R r_tmp), (r r_tmp),
        (Inl (Z.of_nat i0)))) :: ((GetA ((R off), r_stk0)) :: ((Sub ((R off),
        (r r_tmp), (r off))) :: ((Add ((R off), (r off), (Inl
        local_offset))) :: ((StoreU (r_stk0, (r off), (r v))) :: []))))))),
     (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
   | BI_tee_local i0 ->
     let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let off = add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     Ok
     ((instrs ((GetB ((R r_tmp), r_stk0)) :: ((Add ((R r_tmp), (r r_tmp),
        (Inl (Z.of_nat i0)))) :: ((GetA ((R off), r_stk0)) :: ((Sub ((R off),
        (r r_tmp), (r off))) :: ((Add ((R off), (r off), (Inl
        local_offset))) :: ((StoreU (r_stk0, (r off), (r v))) :: []))))))), s)
   | BI_get_global i0 ->
     let tmp_sentry =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))
     in
     let tmp0 =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     in
     let offset_linking_tbl = Z.of_nat module_frame.idx_linking_table in
     let offset_load_global = Z.of_nat offset_global_load in
     let offset_global = Z.of_nat (add module_frame.idx_imports_globals i0) in
     Ok
     ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
        tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
        offset_load_global))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
        Big_int_Z.zero_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)), (Inr r_frame))) :: ((Lea ((R
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
        offset_global))) :: ((Load ((R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))) :: ((Mov ((R Big_int_Z.zero_big_int), (Inr
        PC))) :: ((Lea ((R Big_int_Z.zero_big_int), (Inl
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int)))) :: ((Jmp (R tmp_sentry)) :: ((Mov ((R
        nreg), (Inr (R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int),
        (Inr (R tmp0)))) :: []))))))))))))))),
     (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
   | BI_set_global i0 ->
     let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let tmp_sentry =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     in
     let tmp0 =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
     in
     let offset_linking_tbl = Z.of_nat module_frame.idx_linking_table in
     let offset_store_global = Z.of_nat offset_global_store in
     let offset_global = Z.of_nat (add module_frame.idx_imports_globals i0) in
     Ok
     ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
        tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
        offset_store_global))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
        Big_int_Z.zero_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr (R
        v)))) :: ((Mov ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
        (Inr r_frame))) :: ((Lea ((R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)), (Inl offset_global))) :: ((Load ((R
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (R
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov ((R
        Big_int_Z.zero_big_int), (Inr PC))) :: ((Lea ((R
        Big_int_Z.zero_big_int), (Inl
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int)))) :: ((Jmp (R tmp_sentry)) :: ((Mov ((R
        Big_int_Z.zero_big_int), (Inr (R tmp0)))) :: []))))))))))))))),
     (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
   | BI_load _ ->
     let a = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let tmp_sentry =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     in
     let tmp0 =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))
     in
     let offset_linking_tbl = Z.of_nat module_frame.idx_linking_table in
     let offset_load_lin_mem = Z.of_nat offset_lin_mem_load in
     let offset_lin_mem =
       Z.of_nat (add module_frame.idx_imports_lin_mem Big_int_Z.zero_big_int)
     in
     Ok
     ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
        tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
        offset_load_lin_mem))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
        Big_int_Z.zero_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr (R
        a)))) :: ((Mov ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
        (Inr r_frame))) :: ((Lea ((R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)), (Inl offset_lin_mem))) :: ((Load ((R
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (R
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov ((R
        Big_int_Z.zero_big_int), (Inr PC))) :: ((Lea ((R
        Big_int_Z.zero_big_int), (Inl
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int)))) :: ((Jmp (R tmp_sentry)) :: ((Mov ((R
        res), (Inr (R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int),
        (Inr (R tmp0)))) :: [])))))))))))))))), s)
   | BI_store _ ->
     let a =
       sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))
     in
     let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let tmp_sentry =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))
     in
     let tmp0 =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))))
     in
     let offset_linking_tbl = Z.of_nat module_frame.idx_linking_table in
     let offset_store_lin_mem = Z.of_nat offset_lin_mem_store in
     let offset_lin_mem =
       Z.of_nat (add module_frame.idx_imports_lin_mem Big_int_Z.zero_big_int)
     in
     Ok
     ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
        tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
        offset_store_lin_mem))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
        Big_int_Z.zero_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))), (Inr (R v)))) :: ((Mov ((R
        (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))), (Inr (R a)))) :: ((Mov ((R
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inr
        r_frame))) :: ((Lea ((R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)), (Inl offset_lin_mem))) :: ((Load ((R
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (R
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov ((R
        Big_int_Z.zero_big_int), (Inr PC))) :: ((Lea ((R
        Big_int_Z.zero_big_int), (Inl
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int)))) :: ((Jmp (R tmp_sentry)) :: ((Mov ((R
        Big_int_Z.zero_big_int), (Inr (R tmp0)))) :: [])))))))))))))))),
     (sub_reg s (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
       Big_int_Z.zero_big_int))))
   | BI_segload _UU03c4_ ->
     let h0 = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     Ok
     ((instrs
        (app ((Load ((R res), (R h0))) :: [])
          (dyn_typecheck_instrs h (R res) _UU03c4_))), s)
   | BI_segstore _ ->
     let h0 =
       sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))
     in
     let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     Ok ((instrs ((Store ((R h0), (r v))) :: [])),
     (sub_reg s (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
       Big_int_Z.zero_big_int))))
   | BI_slice ->
     let h0 =
       sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     in
     let o1 =
       sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))
     in
     let o2 = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let tmp2 = add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let res =
       sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     in
     Ok
     ((instrs ((Lea ((R h0), (Inr (R o1)))) :: ((GetB ((R r_tmp), (R
        h0))) :: ((Add ((R o1), (r o1), (r r_tmp))) :: ((GetE ((R tmp2), (R
        h0))) :: ((Sub ((R o2), (r tmp2), (r o2))) :: ((Subseg ((R h0),
        (r o1), (r o2))) :: ((Mov ((R res), (r h0))) :: [])))))))),
     (sub_reg s (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
       Big_int_Z.zero_big_int))))
   | BI_segalloc ->
     let size = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let tmp_sentry =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     in
     let tmp0 =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
     in
     let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let offset_linking_tbl = Z.of_nat module_frame.idx_linking_table in
     let offset_malloc_safe_mem = Z.of_nat offset_safe_mem_malloc in
     let offset_safe_mem =
       Z.of_nat (add module_frame.idx_safe_mem Big_int_Z.zero_big_int)
     in
     Ok
     ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
        tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
        offset_malloc_safe_mem))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
        Big_int_Z.zero_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr (R
        size)))) :: ((Mov ((R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)), (Inr r_frame))) :: ((Lea ((R
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
        offset_safe_mem))) :: ((Load ((R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))) :: ((Mov ((R Big_int_Z.zero_big_int), (Inr
        PC))) :: ((Lea ((R Big_int_Z.zero_big_int), (Inl
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int)))) :: ((Jmp (R tmp_sentry)) :: ((Mov ((R
        res), (Inr (R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int),
        (Inr (R tmp0)))) :: [])))))))))))))))), s)
   | BI_handleadd ->
     let h0 = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let off =
       sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))
     in
     let res =
       sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))
     in
     Ok
     ((instrs ((Lea ((R h0), (r off))) :: ((Mov ((R res), (r h0))) :: []))),
     (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
   | BI_segfree ->
     error_msg3 ('_'::[])
       ('u'::('n'::('s'::('u'::('p'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('i'::('n'::('s'::('t'::('r'::('u'::('c'::('t'::('i'::('o'::('n'::[])))))))))))))))))))))))
   | BI_current_memory ->
     let tmp_sentry =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))
     in
     let tmp0 =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))
     in
     let offset_linking_tbl = Z.of_nat module_frame.idx_linking_table in
     let offset_current_lin_mem = Z.of_nat offset_lin_mem_current in
     let offset_lin_mem =
       Z.of_nat (add module_frame.idx_imports_lin_mem Big_int_Z.zero_big_int)
     in
     Ok
     ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
        tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
        offset_current_lin_mem))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
        Big_int_Z.zero_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)), (Inr r_frame))) :: ((Lea ((R
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
        offset_lin_mem))) :: ((Load ((R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))) :: ((Mov ((R Big_int_Z.zero_big_int), (Inr
        PC))) :: ((Lea ((R Big_int_Z.zero_big_int), (Inl
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int)))) :: ((Jmp (R tmp_sentry)) :: ((Mov ((R
        nreg), (Inr (R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int),
        (Inr (R tmp0)))) :: []))))))))))))))),
     (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
   | BI_grow_memory ->
     let size = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let tmp_sentry =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     in
     let tmp0 =
       add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))
     in
     let offset_linking_tbl = Z.of_nat module_frame.idx_linking_table in
     let offset_grow_lin_mem = Z.of_nat offset_lin_mem_grow in
     let offset_lin_mem =
       Z.of_nat (add module_frame.idx_imports_lin_mem Big_int_Z.zero_big_int)
     in
     Ok
     ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
        tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
        offset_grow_lin_mem))) :: ((Load ((R tmp_sentry), (R
        tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
        Big_int_Z.zero_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr (R
        size)))) :: ((Mov ((R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)), (Inr r_frame))) :: ((Lea ((R
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
        offset_lin_mem))) :: ((Load ((R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))) :: ((Mov ((R Big_int_Z.zero_big_int), (Inr
        PC))) :: ((Lea ((R Big_int_Z.zero_big_int), (Inl
        ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
        Big_int_Z.unit_big_int)))) :: ((Jmp (R tmp_sentry)) :: ((Mov ((R
        res), (Inr (R (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int),
        (Inr (R tmp0)))) :: [])))))))))))))))), s)
   | BI_const v ->
     mbind (Obj.magic (fun _ _ -> error_bind)) (fun val0 -> Ok
       ((instrs ((Mov ((R nreg), (Inl val0))) :: [])),
       (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))
       (match v with
        | Val_int z0 -> Ok (Obj.magic z0)
        | Val_handle _ ->
          error_msg3 ('I'::('_'::('c'::('o'::('n'::('s'::('t'::[])))))))
            ('e'::('x'::('p'::('e'::('c'::('t'::('s'::(' '::('a'::('n'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::[])))))))))))))))))))
   | BI_binop (_, op) ->
     let v1 =
       sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))
     in
     let v2 = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let res =
       sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))
     in
     Ok
     ((instrs
        (match op with
         | BOI_add -> (Add ((R res), (r v1), (r v2))) :: []
         | BOI_sub -> (Sub ((R res), (r v1), (r v2))) :: []
         | BOI_mul -> (Mul ((R res), (r v1), (r v2))) :: []
         | BOI_rem _ -> (Rem ((R res), (r v1), (r v2))) :: []
         | BOI_div _ -> (Div ((R res), (r v1), (r v2))) :: [])),
     (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
   | BI_testop _ ->
     let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     Ok ((instrs (eqz_instrs (R res) (R v) (R r_tmp))), s)
   | BI_relop (_, op) ->
     let v1 =
       sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))
     in
     let v2 = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
     let res =
       sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))
     in
     Ok
     ((instrs
        (match op with
         | ROI_eq -> eq_instrs (R res) (R v1) (R v2) (R r_tmp)
         | ROI_ne -> neq_instrs (R res) (R v1) (R v2) (R r_tmp)
         | ROI_lt _ -> (Lt0 ((R res), (Inr (R v1)), (Inr (R v2)))) :: []
         | ROI_gt _ -> (Lt0 ((R res), (Inr (R v2)), (Inr (R v1)))) :: []
         | ROI_le _ -> ge_instrs (R res) (R v2) (R v1) (R r_tmp)
         | ROI_ge _ -> ge_instrs (R res) (R v1) (R v2) (R r_tmp))),
     (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))

(** val compile_basic_instr :
    compilerParameters -> machineParameters -> ws_module -> typeidx ->
    ws_basic_instruction -> compilation_state -> cfg error **)

let compile_basic_instr cP h module0 f_typeidx i s =
  match get_type module0 f_typeidx with
  | Some f_type -> compile_binstr cP h module0 f_type i s
  | None ->
    error_msg
      ('['::('c'::('o'::('m'::('p'::('i'::('l'::('e'::('_'::('b'::('a'::('s'::('i'::('c'::('_'::('i'::('n'::('s'::('t'::('r'::(']'::(' '::('t'::('y'::('p'::('e'::(' '::('o'::('f'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[]))))))))))))))))))))))))))))))))))))))))))))))))

(** val labeled_compile_expr' :
    compilerParameters -> machineParameters -> ws_module -> typeidx ->
    ws_basic_instruction list -> compilation_state -> cfg error **)

let rec labeled_compile_expr' cP h module0 f_typeidx il s =
  match il with
  | [] -> Ok ([], s)
  | i :: il' ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat ->
      let (instrs_comp, s_comp) = pat in
      mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat0 ->
        let (instr_rec, state_rec) = pat0 in
        Ok ((app instrs_comp instr_rec), state_rec))
        (labeled_compile_expr' cP h module0 f_typeidx il' s_comp))
      (compile_basic_instr cP h module0 f_typeidx i s)

(** val labeled_compile_expr :
    compilerParameters -> machineParameters -> ws_module -> typeidx ->
    ws_basic_instruction list -> compilation_state -> cfg error **)

let labeled_compile_expr =
  labeled_compile_expr'

(** val load_args :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> result_type -> cerise_function **)

let rec load_args mP arg tmp tmp1 tmp2 tf =
  let rarg = R arg in
  let rtmp = R tmp in
  (match tf with
   | [] -> []
   | t0 :: tf' ->
     app ((LoadU (rtmp, rarg, (Inl (Big_int_Z.minus_big_int
       Big_int_Z.unit_big_int)))) :: ((Lea (rarg, (Inl
       (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: []))
       (app ((Mov ((R tmp1),
         (r (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: ((Mov ((R
         tmp2),
         (r (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
           Big_int_Z.zero_big_int))))) :: []))
         (app (dyn_typecheck_instrs mP rtmp t0)
           (app ((Mov ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
             (r tmp1))) :: ((Mov ((R (Big_int_Z.succ_big_int
             (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
             (r tmp2))) :: []))
             (app ((StoreU (r_stk0, (Inl Big_int_Z.zero_big_int), (Inr
               rtmp))) :: []) (load_args mP arg tmp tmp1 tmp2 tf'))))))

(** val prologue_function :
    machineParameters -> function_type -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> labeled_instr list **)

let prologue_function mP tf size_locals max_spilled_reg =
  let prepare_stack_pointer =
    let rec prepare_stack_pointer n0 =
      (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ -> [])
        (fun n' -> (StoreU (r_stk0, (Inl Big_int_Z.zero_big_int), (Inl
        Big_int_Z.zero_big_int))) :: (prepare_stack_pointer n'))
        n0
    in prepare_stack_pointer
  in
  let tmp = fun k ->
    add
      (add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) k
  in
  let Tf (rt, _) = tf in
  instrs
    (app
      (match rt with
       | [] -> []
       | _ :: _ ->
         (LoadU ((R (tmp Big_int_Z.zero_big_int)), r_stk0, (Inl
           (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: ((Lea
           (r_stk0, (Inl (Big_int_Z.minus_big_int
           Big_int_Z.unit_big_int)))) :: []))
      (app
        (load_args mP (tmp Big_int_Z.zero_big_int)
          (tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
          (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int)))
          (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) rt)
        (prepare_stack_pointer (add size_locals max_spilled_reg))))

(** val allocate_data :
    Big_int_Z.big_int -> Big_int_Z.big_int -> word list **)

let allocate_data size z0 =
  repeat (WInt z0) size

(** val compile_func :
    compilerParameters -> machineParameters -> module_func -> ws_module ->
    labeled_instr list error **)

let compile_func cP mP f m =
  let p = (f.modfunc_type, f.modfunc_locals) in
  let mf_body = f.modfunc_body in
  let (mf_type, mf_locals) = p in
  let init_state0 = { regidx = base_reg; current_scope = init_scope_state } in
  let mf_full_body = app mf_body (BI_return :: []) in
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun pat ->
    let (body, _) = pat in
    (match get_type m mf_type with
     | Some f_type ->
       let mAX_REG = Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))))))))))))))))))))))))))))
       in
       let max_spilled_reg = sub (max_reg body) mAX_REG in
       let prologue =
         prologue_function mP f_type (length mf_locals) max_spilled_reg
       in
       Ok (app prologue body)
     | None ->
       error_msg
         ('['::('c'::('o'::('m'::('p'::('i'::('l'::('e'::('_'::('f'::('u'::('n'::('c'::(']'::(' '::('t'::('y'::('p'::('e'::(' '::('o'::('f'::(' '::('t'::('h'::('e'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))))))))))))))))))))))))
    (Obj.magic labeled_compile_expr cP mP m mf_type mf_full_body init_state0)

(** val compile_funcs :
    compilerParameters -> machineParameters -> ws_module -> labeled_function
    list error **)

let compile_funcs cP mP m =
  foldl (fun acc_opt f ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun acc ->
      mbind (Obj.magic (fun _ _ -> error_bind)) (fun compiled_f -> Ok
        (compiled_f :: acc)) (Obj.magic compile_func cP mP f m)) acc_opt) (Ok
    []) m.mod_funcs

(** val compile_lin_mem :
    compilerParameters -> memory_type -> Big_int_Z.big_int -> abstract_word
    list **)

let compile_lin_mem cP m m_b =
  let size_min = mul m.lim_min cP.page_size in
  let size_max =
    mul
      (match m.lim_max with
       | Some n0 -> min n0 cP.max_size_linear_memory
       | None -> cP.max_size_linear_memory) cP.page_size
  in
  let l_m =
    add m_b (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))
  in
  let l_a = add l_m size_min in
  let m_e = add l_m size_max in
  let full_cap = WSealable (SCap ((RW, Global), l_m, (Finite m_e), l_a)) in
  let current_cap = WSealable (SCap ((RW, Global), l_m, (Finite l_a), l_m)) in
  let linear_memory = allocate_data size_max Big_int_Z.zero_big_int in
  map (fun x -> Inl x) (full_cap :: (current_cap :: linear_memory))

(** val compile_lin_mems :
    compilerParameters -> memory_type list -> Big_int_Z.big_int ->
    abstract_word list list **)

let compile_lin_mems cP llm offset_init =
  let (_, l) =
    foldl (fun acc m ->
      let (mb, l_lin_mem) = acc in
      let lin_mem = compile_lin_mem cP m mb in
      ((add mb (length lin_mem)), (lin_mem :: l_lin_mem))) (offset_init, [])
      llm
  in
  l

(** val compile_global : module_glob -> abstract_word list **)

let compile_global g =
  let gtype = g.modglob_type in
  let init_value =
    match gtype.tg_t with
    | T_int ->
      (match g.modglob_init with
       | Val_int z0 -> WInt z0
       | Val_handle _ -> WInt Big_int_Z.zero_big_int)
    | T_handle ->
      WSealable (SCap ((RW, Global), Big_int_Z.zero_big_int, (Finite
        Big_int_Z.zero_big_int), Big_int_Z.zero_big_int))
  in
  let type_val =
    match gtype.tg_t with
    | T_int -> WInt Big_int_Z.zero_big_int
    | T_handle -> WInt Big_int_Z.unit_big_int
  in
  let mut_val =
    match gtype.tg_mut with
    | MUT_immut -> WInt Big_int_Z.zero_big_int
    | MUT_mut -> WInt Big_int_Z.unit_big_int
  in
  map (fun x -> Inl x) (init_value :: (type_val :: (mut_val :: [])))

(** val compile_globals : module_glob list -> abstract_word list list **)

let compile_globals lg =
  map compile_global lg

(** val compile_indirect_table :
    compilerParameters -> module_table -> abstract_word list **)

let compile_indirect_table cP it =
  let nb_entry =
    match (tt_limits (modtab_type it)).lim_max with
    | Some n0 -> min n0 cP.max_size_indirection_table
    | None -> cP.max_size_indirection_table
  in
  let indirect_table =
    allocate_data nb_entry (Big_int_Z.mult_int_big_int 2
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      Big_int_Z.unit_big_int)))
  in
  map (fun x -> Inl x) indirect_table

(** val compile_indirect_tables :
    compilerParameters -> module_table list -> abstract_word list list **)

let compile_indirect_tables cP lit =
  map (fun it -> compile_indirect_table cP it) lit

(** val import_func_closure : module_import -> symbols option **)

let import_func_closure imp =
  let symbol = symbols_encode imp.imp_module imp.imp_name in
  (match imp.imp_desc with
   | ID_func _ -> Some symbol
   | _ -> None)

(** val imports_func_closures : module_import list -> symbols list **)

let rec imports_func_closures = function
| [] -> []
| imp :: imps' ->
  let acc = imports_func_closures imps' in
  (match import_func_closure imp with
   | Some s -> app (s :: []) acc
   | None -> acc)

(** val import_lin_mem : module_import -> symbols option **)

let import_lin_mem imp =
  let symbol = symbols_encode imp.imp_module imp.imp_name in
  (match imp.imp_desc with
   | ID_mem _ -> Some symbol
   | _ -> None)

(** val imports_lin_mems : module_import list -> symbols list **)

let rec imports_lin_mems = function
| [] -> []
| imp :: imps' ->
  let acc = imports_lin_mems imps' in
  (match import_lin_mem imp with
   | Some s -> s :: acc
   | None -> acc)

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
  let acc = imports_globals imps' in
  (match import_global imp with
   | Some s -> app (s :: []) acc
   | None -> acc)

(** val import_itable : module_import -> symbols option **)

let import_itable imp =
  let symbol = symbols_encode imp.imp_module imp.imp_name in
  (match imp.imp_desc with
   | ID_table _ -> Some symbol
   | _ -> None)

(** val imports_itables : module_import list -> symbols list **)

let rec imports_itables = function
| [] -> []
| imp :: imps' ->
  let acc = imports_itables imps' in
  (match import_itable imp with
   | Some s -> app (s :: []) acc
   | None -> acc)

(** val compile_frame :
    compilerParameters -> ws_module -> Big_int_Z.big_int ->
    ((word * word) * word) list -> abstract_word list list -> abstract_word
    list list -> abstract_word list list -> abstract_word list error **)

let compile_frame cP m offset_datas func_closures lin_mems globals itables =
  let imported_func_closures =
    map (fun x -> Inr x) (imports_func_closures m.mod_imports)
  in
  let imported_lin_mems =
    map (fun x -> Inr x) (imports_lin_mems m.mod_imports)
  in
  let imported_globals = map (fun x -> Inr x) (imports_globals m.mod_imports)
  in
  let imported_itables = map (fun x -> Inr x) (imports_itables m.mod_imports)
  in
  let commons =
    map (fun x -> Inr x)
      (cP.common_safe_memory_symbol :: (cP.common_linking_table_symbol :: []))
  in
  let defined_func_closures =
    let (_, func_closures0) =
      foldl (fun acc _ ->
        let (offset0, func_closures0) = acc in
        let len_fc = Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
        in
        let e = add offset0 len_fc in
        let func_closure = WSealable (SCap ((RO, Global), offset0, (Finite
          e), offset0))
        in
        (e, (app func_closures0 (func_closure :: [])))) (offset_datas, [])
        func_closures
    in
    map (fun x -> Inl x) func_closures0
  in
  let offset_lin_mems =
    add offset_datas
      (mul (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
        (length func_closures))
  in
  let defined_lin_mems =
    let (_, sealeds) =
      foldl (fun acc m0 ->
        let (offset0, sealed_lin_mems) = acc in
        let e = add offset0 (length m0) in
        let sealed_lin_mem = WSealed (cP.otype_lin_mem, (SCap ((RW, Global),
          offset0, (Finite e), offset0)))
        in
        (e, (app sealed_lin_mems (sealed_lin_mem :: [])))) (offset_lin_mems,
        []) lin_mems
    in
    map (fun x -> Inl x) sealeds
  in
  let offset_globals = add offset_lin_mems (length (concat lin_mems)) in
  let defined_globals =
    let (_, sealeds) =
      foldl (fun acc g ->
        let (offset0, sealed_globals) = acc in
        let e = add offset0 (length g) in
        let sealed_global = WSealed (cP.otype_global, (SCap ((RW, Global),
          offset0, (Finite e), offset0)))
        in
        (e, (app sealed_globals (sealed_global :: [])))) (offset_globals, [])
        globals
    in
    map (fun x -> Inl x) sealeds
  in
  let offset_itables = add offset_globals (length (concat globals)) in
  let defined_itables =
    let (_, ro_itables) =
      foldl (fun acc it ->
        let (offset0, ro_itables) = acc in
        let e = add offset0 (length it) in
        let ro_itable = WSealable (SCap ((RO, Global), offset0, (Finite e),
          offset0))
        in
        (e, (app ro_itables (ro_itable :: [])))) (offset_itables, []) itables
    in
    map (fun x -> Inl x) ro_itables
  in
  Ok
  (app imported_func_closures
    (app defined_func_closures
      (app imported_lin_mems
        (app defined_lin_mems
          (app imported_globals
            (app defined_globals
              (app imported_itables (app defined_itables commons))))))))

(** val compile_export :
    module_export -> frame -> char list -> (symbols, section_offset) gmap **)

let compile_export exp frm module_name =
  let s = symbols_encode module_name exp.modexp_name in
  (match exp.modexp_desc with
   | MED_func f ->
     singletonM0
       (map_singleton (gmap_partial_alter string_eq_dec string_countable)
         (gmap_empty string_eq_dec string_countable)) s (Data, f)
   | MED_table t0 ->
     singletonM0
       (map_singleton (gmap_partial_alter string_eq_dec string_countable)
         (gmap_empty string_eq_dec string_countable)) s (Data,
       (add frm.idx_imports_itable t0))
   | MED_mem m ->
     singletonM0
       (map_singleton (gmap_partial_alter string_eq_dec string_countable)
         (gmap_empty string_eq_dec string_countable)) s (Data,
       (add frm.idx_imports_lin_mem m))
   | MED_global g ->
     singletonM0
       (map_singleton (gmap_partial_alter string_eq_dec string_countable)
         (gmap_empty string_eq_dec string_countable)) s (Data,
       (add frm.idx_imports_globals g)))

(** val compile_exports :
    module_export list -> frame -> name -> (symbols, section_offset) gmap **)

let rec compile_exports exps frm module_name =
  match exps with
  | [] -> gmap_empty string_eq_dec string_countable
  | exp :: exps' ->
    union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge string_eq_dec string_countable)))
      (compile_export exp frm module_name)
      (compile_exports exps' frm module_name)

(** val get_start : ws_module -> section_offset option **)

let get_start m =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun mstart -> Some (Data,
    (modstart_func mstart))) (Obj.magic m.mod_start)

(** val prepare_function_closures :
    compilerParameters -> module_func list -> ws_module ->
    ((word * word) * word) list error **)

let rec prepare_function_closures cP func_mods m =
  match func_mods with
  | [] -> Ok []
  | f :: fs ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun acc ->
      let placeholder = WSealed (cP.otype_module, (SCap ((O, Global),
        Big_int_Z.zero_big_int, (Finite Big_int_Z.zero_big_int),
        Big_int_Z.zero_big_int)))
      in
      (match get_type m f.modfunc_type with
       | Some t0 ->
         Ok (((placeholder, placeholder), (WInt
           (cP.encode_function_type t0))) :: acc)
       | None ->
         error_msg
           ('['::('p'::('r'::('e'::('p'::('a'::('r'::('e'::('_'::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::('_'::('c'::('l'::('o'::('s'::('u'::('r'::('e'::('s'::(']'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('t'::('y'::('p'::('e'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::('.'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))
      (prepare_function_closures cP fs m)

(** val compile_func_closures :
    compilerParameters -> addr -> addr -> labeled_function list ->
    module_func list -> ws_module -> ((word * word) * word) list error **)

let compile_func_closures cP frm_b frm_e compiled_functions func_mods m =
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun func_closures ->
    compute_func_closure frm_b frm_e compiled_functions func_closures)
    (prepare_function_closures cP func_mods m)

(** val compile_import :
    compilerParameters -> module_import -> ws_module ->
    (symbols * Big_int_Z.big_int) error **)

let compile_import cP imp m =
  let symbol = symbols_encode imp.imp_module imp.imp_name in
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun t0 -> Ok (symbol, t0))
    (match imp.imp_desc with
     | ID_func tidx ->
       (match get_type m tidx with
        | Some ft -> Ok (Obj.magic cP.encode_extern_t (ET_func ft))
        | None ->
          error_msg
            ('['::('c'::('o'::('m'::('p'::('i'::('l'::('e'::('_'::('i'::('m'::('p'::('o'::('r'::('t'::(']'::(' '::('t'::('y'::('p'::('e'::(' '::('i'::('n'::('d'::('e'::('x'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::('.'::[])))))))))))))))))))))))))))))))))))))))
     | ID_table tbl_t -> Ok (Obj.magic cP.encode_extern_t (ET_tab tbl_t))
     | ID_mem mem_t -> Ok (Obj.magic cP.encode_extern_t (ET_mem mem_t))
     | ID_global glob_t -> Ok (Obj.magic cP.encode_extern_t (ET_glob glob_t)))

(** val compile_imports :
    compilerParameters -> ws_module -> (symbols, Big_int_Z.big_int) gmap error **)

let compile_imports cP m =
  foldl (fun er_acc imp ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun acc ->
      mbind (Obj.magic (fun _ _ -> error_bind)) (fun imp_entry -> Ok
        (insert0
          (map_insert (gmap_partial_alter string_eq_dec string_countable))
          (fst imp_entry) (snd imp_entry) acc))
        (Obj.magic compile_import cP imp m)) er_acc) (Ok
    (gmap_empty string_eq_dec string_countable)) m.mod_imports

type itableType = abstract_word list

type initType = (symbols, (Big_int_Z.big_int * word) list) gmap

(** val update_table :
    itableType -> Big_int_Z.big_int -> word -> itableType error **)

let update_table itable offset_entry entry_word =
  match update_nth_error itable offset_entry (Inl entry_word) with
  | Some _ -> Ok itable
  | None ->
    error_msg
      ('['::('u'::('p'::('d'::('a'::('t'::('e'::('_'::('t'::('a'::('b'::('l'::('e'::(']'::(' '::('e'::('r'::('r'::('o'::('r'::(' '::('u'::('p'::('d'::('a'::('t'::('e'::(' '::('i'::('t'::('a'::('b'::('l'::('e'::[]))))))))))))))))))))))))))))))))))

(** val update_tables :
    itableType list -> Big_int_Z.big_int -> Big_int_Z.big_int -> word list ->
    itableType list error **)

let rec update_tables itables n_tbl offset_entry = function
| [] -> Ok itables
| w :: entry_words' ->
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun itables' ->
    match nth_error itables' n_tbl with
    | Some itable ->
      mbind (Obj.magic (fun _ _ -> error_bind)) (fun itable' ->
        match update_nth_error itables' n_tbl itable' with
        | Some itables'' -> Ok itables''
        | None ->
          error_msg
            ('['::('u'::('p'::('d'::('a'::('t'::('e'::('_'::('t'::('a'::('b'::('l'::('e'::('s'::(']'::(' '::('e'::('r'::('r'::('o'::('r'::(' '::('u'::('p'::('d'::('a'::('t'::('e'::(' '::('i'::('t'::('a'::('b'::('l'::('e'::('s'::[])))))))))))))))))))))))))))))))))))))
        (Obj.magic update_table itable offset_entry w)
    | None ->
      error_msg
        ('['::('u'::('p'::('d'::('a'::('t'::('e'::('_'::('t'::('a'::('b'::('l'::('e'::('s'::(']'::(' '::('t'::('a'::('b'::('l'::('e'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[]))))))))))))))))))))))))))))))))
    (update_tables itables n_tbl
      (add offset_entry (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
      entry_words')

(** val imported_entries :
    symbols -> Big_int_Z.big_int -> word list ->
    ((symbols * Big_int_Z.big_int) * word) list **)

let rec imported_entries sym offset_entry = function
| [] -> []
| w :: entry_words' ->
  ((sym, offset_entry),
    w) :: (imported_entries sym
            (add offset_entry (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
            entry_words')

(** val init_entries :
    funcidx list -> abstract_word list -> word list error **)

let rec init_entries fidxs frame_data =
  match fidxs with
  | [] -> Ok []
  | f :: fidxs' ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun words ->
      mbind (Obj.magic (fun _ _ -> error_bind)) (fun word0 -> Ok
        (word0 :: words))
        (match nth_error frame_data f with
         | Some a ->
           (match a with
            | Inl w -> Ok (Obj.magic w)
            | Inr _ ->
              error_msg
                ('['::('i'::('n'::('i'::('t'::('_'::('e'::('n'::('t'::('r'::('i'::('e'::('s'::(']'::(' '::('i'::('n'::('i'::('t'::(' '::('e'::('l'::('e'::('m'::(' '::('e'::('n'::('t'::('r'::('y'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('e'::(' '::('a'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::[]))))))))))))))))))))))))))))))))))))))))))))))))))
         | None ->
           error_msg
             ('['::('i'::('n'::('i'::('t'::('_'::('e'::('n'::('t'::('r'::('i'::('e'::('s'::(']'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))))))))))))))))))))
      (init_entries fidxs' frame_data)

(** val compile_element :
    module_element -> itableType list -> abstract_word list ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> (itableType list,
    ((symbols * Big_int_Z.big_int) * word) list) sum error **)

let compile_element element itables frame_data idx_imports_itables idx_defined_itables =
  let tidx = element.modelem_table in
  let offset_entry = element.modelem_offset in
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun entry_words ->
    match nth_error frame_data (add idx_imports_itables tidx) with
    | Some a ->
      (match a with
       | Inl _ ->
         let size_imports_itables =
           sub idx_defined_itables idx_imports_itables
         in
         let n_tbl = sub tidx size_imports_itables in
         mbind (Obj.magic (fun _ _ -> error_bind)) (fun upd_tbl -> Ok (Inl
           upd_tbl))
           (Obj.magic update_tables itables n_tbl offset_entry entry_words)
       | Inr s -> Ok (Inr (imported_entries s offset_entry entry_words)))
    | None ->
      error_msg
        ('['::('c'::('o'::('m'::('p'::('i'::('l'::('e'::('_'::('e'::('l'::('e'::('m'::('e'::('n'::('t'::(']'::(' '::('i'::('t'::('a'::('b'::('l'::('e'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::(' '::('i'::('n'::(' '::('t'::('h'::('e'::(' '::('f'::('r'::('a'::('m'::('e'::[]))))))))))))))))))))))))))))))))))))))))))))))))
    (Obj.magic init_entries element.modelem_init frame_data)

(** val update_inits :
    initType -> symbols -> Big_int_Z.big_int -> word -> initType **)

let update_inits inits sym off w =
  let new_elem = (off, w) in
  (match lookup0 (gmap_lookup string_eq_dec string_countable) sym inits with
   | Some l ->
     insert0 (map_insert (gmap_partial_alter string_eq_dec string_countable))
       sym (new_elem :: l) inits
   | None ->
     insert0 (map_insert (gmap_partial_alter string_eq_dec string_countable))
       sym (new_elem :: []) inits)

(** val update_linits :
    initType -> ((symbols * Big_int_Z.big_int) * word) list -> initType **)

let rec update_linits inits = function
| [] -> inits
| p :: linits' ->
  let (p0, w) = p in
  let (sym, off) = p0 in
  let inits' = update_inits inits sym off w in update_linits inits' linits'

(** val compile_elements :
    module_element list -> itableType list -> abstract_word list ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> (itableType list * initType)
    error **)

let rec compile_elements elements itables frame_data idx_imports_itables idx_defined_itables =
  match elements with
  | [] -> Ok (itables, (gmap_empty string_eq_dec string_countable))
  | e :: elements' ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun compiled_elems ->
      let (itables', inits) = compiled_elems in
      mbind (Obj.magic (fun _ _ -> error_bind)) (fun compiled_elem ->
        match compiled_elem with
        | Inl itbl -> Ok (itbl, inits)
        | Inr linits -> Ok (itables', (update_linits inits linits)))
        (Obj.magic compile_element e itables' frame_data idx_imports_itables
          idx_defined_itables))
      (compile_elements elements' itables frame_data idx_imports_itables
        idx_defined_itables)

(** val compile_module :
    compilerParameters -> machineParameters -> ws_module -> name ->
    labeled_cerise_component error **)

let compile_module cP mP m module_name =
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun compiled_functions ->
    let frm = define_module_frame m in
    let lin_mem_section = compile_lin_mems cP m.mod_mems in
    let globals_section = compile_globals m.mod_globals in
    let uninit_itables = compile_indirect_tables cP m.mod_tables in
    let exports = compile_exports m.mod_exports frm module_name in
    let b_frm = length (concat compiled_functions) in
    let e_frm = add b_frm (len_frame m) in
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun func_closure_section ->
      mbind (Obj.magic (fun _ _ -> error_bind)) (fun data_frame ->
        mbind (Obj.magic (fun _ _ -> error_bind)) (fun elems ->
          let (itable_section, inits) = elems in
          mbind (Obj.magic (fun _ _ -> error_bind)) (fun imports ->
            let len_fc = Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
              (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
            in
            let offset_lin_mem_section =
              add e_frm (mul len_fc (length func_closure_section))
            in
            let data_section =
              app (concat (lin_mem_section offset_lin_mem_section))
                (app (concat globals_section) (concat itable_section))
            in
            Ok { l_code = compiled_functions; l_data = { l_data_frame =
            data_frame; l_data_func_closures = func_closure_section;
            l_data_section = data_section }; l_main = (get_start m);
            l_exports = exports; l_imports = imports; l_init = inits })
            (Obj.magic compile_imports cP m))
          (Obj.magic compile_elements m.mod_elem uninit_itables data_frame
            frm.idx_imports_itable frm.idx_defined_itable))
        (Obj.magic compile_frame cP m e_frm func_closure_section
          (lin_mem_section Big_int_Z.zero_big_int) globals_section
          uninit_itables))
      (Obj.magic compile_func_closures cP b_frm e_frm compiled_functions
        m.mod_funcs m)) (Obj.magic compile_funcs cP mP m)

(** val error_msg0 : char list -> 'a1 error **)

let error_msg0 s =
  Error
    (append
      ('['::('c'::('e'::('r'::('i'::('s'::('e'::('_'::('g'::('e'::('n'::(']'::(' '::[])))))))))))))
      s)

(** val addresses_labels' :
    labeled_instr list -> Big_int_Z.big_int -> (label, Big_int_Z.big_int) gmap **)

let rec addresses_labels' il addr0 =
  match il with
  | [] ->
    gmap_empty (list_eq_dec0 Coq0_Nat.eq_dec)
      (list_countable Coq0_Nat.eq_dec nat_countable)
  | l :: il' ->
    (match l with
     | Label lbl ->
       insert0
         (map_insert
           (gmap_partial_alter (list_eq_dec0 Coq0_Nat.eq_dec)
             (list_countable Coq0_Nat.eq_dec nat_countable))) lbl addr0
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
    Big_int_Z.big_int -> cerise_function error **)

let rec branch_labels' il label_map addr0 =
  match il with
  | [] -> Ok []
  | l :: il' ->
    (match l with
     | Label _ -> branch_labels' il' label_map addr0
     | BInstr i ->
       mbind (Obj.magic (fun _ _ -> error_bind)) (fun next -> Ok (i :: next))
         (branch_labels' il' label_map
           (add addr0 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
     | Br_Jmp lbl ->
       (match lookup0
                (gmap_lookup (list_eq_dec0 Coq0_Nat.eq_dec)
                  (list_countable Coq0_Nat.eq_dec nat_countable)) lbl
                label_map with
        | Some o ->
          mbind (Obj.magic (fun _ _ -> error_bind)) (fun next ->
            let off = Z.sub (Z.of_nat o) (Z.of_nat addr0) in
            Ok
            (app ((Mov ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
              (Inr PC))) :: ((Lea ((R (Big_int_Z.succ_big_int
              Big_int_Z.zero_big_int)), (Inl off))) :: ((Jmp (R
              (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) :: []))) next))
            (branch_labels' il' label_map
              (add addr0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
        | None ->
          error_msg0
            ('['::('b'::('r'::('a'::('n'::('c'::('h'::('_'::('l'::('a'::('b'::('e'::('l'::('s'::('\''::(']'::(' '::('['::('B'::('r'::('_'::('J'::('m'::('p'::(']'::(' '::('l'::('a'::('b'::('e'::('l'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[]))))))))))))))))))))))))))))))))))))))))))
     | Br_Jnz (lbl, reg_cond) ->
       (match lookup0
                (gmap_lookup (list_eq_dec0 Coq0_Nat.eq_dec)
                  (list_countable Coq0_Nat.eq_dec nat_countable)) lbl
                label_map with
        | Some o ->
          mbind (Obj.magic (fun _ _ -> error_bind)) (fun next ->
            let off = Z.sub (Z.of_nat o) (Z.of_nat addr0) in
            Ok
            (app ((Mov ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
              (Inr PC))) :: ((Lea ((R (Big_int_Z.succ_big_int
              Big_int_Z.zero_big_int)), (Inl off))) :: ((Jnz ((R
              (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (R
              reg_cond))) :: []))) next))
            (branch_labels' il' label_map
              (add addr0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))
        | None ->
          error_msg0
            ('['::('b'::('r'::('a'::('n'::('c'::('h'::('_'::('l'::('a'::('b'::('e'::('l'::('s'::('\''::(']'::(' '::('['::('B'::('r'::('_'::('J'::('n'::('z'::(']'::(' '::('l'::('a'::('b'::('e'::('l'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))))))))))))))))))))

(** val branch_labels : labeled_instr list -> cerise_function error **)

let branch_labels il =
  branch_labels' il (addresses_labels il) Big_int_Z.zero_big_int

(** val compile_functions :
    labeled_function list -> cerise_function list error **)

let compile_functions progs =
  foldl (fun acc_opt p ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun acc ->
      mbind (Obj.magic (fun _ _ -> error_bind)) (fun compiled_prog -> Ok
        (compiled_prog :: acc)) (Obj.magic branch_labels p)) acc_opt) (Ok [])
    progs

(** val compile_component :
    machineParameters -> labeled_cerise_component -> cerise_linkable_object
    error **)

let compile_component mP m =
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun lbl_erased_code ->
    let reloc_shift =
      sub (length (concat lbl_erased_code)) (length (concat m.l_code))
    in
    let relocate = fun data n0 ->
      map (fun w ->
        match w with
        | Inl w0 -> Inl (shift_word w0 n0)
        | Inr s -> Inr s) data
    in
    let b_frm = length (concat lbl_erased_code) in
    let e_frm = add b_frm (length m.l_data.l_data_frame) in
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun func_closures ->
      let relocated_data =
        app (relocate m.l_data.l_data_frame reloc_shift)
          (app
            (concat
              (map (fun s ->
                let (p, sd) = s in
                let (se, t0) = p in (Inl se) :: ((Inl t0) :: ((Inl sd) :: [])))
                func_closures))
            (relocate m.l_data.l_data_section reloc_shift))
      in
      Ok { c_code =
      (map (fun x -> Inl x) (encodeInstrsW mP (concat lbl_erased_code)));
      c_data = relocated_data; c_main = m.l_main; c_exports = m.l_exports;
      c_imports = m.l_imports; c_init = m.l_init })
      (Obj.magic compute_func_closure b_frm e_frm lbl_erased_code
        m.l_data.l_data_func_closures)) (Obj.magic compile_functions m.l_code)

(** val get_spilling_offset :
    Big_int_Z.big_int -> Big_int_Z.big_int option **)

let get_spilling_offset reg_num =
  let mAX_REG = Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
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
    Big_int_Z.zero_big_int))))))))))))))))))))))))))))))
  in
  if Coq_Nat.leb reg_num mAX_REG
  then None
  else Some (Z.sub (Z.of_nat mAX_REG) (Z.of_nat reg_num))

(** val load_spilled :
    Big_int_Z.big_int -> Big_int_Z.big_int ->
    cerise_function * Big_int_Z.big_int **)

let load_spilled reg_num tmp =
  match get_spilling_offset reg_num with
  | Some offset0 -> (((LoadU ((R tmp), r_stk0, (Inl offset0))) :: []), tmp)
  | None -> ([], reg_num)

(** val load_spilled_reg :
    regName -> Big_int_Z.big_int -> cerise_function * regName **)

let load_spilled_reg reg tmp =
  match reg with
  | R n0 -> let (li, n') = load_spilled n0 tmp in (li, (R n'))
  | _ -> ([], reg)

(** val load_spilled_zreg :
    (Big_int_Z.big_int, regName) sum -> Big_int_Z.big_int ->
    cerise_function * (Big_int_Z.big_int, regName) sum **)

let load_spilled_zreg zreg tmp =
  match zreg with
  | Inl z0 -> ([], (Inl z0))
  | Inr r0 -> let (li, r') = load_spilled_reg r0 tmp in (li, (Inr r'))

(** val store_spilled_reg :
    regName -> regName -> cerise_function * regName **)

let store_spilled_reg dst tmp =
  match dst with
  | R n0 ->
    (match get_spilling_offset n0 with
     | Some offset0 ->
       (((StoreU (r_stk0, (Inl offset0), (Inr tmp))) :: []), tmp)
     | None -> ([], (R n0)))
  | _ -> ([], dst)

(** val spill_instruction : cerise_instruction -> cerise_function **)

let spill_instruction = function
| Jmp src ->
  let (li, src') = load_spilled_reg src r_tmp in app li ((Jmp src') :: [])
| Jnz (src1, src2) ->
  let (li1, src1') = load_spilled_reg src1 r_tmp in
  let (li2, src2') =
    load_spilled_reg src2
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  app li1 (app li2 ((Jnz (src1', src2')) :: []))
| Mov (dst, src) ->
  let (li_src, src') = load_spilled_zreg src r_tmp in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
  in
  app li_src (app ((Mov (dst', src')) :: []) li_dst)
| Load (dst, src) ->
  let (li_src, src') = load_spilled_reg src r_tmp in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
  in
  app li_src (app ((Load (dst', src')) :: []) li_dst)
| Store (src1, src2) ->
  let (li_src1, src1') = load_spilled_reg src1 r_tmp in
  let (li_src2, src2') =
    load_spilled_zreg src2
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  app li_src1 (app li_src2 ((Store (src1', src2')) :: []))
| Lt0 (dst, src1, src2) ->
  let (li_src1, src1') = load_spilled_zreg src1 r_tmp in
  let (li_src2, src2') =
    load_spilled_zreg src2
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))
  in
  app li_src1 (app li_src2 (app ((Lt0 (dst', src1', src2')) :: []) li_dst))
| Add (dst, src1, src2) ->
  let (li_src1, src1') = load_spilled_zreg src1 r_tmp in
  let (li_src2, src2') =
    load_spilled_zreg src2
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))
  in
  app li_src1 (app li_src2 (app ((Add (dst', src1', src2')) :: []) li_dst))
| Sub (dst, src1, src2) ->
  let (li_src1, src1') = load_spilled_zreg src1 r_tmp in
  let (li_src2, src2') =
    load_spilled_zreg src2
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))
  in
  app li_src1 (app li_src2 (app ((Sub (dst', src1', src2')) :: []) li_dst))
| Mul (dst, src1, src2) ->
  let (li_src1, src1') = load_spilled_zreg src1 r_tmp in
  let (li_src2, src2') =
    load_spilled_zreg src2
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))
  in
  app li_src1 (app li_src2 (app ((Mul (dst', src1', src2')) :: []) li_dst))
| Rem (dst, src1, src2) ->
  let (li_src1, src1') = load_spilled_zreg src1 r_tmp in
  let (li_src2, src2') =
    load_spilled_zreg src2
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))
  in
  app li_src1 (app li_src2 (app ((Rem (dst', src1', src2')) :: []) li_dst))
| Div (dst, src1, src2) ->
  let (li_src1, src1') = load_spilled_zreg src1 r_tmp in
  let (li_src2, src2') =
    load_spilled_zreg src2
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))
  in
  app li_src1 (app li_src2 (app ((Div (dst', src1', src2')) :: []) li_dst))
| Lea (dst, src1) ->
  let (li_src0, src0') = load_spilled_reg dst r_tmp in
  let (li_src1, src1') =
    load_spilled_zreg src1
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let (li_dst, dst') = store_spilled_reg dst src0' in
  app li_src0 (app li_src1 (app ((Lea (dst', src1')) :: []) li_dst))
| Restrict (dst, src1) ->
  let (li_src0, src0') = load_spilled_reg dst r_tmp in
  let (li_src1, src1') =
    load_spilled_zreg src1
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let (li_dst, dst') = store_spilled_reg dst src0' in
  app li_src0 (app li_src1 (app ((Restrict (dst', src1')) :: []) li_dst))
| Subseg (dst, src1, src2) ->
  let (li_src0, src0') = load_spilled_reg dst r_tmp in
  let (li_src1, src1') =
    load_spilled_zreg src1
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let (li_src2, src2') =
    load_spilled_zreg src2
      (add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))
  in
  let (li_dst, dst') = store_spilled_reg dst src0' in
  app li_src0
    (app li_src1
      (app li_src2 (app ((Subseg (dst', src1', src2')) :: []) li_dst)))
| GetL (dst, src) ->
  let (li_src, src') = load_spilled_reg src r_tmp in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
  in
  app li_src (app ((GetL (dst', src')) :: []) li_dst)
| GetB (dst, src) ->
  let (li_src, src') = load_spilled_reg src r_tmp in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
  in
  app li_src (app ((GetB (dst', src')) :: []) li_dst)
| GetE (dst, src) ->
  let (li_src, src') = load_spilled_reg src r_tmp in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
  in
  app li_src (app ((GetE (dst', src')) :: []) li_dst)
| GetA (dst, src) ->
  let (li_src, src') = load_spilled_reg src r_tmp in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
  in
  app li_src (app ((GetA (dst', src')) :: []) li_dst)
| GetP (dst, src) ->
  let (li_src, src') = load_spilled_reg src r_tmp in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
  in
  app li_src (app ((GetP (dst', src')) :: []) li_dst)
| GetWType (dst, src) ->
  let (li_src, src') = load_spilled_reg src r_tmp in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
  in
  app li_src (app ((GetWType (dst', src')) :: []) li_dst)
| GetOType (dst, src) ->
  let (li_src, src') = load_spilled_reg src r_tmp in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
  in
  app li_src (app ((GetOType (dst', src')) :: []) li_dst)
| Seal (dst, src1, src2) ->
  let (li_src1, src1') = load_spilled_reg src1 r_tmp in
  let (li_src2, src2') =
    load_spilled_reg src2
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))
  in
  app li_src1 (app li_src2 (app ((Seal (dst', src1', src2')) :: []) li_dst))
| UnSeal (dst, src1, src2) ->
  let (li_src1, src1') = load_spilled_reg src1 r_tmp in
  let (li_src2, src2') =
    load_spilled_reg src2
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))
  in
  app li_src1 (app li_src2 (app ((UnSeal (dst', src1', src2')) :: []) li_dst))
| Invoke (src1, src2) ->
  let (li1, src1') = load_spilled_reg src1 r_tmp in
  let (li2, src2') =
    load_spilled_reg src2
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  app li1 (app li2 ((Invoke (src1', src2')) :: []))
| LoadU (dst, src1, src2) ->
  let (li_src1, src1') = load_spilled_reg src1 r_tmp in
  let (li_src2, src2') =
    load_spilled_zreg src2
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let (li_dst, dst') =
    store_spilled_reg dst (R
      (add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))
  in
  app li_src1 (app li_src2 (app ((LoadU (dst', src1', src2')) :: []) li_dst))
| StoreU (src1, src2, src3) ->
  let (li_src1, src1') = load_spilled_reg src1 r_tmp in
  let (li_src2, src2') =
    load_spilled_zreg src2
      (add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let (li_src3, src3') =
    load_spilled_zreg src3
      (add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))
  in
  app li_src1
    (app li_src2 (app li_src3 ((StoreU (src1', src2', src3')) :: [])))
| PromoteU dst ->
  let (li_src0, src0') = load_spilled_reg dst r_tmp in
  let (li_dst, dst') = store_spilled_reg dst src0' in
  app li_src0 (app ((PromoteU dst') :: []) li_dst)
| x -> x :: []

(** val spill_labeled_instr : labeled_instr -> labeled_instr list **)

let spill_labeled_instr = function
| BInstr i0 -> instrs (spill_instruction i0)
| Br_Jnz (lbl, reg) ->
  let (il, r0) = load_spilled reg r_tmp in
  app (instrs il) ((Br_Jnz (lbl, r0)) :: [])
| x -> x :: []

(** val spilling : labeled_instr list -> labeled_instr list **)

let rec spilling = function
| [] -> []
| i :: il' -> app (spill_labeled_instr i) (spilling il')

(** val register_allocation :
    labeled_cerise_component -> labeled_cerise_component error **)

let register_allocation m =
  let opt_code = map spilling m.l_code in
  let reloc_shift = sub (length (concat opt_code)) (length (concat m.l_code))
  in
  let relocate = fun data n0 ->
    map (fun w ->
      match w with
      | Inl w0 -> Inl (shift_word w0 n0)
      | Inr s -> Inr s) data
  in
  let b_frm = length (concat opt_code) in
  let e_frm = add b_frm (length m.l_data.l_data_frame) in
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun func_closures -> Ok
    { l_code = opt_code; l_data = { l_data_frame =
    (relocate m.l_data.l_data_frame reloc_shift); l_data_func_closures =
    func_closures; l_data_section =
    (relocate m.l_data.l_data_section reloc_shift) }; l_main = m.l_main;
    l_exports = m.l_exports; l_imports = m.l_imports; l_init = m.l_init })
    (Obj.magic compute_func_closure b_frm e_frm opt_code
      m.l_data.l_data_func_closures)

(** val error_msg1 : char list -> 'a1 error **)

let error_msg1 s =
  Error
    (append
      ('['::('c'::('e'::('r'::('i'::('s'::('e'::('_'::('l'::('i'::('n'::('k'::('e'::('r'::('.'::('v'::(']'::(' '::[]))))))))))))))))))
      s)

(** val relocate_right :
    ('a1 -> Big_int_Z.big_int -> bool) -> ('a1 -> Big_int_Z.big_int -> 'a1)
    -> 'a1 -> cerise_linkable_object -> cerise_linkable_object -> 'a1 **)

let relocate_right leb0 add0 y p_left p_right =
  let a = length p_left.c_code in
  let b = length p_left.c_data in
  let c = length p_right.c_code in
  if leb0 y c then add0 y a else add0 y (add a b)

(** val relocate_left :
    ('a1 -> Big_int_Z.big_int -> bool) -> ('a1 -> Big_int_Z.big_int -> 'a1)
    -> 'a1 -> cerise_linkable_object -> cerise_linkable_object -> 'a1 **)

let relocate_left leb0 add0 y p_left p_right =
  let a = length p_left.c_code in
  let c = length p_right.c_code in if leb0 y a then y else add0 y c

(** val relocate_export_left :
    (section * Big_int_Z.big_int) -> cerise_linkable_object ->
    cerise_linkable_object -> section * Big_int_Z.big_int **)

let relocate_export_left export _ _ =
  export

(** val relocate_export_right :
    (section * Big_int_Z.big_int) -> cerise_linkable_object ->
    cerise_linkable_object -> section * Big_int_Z.big_int **)

let relocate_export_right export p_left _ =
  let (s, n0) = export in
  (match s with
   | Code -> (Code, (add n0 (length p_left.c_code)))
   | Data -> (Data, (add n0 (length p_left.c_data))))

(** val relocate_addr_right :
    Big_int_Z.big_int -> cerise_linkable_object -> cerise_linkable_object ->
    Big_int_Z.big_int **)

let relocate_addr_right =
  relocate_right Coq_Nat.ltb add

(** val relocate_eaddr_right :
    nbar -> cerise_linkable_object -> cerise_linkable_object -> nbar **)

let relocate_eaddr_right =
  relocate_right (fun x y -> nbar_leb x (Finite y)) (fun x y ->
    nbar_plus x (Finite y))

(** val relocate_addr_left :
    Big_int_Z.big_int -> cerise_linkable_object -> cerise_linkable_object ->
    Big_int_Z.big_int **)

let relocate_addr_left =
  relocate_left Coq_Nat.ltb add

(** val relocate_eaddr_left :
    nbar -> cerise_linkable_object -> cerise_linkable_object -> nbar **)

let relocate_eaddr_left =
  relocate_left (fun x y -> nbar_leb x (Finite y)) (fun x y ->
    nbar_plus x (Finite y))

(** val relocate_word :
    (Big_int_Z.big_int -> cerise_linkable_object -> cerise_linkable_object ->
    Big_int_Z.big_int) -> (nbar -> cerise_linkable_object ->
    cerise_linkable_object -> nbar) -> word -> cerise_linkable_object ->
    cerise_linkable_object -> word **)

let relocate_word relocate_addr relocate_eaddr w p_left p_right =
  let relocate_sealable = fun sb ->
    match sb with
    | SCap (p0, b, e, a) ->
      let b0 = relocate_addr b p_left p_right in
      let e0 = relocate_eaddr e p_left p_right in
      let a0 = relocate_addr a p_left p_right in SCap (p0, b0, e0, a0)
    | SSealRange (p0, b, e, a) -> SSealRange (p0, b, e, a)
  in
  (match w with
   | WInt z0 -> WInt z0
   | WSealable sb -> WSealable (relocate_sealable sb)
   | WSealed (ot, sb) -> WSealed (ot, (relocate_sealable sb)))

(** val relocate_abstract_word :
    (Big_int_Z.big_int -> cerise_linkable_object -> cerise_linkable_object ->
    Big_int_Z.big_int) -> (nbar -> cerise_linkable_object ->
    cerise_linkable_object -> nbar) -> abstract_word ->
    cerise_linkable_object -> cerise_linkable_object -> abstract_word **)

let relocate_abstract_word relocate_addr relocate_eaddr w p_left p_right =
  match w with
  | Inl w0 ->
    Inl (relocate_word relocate_addr relocate_eaddr w0 p_left p_right)
  | Inr s -> Inr s

(** val relocate_word_right :
    word -> cerise_linkable_object -> cerise_linkable_object -> word **)

let relocate_word_right =
  relocate_word relocate_addr_right relocate_eaddr_right

(** val relocate_abstract_word_right :
    abstract_word -> cerise_linkable_object -> cerise_linkable_object ->
    abstract_word **)

let relocate_abstract_word_right =
  relocate_abstract_word relocate_addr_right relocate_eaddr_right

(** val relocate_abstract_word_left :
    abstract_word -> cerise_linkable_object -> cerise_linkable_object ->
    abstract_word **)

let relocate_abstract_word_left =
  relocate_abstract_word relocate_addr_left relocate_eaddr_left

(** val check_global :
    compilerParameters -> word -> global_type -> abstract_word list -> unit
    error **)

let check_global h0 w glob_t mem =
  let error_msg3 = fun s ->
    error_msg1
      (append
        ('['::('c'::('h'::('e'::('c'::('k'::('_'::('g'::('l'::('o'::('b'::('a'::('l'::(']'::(' '::[])))))))))))))))
        s)
  in
  (match w with
   | WSealed (o, s) ->
     (match s with
      | SCap (p, gb, ge, ga) ->
        let (p0, l) = p in
        (match p0 with
         | RW ->
           (match l with
            | Global ->
              if negb (Nat.eqb o h0.otype_global)
              then error_msg3
                     ('t'::('h'::('e'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::(' '::('h'::('a'::('s'::(' '::('t'::('h'::('e'::(' '::('w'::('r'::('o'::('n'::('g'::(' '::('o'::('t'::('y'::('p'::('e'::[]))))))))))))))))))))))))))))))
              else if negb
                        (nbar_eqb ge
                          (nbar_plus (Finite gb) (Finite
                            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                            (Big_int_Z.succ_big_int
                            Big_int_Z.zero_big_int))))))
                   then error_msg3
                          ('g'::('l'::('o'::('b'::('a'::('l'::(' '::('h'::('a'::('s'::(' '::('t'::('h'::('e'::(' '::('w'::('r'::('o'::('n'::('g'::(' '::('s'::('i'::('z'::('e'::[])))))))))))))))))))))))))
                   else if negb (Nat.eqb ga gb)
                        then error_msg3
                               ('g'::('l'::('o'::('b'::('a'::('l'::(' '::('d'::('o'::('e'::('s'::('n'::('\''::('t'::(' '::('p'::('o'::('i'::('n'::('t'::('s'::(' '::('t'::('o'::(' '::('t'::('h'::('e'::(' '::('l'::('o'::('w'::('e'::('r'::(' '::('b'::('o'::('u'::('n'::('d'::[]))))))))))))))))))))))))))))))))))))))))
                        else (match nth_error mem gb with
                              | Some a ->
                                (match a with
                                 | Inl g_val ->
                                   (match nth_error mem
                                            (add gb (Big_int_Z.succ_big_int
                                              Big_int_Z.zero_big_int)) with
                                    | Some a0 ->
                                      (match a0 with
                                       | Inl w0 ->
                                         (match w0 with
                                          | WInt g_type ->
                                            (match nth_error mem
                                                     (add gb
                                                       (Big_int_Z.succ_big_int
                                                       (Big_int_Z.succ_big_int
                                                       Big_int_Z.zero_big_int))) with
                                             | Some a1 ->
                                               (match a1 with
                                                | Inl _ ->
                                                  mbind
                                                    (Obj.magic (fun _ _ ->
                                                      error_bind)) (fun _ ->
                                                    mbind
                                                      (Obj.magic (fun _ _ ->
                                                        error_bind))
                                                      (fun _ -> Ok ())
                                                      (match glob_t.tg_mut with
                                                       | MUT_immut ->
                                                         if Z.eqb g_type
                                                              Big_int_Z.zero_big_int
                                                         then Ok ()
                                                         else error_msg3
                                                                ('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('m'::('u'::('t'::(' '::('1'::[]))))))))))))))
                                                       | MUT_mut ->
                                                         if Z.eqb g_type
                                                              Big_int_Z.unit_big_int
                                                         then Ok ()
                                                         else error_msg3
                                                                ('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('m'::('u'::('t'::(' '::('1'::[]))))))))))))))))
                                                    (match glob_t.tg_t with
                                                     | T_int ->
                                                       if Z.eqb g_type
                                                            Big_int_Z.zero_big_int
                                                       then (match g_val with
                                                             | WInt _ -> Ok ()
                                                             | _ ->
                                                               error_msg3
                                                                 ('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('Z'::(','::(' '::('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('a'::('c'::('t'::('u'::('a'::('l'::(' '::('v'::('a'::('l'::('u'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::[])))))))))))))))))))))))))))))))))))))))))))))
                                                       else error_msg3
                                                              ('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('Z'::[])))))))))))))))
                                                     | T_handle ->
                                                       if Z.eqb g_type
                                                            Big_int_Z.unit_big_int
                                                       then (match g_val with
                                                             | WSealable sb ->
                                                               (match sb with
                                                                | SCap (
                                                                    p1, a2,
                                                                    n0, a3) ->
                                                                  let (
                                                                    p2, l0) =
                                                                    p1
                                                                  in
                                                                  (match p2 with
                                                                   | RW ->
                                                                    (match l0 with
                                                                    | Global ->
                                                                    ((fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
                                                                    (fun _ ->
                                                                    match n0 with
                                                                    | Finite n1 ->
                                                                    ((fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
                                                                    (fun _ ->
                                                                    (fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
                                                                    (fun _ ->
                                                                    Ok
                                                                    ())
                                                                    (fun _ ->
                                                                    error_msg3
                                                                    ('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('C'::('a'::('p'::(','::(' '::('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('a'::('c'::('t'::('u'::('a'::('l'::(' '::('v'::('a'::('l'::('u'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('t'::('h'::('e'::(' '::('n'::('u'::('l'::('l'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                                                    a3)
                                                                    (fun _ ->
                                                                    error_msg3
                                                                    ('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('C'::('a'::('p'::(','::(' '::('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('a'::('c'::('t'::('u'::('a'::('l'::(' '::('v'::('a'::('l'::('u'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('t'::('h'::('e'::(' '::('n'::('u'::('l'::('l'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                                                    n1)
                                                                    | P_infty ->
                                                                    error_msg3
                                                                    ('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('C'::('a'::('p'::(','::(' '::('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('a'::('c'::('t'::('u'::('a'::('l'::(' '::('v'::('a'::('l'::('u'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('t'::('h'::('e'::(' '::('n'::('u'::('l'::('l'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                                                    (fun _ ->
                                                                    error_msg3
                                                                    ('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('C'::('a'::('p'::(','::(' '::('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('a'::('c'::('t'::('u'::('a'::('l'::(' '::('v'::('a'::('l'::('u'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('t'::('h'::('e'::(' '::('n'::('u'::('l'::('l'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                                                    a2)
                                                                    | _ ->
                                                                    error_msg3
                                                                    ('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('C'::('a'::('p'::(','::(' '::('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('a'::('c'::('t'::('u'::('a'::('l'::(' '::('v'::('a'::('l'::('u'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('t'::('h'::('e'::(' '::('n'::('u'::('l'::('l'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                                                   | _ ->
                                                                    error_msg3
                                                                    ('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('C'::('a'::('p'::(','::(' '::('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('a'::('c'::('t'::('u'::('a'::('l'::(' '::('v'::('a'::('l'::('u'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('t'::('h'::('e'::(' '::('n'::('u'::('l'::('l'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                                                | SSealRange (
                                                                    _, _, _, _) ->
                                                                  error_msg3
                                                                    ('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('C'::('a'::('p'::(','::(' '::('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('a'::('c'::('t'::('u'::('a'::('l'::(' '::('v'::('a'::('l'::('u'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('t'::('h'::('e'::(' '::('n'::('u'::('l'::('l'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                                             | _ ->
                                                               error_msg3
                                                                 ('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('C'::('a'::('p'::(','::(' '::('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('a'::('c'::('t'::('u'::('a'::('l'::(' '::('v'::('a'::('l'::('u'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('t'::('h'::('e'::(' '::('n'::('u'::('l'::('l'::(' '::('p'::('o'::('i'::('n'::('t'::('e'::('r'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
                                                       else error_msg3
                                                              ('e'::('x'::('p'::('e'::('c'::('t'::('e'::('d'::(' '::('t'::('y'::('p'::('e'::(' '::('C'::('a'::('p'::[]))))))))))))))))))
                                                | Inr _ ->
                                                  error_msg3
                                                    ('t'::('h'::('e'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::('_'::('m'::('u'::('t'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('e'::(' '::('a'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::[])))))))))))))))))))))))))))))))))))))))))
                                             | None ->
                                               error_msg3
                                                 ('n'::('o'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::('_'::('m'::('u'::('t'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::(' '::('i'::('n'::(' '::('t'::('h'::('e'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::(' '::('r'::('e'::('g'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))))))))
                                          | _ ->
                                            error_msg3
                                              ('t'::('h'::('e'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::('_'::('t'::('y'::('p'::('e'::(' '::('m'::('u'::('s'::('t'::(' '::('b'::('e'::(' '::('a'::('n'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::[])))))))))))))))))))))))))))))))))))
                                       | Inr _ ->
                                         error_msg3
                                           ('t'::('h'::('e'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::('_'::('t'::('y'::('p'::('e'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('e'::(' '::('a'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::[])))))))))))))))))))))))))))))))))))
                                    | None ->
                                      error_msg3
                                        ('n'::('o'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::('_'::('t'::('y'::('p'::('e'::(' '::('i'::('n'::(' '::('t'::('h'::('e'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::(' '::('r'::('e'::('g'::('i'::('o'::('n'::[]))))))))))))))))))))))))))))))))))))
                                 | Inr _ ->
                                   error_msg3
                                     ('t'::('h'::('e'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::('_'::('v'::('a'::('l'::('u'::('e'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('e'::(' '::('a'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::[]))))))))))))))))))))))))))))))))))))
                              | None ->
                                error_msg3
                                  ('n'::('o'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::('_'::('v'::('a'::('l'::('u'::('e'::(' '::('i'::('n'::(' '::('t'::('h'::('e'::(' '::('g'::('l'::('o'::('b'::('a'::('l'::(' '::('r'::('e'::('g'::('i'::('o'::('n'::[])))))))))))))))))))))))))))))))))))))
            | _ ->
              error_msg3
                ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))
         | _ ->
           error_msg3
             ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))
      | SSealRange (_, _, _, _) ->
        error_msg3
          ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))
   | _ ->
     error_msg3
       ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))

(** val check_limits :
    Big_int_Z.big_int -> Big_int_Z.big_int option -> Big_int_Z.big_int ->
    nbar -> unit error **)

let check_limits imp_min imp_max exp_min exp_max =
  if negb (Coq_Nat.leb imp_min exp_min)
  then error_msg1
         ('['::('c'::('h'::('e'::('c'::('k'::('_'::('l'::('i'::('m'::('i'::('t'::('s'::(']'::(' '::('i'::('m'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('m'::('i'::('n'::(' '::('n'::('e'::('e'::('d'::('s'::(' '::('t'::('o'::(' '::('b'::('e'::(' '::('l'::('a'::('r'::('g'::('e'::('r'::(' '::('o'::('r'::(' '::('e'::('q'::('u'::('a'::('l'::(' '::('t'::('h'::('a'::('n'::(' '::('e'::('x'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('m'::('i'::('n'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  else (match imp_max with
        | Some imp_max0 ->
          if negb (nbar_leb (Finite imp_max0) exp_max)
          then error_msg1
                 ('['::('c'::('h'::('e'::('c'::('k'::('_'::('l'::('i'::('m'::('i'::('t'::('s'::(']'::(' '::('i'::('m'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('m'::('a'::('x'::(' '::('n'::('e'::('e'::('d'::('s'::(' '::('t'::('o'::(' '::('b'::('e'::(' '::('l'::('o'::('w'::('e'::('r'::(' '::('o'::('r'::(' '::('e'::('q'::('u'::('a'::('l'::(' '::('t'::('h'::('a'::('n'::(' '::('e'::('x'::('p'::('o'::('r'::('t'::('e'::('d'::(' '::('m'::('a'::('x'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
          else Ok ()
        | None -> Ok ())

(** val check_function_closure :
    compilerParameters -> word -> function_type option -> abstract_word list
    -> unit error **)

let check_function_closure h0 w ft mem =
  let error_msg3 = fun s ->
    error_msg1
      (append
        ('['::('c'::('h'::('e'::('c'::('k'::('_'::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::('_'::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(']'::(' '::[])))))))))))))))))))))))))
        s)
  in
  (match w with
   | WSealable sb ->
     (match sb with
      | SCap (p, fcb, fce, fca) ->
        let (p0, l) = p in
        (match p0 with
         | RO ->
           (match l with
            | Global ->
              if negb (Nat.eqb fca fcb)
              then error_msg3
                     ('c'::('l'::('o'::('s'::('u'::('r'::('e'::(' '::('d'::('o'::('e'::('s'::('n'::('\''::('t'::(' '::('p'::('o'::('i'::('n'::('t'::('s'::(' '::('t'::('o'::(' '::('t'::('h'::('e'::(' '::('l'::('o'::('w'::('e'::('r'::(' '::('b'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))))))))))))))))))
              else if negb
                        (nbar_eqb fce
                          (nbar_plus (Finite fcb) (Finite
                            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
                            (Big_int_Z.succ_big_int
                            Big_int_Z.zero_big_int))))))
                   then error_msg3
                          ('c'::('l'::('o'::('s'::('u'::('r'::('e'::(' '::('h'::('a'::('s'::(' '::('t'::('h'::('e'::(' '::('w'::('r'::('o'::('n'::('g'::(' '::('s'::('i'::('z'::('e'::[]))))))))))))))))))))))))))
                   else (match nth_error mem fcb with
                         | Some a ->
                           (match a with
                            | Inl w_code ->
                              (match nth_error mem
                                       (add fcb (Big_int_Z.succ_big_int
                                         Big_int_Z.zero_big_int)) with
                               | Some a0 ->
                                 (match a0 with
                                  | Inl w_data ->
                                    (match nth_error mem
                                             (add fcb (Big_int_Z.succ_big_int
                                               (Big_int_Z.succ_big_int
                                               Big_int_Z.zero_big_int))) with
                                     | Some a1 ->
                                       (match a1 with
                                        | Inl w_ftype ->
                                          (match w_ftype with
                                           | WInt ft_exp ->
                                             (match w_code with
                                              | WSealed (ot_code, s) ->
                                                (match s with
                                                 | SCap (p1, fb, fe, fa) ->
                                                   let (p_code, l0) = p1 in
                                                   (match l0 with
                                                    | Global ->
                                                      if negb
                                                           (executeAllowed
                                                             p_code)
                                                      then error_msg3
                                                             ('c'::('o'::('d'::('e'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('e'::('x'::('e'::('c'::('u'::('t'::('a'::('b'::('l'::('e'::[])))))))))))))))))))))))))))))))))
                                                      else (match w_data with
                                                            | WSealed (
                                                                ot_data, s0) ->
                                                              (match s0 with
                                                               | SCap (
                                                                   p2, fmb,
                                                                   fme, fma) ->
                                                                 let (
                                                                   p_data, l1) =
                                                                   p2
                                                                 in
                                                                 (match l1 with
                                                                  | Global ->
                                                                    if 
                                                                    executeAllowed
                                                                    p_data
                                                                    then 
                                                                    error_msg3
                                                                    ('d'::('a'::('t'::('a'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::(' '::('i'::('s'::(' '::('e'::('x'::('e'::('c'::('u'::('t'::('a'::('b'::('l'::('e'::[])))))))))))))))))))))))))))))
                                                                    else 
                                                                    if 
                                                                    negb
                                                                    (Nat.eqb
                                                                    ot_code
                                                                    ot_data)
                                                                    then 
                                                                    error_msg3
                                                                    ('d'::('i'::('f'::('f'::('e'::('r'::('e'::('n'::('t'::(' '::('c'::('o'::('d'::('e'::(' '::('a'::('n'::('d'::(' '::('d'::('a'::('t'::('a'::(' '::('o'::('t'::('y'::('p'::('e'::('s'::[]))))))))))))))))))))))))))))))
                                                                    else 
                                                                    if 
                                                                    negb
                                                                    (Coq_Nat.leb
                                                                    ot_code
                                                                    ot_data)
                                                                    then 
                                                                    error_msg3
                                                                    ('o'::('t'::('y'::('p'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('o'::('t'::('y'::('p'::('e'::[])))))))))))))))))))))))))))))
                                                                    else 
                                                                    if 
                                                                    negb
                                                                    ((&&)
                                                                    (Coq_Nat.leb
                                                                    fb fa)
                                                                    (nbar_ltb
                                                                    (Finite
                                                                    fb) fe))
                                                                    then 
                                                                    error_msg3
                                                                    ('c'::('o'::('d'::('e'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::(' '::('m'::('a'::('l'::('f'::('o'::('r'::('m'::('e'::('d'::[])))))))))))))))))))))))))
                                                                    else 
                                                                    if 
                                                                    negb
                                                                    ((&&)
                                                                    (Coq_Nat.leb
                                                                    fmb fma)
                                                                    (nbar_ltb
                                                                    (Finite
                                                                    fmb) fme))
                                                                    then 
                                                                    error_msg3
                                                                    ('d'::('a'::('t'::('a'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::(' '::('m'::('a'::('l'::('f'::('o'::('r'::('m'::('e'::('d'::[])))))))))))))))))))))))))
                                                                    else 
                                                                    (match ft with
                                                                    | Some ft0 ->
                                                                    if 
                                                                    Z.eqb
                                                                    (h0.encode_function_type
                                                                    ft0)
                                                                    ft_exp
                                                                    then Ok ()
                                                                    else 
                                                                    error_msg3
                                                                    ('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('t'::('y'::('p'::('e'::('s'::(' '::('d'::('o'::('n'::('\''::('t'::(' '::('m'::('a'::('t'::('c'::('h'::[]))))))))))))))))))))))))))
                                                                    | None ->
                                                                    Ok ())
                                                                  | _ ->
                                                                    error_msg3
                                                                    ('d'::('a'::('t'::('a'::(' '::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))
                                                               | SSealRange (
                                                                   _, _, _, _) ->
                                                                 error_msg3
                                                                   ('d'::('a'::('t'::('a'::(' '::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))
                                                            | _ ->
                                                              error_msg3
                                                                ('d'::('a'::('t'::('a'::(' '::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))
                                                    | _ ->
                                                      error_msg3
                                                        ('c'::('o'::('d'::('e'::(' '::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))
                                                 | SSealRange (_, _, _, _) ->
                                                   error_msg3
                                                     ('c'::('o'::('d'::('e'::(' '::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))
                                              | _ ->
                                                error_msg3
                                                  ('c'::('o'::('d'::('e'::(' '::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))
                                           | _ ->
                                             error_msg3
                                               ('c'::('o'::('d'::('e'::(' '::('c'::('l'::('o'::('s'::('u'::('r'::('e'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::('n'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::[])))))))))))))))))))))))))))))))
                                        | Inr _ ->
                                          error_msg3
                                            ('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('t'::('y'::('p'::('e'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('e'::(' '::('a'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::[])))))))))))))))))))))))))))))))))
                                     | None ->
                                       error_msg3
                                         ('f'::('u'::('n'::('c'::('t'::('i'::('o'::('n'::(' '::('t'::('y'::('p'::('e'::(' '::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))
                                  | Inr _ ->
                                    error_msg3
                                      ('d'::('a'::('t'::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('e'::(' '::('a'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::[])))))))))))))))))))))))))))))))
                               | None ->
                                 error_msg3
                                   ('d'::('a'::('t'::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[]))))))))))))))))))))))
                            | Inr _ ->
                              error_msg3
                                ('c'::('o'::('d'::('e'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('e'::(' '::('a'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::[])))))))))))))))))))))))))))))))
                         | None ->
                           error_msg3
                             ('c'::('o'::('d'::('e'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[]))))))))))))))))))))))
            | _ ->
              error_msg3
                ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('R'::('O'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[]))))))))))))))))))))))))))))))))))))
         | _ ->
           error_msg3
             ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('R'::('O'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[]))))))))))))))))))))))))))))))))))))
      | SSealRange (_, _, _, _) ->
        error_msg3
          ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('R'::('O'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[]))))))))))))))))))))))))))))))))))))
   | _ ->
     error_msg3
       ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('R'::('O'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[]))))))))))))))))))))))))))))))))))))

(** val check_itable_entry :
    compilerParameters -> Big_int_Z.big_int -> addr -> abstract_word list ->
    unit error **)

let check_itable_entry h0 i tb mem =
  match nth_error mem (add tb i) with
  | Some a ->
    (match a with
     | Inl w ->
       (match w with
        | WInt z0 ->
          ((fun fO fp fn z -> let s = Big_int_Z.sign_big_int z in
  if s = 0 then fO () else if s > 0 then fp z
  else fn (Big_int_Z.minus_big_int z))
             (fun _ -> Ok ())
             (fun _ -> check_function_closure h0 w None mem)
             (fun _ -> check_function_closure h0 w None mem)
             z0)
        | _ -> check_function_closure h0 w None mem)
     | Inr _ ->
       error_msg1
         ('['::('c'::('h'::('e'::('c'::('k'::('_'::('i'::('t'::('a'::('b'::('l'::('e'::('_'::('e'::('n'::('t'::('r'::('i'::('e'::('s'::(']'::(' '::('e'::('n'::('t'::('r'::('y'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('e'::(' '::('a'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::[]))))))))))))))))))))))))))))))))))))))))))))))))
  | None ->
    error_msg1
      ('['::('c'::('h'::('e'::('c'::('k'::('_'::('i'::('t'::('a'::('b'::('l'::('e'::('_'::('e'::('n'::('t'::('r'::('i'::('e'::('s'::(']'::(' '::('e'::('n'::('t'::('r'::('y'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[]))))))))))))))))))))))))))))))))))))))

(** val check_itable_entries_func :
    compilerParameters -> (nbar, (addr, abstract_word list) sigT) sigT ->
    unit error **)

let rec check_itable_entries_func h0 x =
  let i = projT1 x in
  let tb = projT1 (projT2 x) in
  let mem = projT2 (projT2 x) in
  let check_itable_entries0 = fun i0 tb0 mem0 ->
    check_itable_entries_func h0 (ExistT (i0, (ExistT (tb0, mem0))))
  in
  (match i with
   | Finite n0 ->
     ((fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ ->
        check_itable_entry h0 Big_int_Z.zero_big_int tb mem)
        (fun n1 ->
        mbind (Obj.magic (fun _ _ -> error_bind)) (fun _ ->
          check_itable_entries0 (Finite n1) tb mem)
          (check_itable_entry h0 (Big_int_Z.succ_big_int n1) tb mem))
        n0)
   | P_infty ->
     error_msg1
       ('['::('c'::('h'::('e'::('c'::('k'::('_'::('i'::('t'::('a'::('b'::('l'::('e'::('_'::('e'::('n'::('t'::('r'::('i'::('e'::('s'::(']'::(' '::('t'::('a'::('b'::('l'::('e'::(' '::('c'::('a'::('n'::('\''::('t'::(' '::('h'::('a'::('v'::('e'::(' '::('a'::('n'::(' '::('i'::('n'::('f'::('i'::('n'::('i'::('t'::('y'::(' '::('s'::('i'::('z'::('e'::(' '::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val check_itable_entries :
    compilerParameters -> nbar -> addr -> abstract_word list -> unit error **)

let check_itable_entries h0 i tb mem =
  check_itable_entries_func h0 (ExistT (i, (ExistT (tb, mem))))

(** val check_itable :
    compilerParameters -> word -> table_type -> abstract_word list -> unit
    error **)

let check_itable h0 w tbl_t mem =
  let error_msg3 = fun s ->
    error_msg1
      (append
        ('['::('c'::('h'::('e'::('c'::('k'::('_'::('i'::('t'::('a'::('b'::('l'::('e'::(']'::(' '::[])))))))))))))))
        s)
  in
  (match w with
   | WSealable sb ->
     (match sb with
      | SCap (p, tb, te, ta) ->
        let (p0, l) = p in
        (match p0 with
         | RO ->
           (match l with
            | Global ->
              if negb (nbar_eqb (Finite ta) (Finite tb))
              then error_msg3
                     ('i'::('t'::('a'::('b'::('l'::('e'::(' '::('d'::('o'::('e'::('s'::('n'::('\''::('t'::(' '::('p'::('o'::('i'::('n'::('t'::('s'::(' '::('t'::('o'::(' '::('t'::('h'::('e'::(' '::('l'::('o'::('w'::('e'::('r'::(' '::('b'::('o'::('u'::('n'::('d'::[]))))))))))))))))))))))))))))))))))))))))
              else let imp_min = (tt_limits tbl_t).lim_min in
                   let imp_max = (tt_limits tbl_t).lim_max in
                   let exp_min = Big_int_Z.zero_big_int in
                   let exp_max = nbar_minus te (Finite tb) in
                   mbind (Obj.magic (fun _ _ -> error_bind)) (fun _ ->
                     check_itable_entries h0 (nbar_minus te (Finite tb)) tb
                       mem) (check_limits imp_min imp_max exp_min exp_max)
            | _ ->
              error_msg3
                ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('R'::('O'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[]))))))))))))))))))))))))))))))))))))
         | _ ->
           error_msg3
             ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('R'::('O'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[]))))))))))))))))))))))))))))))))))))
      | SSealRange (_, _, _, _) ->
        error_msg3
          ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('R'::('O'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[]))))))))))))))))))))))))))))))))))))
   | _ ->
     error_msg3
       ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('R'::('O'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[]))))))))))))))))))))))))))))))))))))

(** val check_lin_mem_entry :
    Big_int_Z.big_int -> addr -> abstract_word list -> unit error **)

let check_lin_mem_entry i tb mem =
  match nth_error mem (add tb i) with
  | Some a ->
    (match a with
     | Inl w ->
       (match w with
        | WInt _ -> Ok ()
        | _ ->
          error_msg1
            ('['::('c'::('h'::('e'::('c'::('k'::('_'::('l'::('i'::('n'::('_'::('m'::('e'::('m'::('_'::('e'::('n'::('t'::('r'::('y'::(']'::(' '::('e'::('n'::('t'::('r'::('y'::(' '::('m'::('u'::('s'::('t'::(' '::('b'::('e'::(' '::('a'::('n'::(' '::('i'::('n'::('t'::('e'::('g'::('e'::('r'::[])))))))))))))))))))))))))))))))))))))))))))))))
     | Inr _ ->
       error_msg1
         ('['::('c'::('h'::('e'::('c'::('k'::('_'::('l'::('i'::('n'::('_'::('m'::('e'::('m'::('_'::('e'::('n'::('t'::('r'::('y'::(']'::(' '::('e'::('n'::('t'::('r'::('y'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('e'::(' '::('a'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::[])))))))))))))))))))))))))))))))))))))))))))))))
  | None ->
    error_msg1
      ('['::('c'::('h'::('e'::('c'::('k'::('_'::('l'::('i'::('n'::('_'::('m'::('e'::('m'::('_'::('e'::('n'::('t'::('r'::('y'::(']'::(' '::('e'::('n'::('t'::('r'::('y'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))))))))))))))

(** val check_lin_mem_entries_func :
    compilerParameters -> (nbar, (addr, abstract_word list) sigT) sigT ->
    unit error **)

let rec check_lin_mem_entries_func h0 x =
  let i = projT1 x in
  let tb = projT1 (projT2 x) in
  let mem = projT2 (projT2 x) in
  let check_lin_mem_entries0 = fun i0 tb0 mem0 ->
    check_lin_mem_entries_func h0 (ExistT (i0, (ExistT (tb0, mem0))))
  in
  (match i with
   | Finite n0 ->
     ((fun fO fS n -> if Big_int_Z.sign_big_int n <= 0 then fO ()
  else fS (Big_int_Z.pred_big_int n))
        (fun _ ->
        check_itable_entry h0 Big_int_Z.zero_big_int tb mem)
        (fun n1 ->
        mbind (Obj.magic (fun _ _ -> error_bind)) (fun _ ->
          check_lin_mem_entries0 (Finite n1) tb mem)
          (check_lin_mem_entry (Big_int_Z.succ_big_int n1) tb mem))
        n0)
   | P_infty ->
     error_msg1
       ('['::('c'::('h'::('e'::('c'::('k'::('_'::('l'::('i'::('n'::('_'::('m'::('e'::('m'::('_'::('e'::('n'::('t'::('r'::('i'::('e'::('s'::(']'::(' '::('t'::('a'::('b'::('l'::('e'::(' '::('c'::('a'::('n'::('\''::('t'::(' '::('h'::('a'::('v'::('e'::(' '::('a'::('n'::(' '::('i'::('n'::('f'::('i'::('n'::('i'::('t'::('y'::(' '::('s'::('i'::('z'::('e'::(' '::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val check_lin_mem_entries :
    compilerParameters -> nbar -> addr -> abstract_word list -> unit error **)

let check_lin_mem_entries h0 i tb mem =
  check_lin_mem_entries_func h0 (ExistT (i, (ExistT (tb, mem))))

(** val check_lin_mem :
    compilerParameters -> word -> memory_type -> abstract_word list -> unit
    error **)

let check_lin_mem h0 w mem_t mem =
  let error_msg3 = fun s ->
    error_msg1
      (append
        ('['::('c'::('h'::('e'::('c'::('k'::('_'::('l'::('i'::('n'::('_'::('m'::('e'::('m'::(']'::(' '::[]))))))))))))))))
        s)
  in
  (match w with
   | WSealed (_, s) ->
     (match s with
      | SCap (p, lmb, lme, lma) ->
        let (p0, l) = p in
        (match p0 with
         | RW ->
           (match l with
            | Global ->
              if negb (Nat.eqb lma lmb)
              then error_msg3
                     ('l'::('i'::('n'::('e'::('a'::('r'::(' '::('m'::('e'::('m'::('o'::('r'::('y'::(' '::('d'::('o'::('e'::('s'::('n'::('\''::('t'::(' '::('p'::('o'::('i'::('n'::('t'::('s'::(' '::('t'::('o'::(' '::('t'::('h'::('e'::(' '::('l'::('o'::('w'::('e'::('r'::(' '::('b'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))))))))))))))))))))))))
              else (match nth_error mem lmb with
                    | Some a ->
                      (match a with
                       | Inl w_full_cap ->
                         (match nth_error mem
                                  (add lmb (Big_int_Z.succ_big_int
                                    Big_int_Z.zero_big_int)) with
                          | Some a0 ->
                            (match a0 with
                             | Inl w_cur_cap ->
                               (match w_full_cap with
                                | WSealable sb ->
                                  (match sb with
                                   | SCap (p1, fmb, fme, fma) ->
                                     let (p2, l0) = p1 in
                                     (match p2 with
                                      | RW ->
                                        (match l0 with
                                         | Global ->
                                           (match w_cur_cap with
                                            | WSealable sb0 ->
                                              (match sb0 with
                                               | SCap (p3, fcb, fce, fca) ->
                                                 let (p4, l1) = p3 in
                                                 (match p4 with
                                                  | RW ->
                                                    (match l1 with
                                                     | Global ->
                                                       if negb
                                                            (Nat.eqb fmb
                                                              (add lmb
                                                                (Big_int_Z.succ_big_int
                                                                (Big_int_Z.succ_big_int
                                                                Big_int_Z.zero_big_int))))
                                                       then error_msg3
                                                              ('b'::('a'::('d'::(' '::('f'::('u'::('l'::('l'::(' '::('c'::('a'::('p'::(' '::('l'::('o'::('w'::('e'::('r'::(' '::('b'::('o'::('u'::('n'::('d'::[]))))))))))))))))))))))))
                                                       else if negb
                                                                 (nbar_eqb
                                                                   fme lme)
                                                            then error_msg3
                                                                   ('b'::('a'::('d'::(' '::('f'::('u'::('l'::('l'::(' '::('c'::('a'::('p'::(' '::('u'::('p'::('p'::('e'::('r'::(' '::('b'::('o'::('u'::('n'::('d'::[]))))))))))))))))))))))))
                                                            else if negb
                                                                    (Nat.eqb
                                                                    fcb fmb)
                                                                 then 
                                                                   error_msg3
                                                                    ('b'::('a'::('d'::(' '::('c'::('u'::('r'::('r'::('e'::('n'::('t'::(' '::('c'::('a'::('p'::(' '::('l'::('o'::('w'::('e'::('r'::(' '::('b'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))))
                                                                 else 
                                                                   if 
                                                                    negb
                                                                    (nbar_eqb
                                                                    fce
                                                                    (Finite
                                                                    fma))
                                                                   then 
                                                                    error_msg3
                                                                    ('b'::('a'::('d'::(' '::('c'::('u'::('r'::('r'::('e'::('n'::('t'::(' '::('c'::('a'::('p'::(' '::('u'::('p'::('p'::('e'::('r'::(' '::('b'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))))
                                                                   else 
                                                                    if 
                                                                    negb
                                                                    (nbar_eqb
                                                                    (Finite
                                                                    fca)
                                                                    (Finite
                                                                    fcb))
                                                                    then 
                                                                    error_msg3
                                                                    ('b'::('a'::('d'::(' '::('c'::('u'::('r'::('r'::('e'::('n'::('t'::(' '::('c'::('a'::('p'::(' '::('a'::('d'::('d'::('r'::('e'::('s'::('s'::[])))))))))))))))))))))))
                                                                    else 
                                                                    let imp_min =
                                                                    mem_t.lim_min
                                                                    in
                                                                    let imp_max =
                                                                    mem_t.lim_max
                                                                    in
                                                                    let exp_min =
                                                                    Big_int_Z.zero_big_int
                                                                    in
                                                                    let exp_max =
                                                                    nbar_minus
                                                                    fme
                                                                    (Finite
                                                                    fmb)
                                                                    in
                                                                    mbind
                                                                    (Obj.magic
                                                                    (fun _ _ ->
                                                                    error_bind))
                                                                    (fun _ ->
                                                                    check_lin_mem_entries
                                                                    h0
                                                                    (nbar_minus
                                                                    fme
                                                                    (Finite
                                                                    fmb)) fmb
                                                                    mem)
                                                                    (check_limits
                                                                    imp_min
                                                                    imp_max
                                                                    exp_min
                                                                    exp_max)
                                                     | _ ->
                                                       error_msg3
                                                         ('c'::('u'::('r'::('_'::('m'::('e'::('m'::('_'::('c'::('a'::('p'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('c'::('u'::('r'::('r'::('e'::('n'::('t'::(' '::('m'::('e'::('m'::('o'::('r'::('y'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[])))))))))))))))))))))))))))))))))))))))))))))))
                                                  | _ ->
                                                    error_msg3
                                                      ('c'::('u'::('r'::('_'::('m'::('e'::('m'::('_'::('c'::('a'::('p'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('c'::('u'::('r'::('r'::('e'::('n'::('t'::(' '::('m'::('e'::('m'::('o'::('r'::('y'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[])))))))))))))))))))))))))))))))))))))))))))))))
                                               | SSealRange (_, _, _, _) ->
                                                 error_msg3
                                                   ('c'::('u'::('r'::('_'::('m'::('e'::('m'::('_'::('c'::('a'::('p'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('c'::('u'::('r'::('r'::('e'::('n'::('t'::(' '::('m'::('e'::('m'::('o'::('r'::('y'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[])))))))))))))))))))))))))))))))))))))))))))))))
                                            | _ ->
                                              error_msg3
                                                ('c'::('u'::('r'::('_'::('m'::('e'::('m'::('_'::('c'::('a'::('p'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('c'::('u'::('r'::('r'::('e'::('n'::('t'::(' '::('m'::('e'::('m'::('o'::('r'::('y'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[])))))))))))))))))))))))))))))))))))))))))))))))
                                         | _ ->
                                           error_msg3
                                             ('f'::('u'::('l'::('l'::('_'::('m'::('e'::('m'::('_'::('c'::('a'::('p'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('f'::('u'::('l'::('l'::(' '::('m'::('e'::('m'::('o'::('r'::('y'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[])))))))))))))))))))))))))))))))))))))))))))))
                                      | _ ->
                                        error_msg3
                                          ('f'::('u'::('l'::('l'::('_'::('m'::('e'::('m'::('_'::('c'::('a'::('p'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('f'::('u'::('l'::('l'::(' '::('m'::('e'::('m'::('o'::('r'::('y'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[])))))))))))))))))))))))))))))))))))))))))))))
                                   | SSealRange (_, _, _, _) ->
                                     error_msg3
                                       ('f'::('u'::('l'::('l'::('_'::('m'::('e'::('m'::('_'::('c'::('a'::('p'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('f'::('u'::('l'::('l'::(' '::('m'::('e'::('m'::('o'::('r'::('y'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[])))))))))))))))))))))))))))))))))))))))))))))
                                | _ ->
                                  error_msg3
                                    ('f'::('u'::('l'::('l'::('_'::('m'::('e'::('m'::('_'::('c'::('a'::('p'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('f'::('u'::('l'::('l'::(' '::('m'::('e'::('m'::('o'::('r'::('y'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[])))))))))))))))))))))))))))))))))))))))))))))
                             | Inr _ ->
                               error_msg3
                                 ('c'::('u'::('r'::('_'::('m'::('e'::('m'::('_'::('c'::('a'::('p'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('e'::(' '::('a'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::[])))))))))))))))))))))))))))))))
                          | None ->
                            error_msg3
                              ('c'::('u'::('r'::('_'::('m'::('e'::('m'::('_'::('c'::('a'::('p'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[]))))))))))))))))))))))
                       | Inr _ ->
                         error_msg3
                           ('f'::('u'::('l'::('l'::('_'::('m'::('e'::('m'::('_'::('c'::('a'::('p'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('b'::('e'::(' '::('a'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::[]))))))))))))))))))))))))))))))))
                    | None ->
                      error_msg3
                        ('f'::('u'::('l'::('l'::('_'::('m'::('e'::('m'::('_'::('c'::('a'::('p'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))
            | _ ->
              error_msg3
                ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))
         | _ ->
           error_msg3
             ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))
      | SSealRange (_, _, _, _) ->
        error_msg3
          ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))
   | _ ->
     error_msg3
       ('b'::('u'::('t'::(' '::('t'::('h'::('e'::(' '::('w'::('o'::('r'::('d'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::(' '::('s'::('e'::('a'::('l'::('e'::('d'::[])))))))))))))))))))))))))))))

(** val check_symbol_type :
    compilerParameters -> word -> Big_int_Z.big_int -> abstract_word list ->
    unit error **)

let check_symbol_type h0 w et mem =
  match h0.decode_extern_t et with
  | Some e ->
    (match e with
     | ET_func ft -> check_function_closure h0 w (Some ft) mem
     | ET_tab tbl_t -> check_itable h0 w tbl_t mem
     | ET_mem mem_t -> check_lin_mem h0 w mem_t mem
     | ET_glob glob_t -> check_global h0 w glob_t mem)
  | None -> Ok ()

(** val resolve_word :
    compilerParameters -> abstract_word -> abstract_word list ->
    abstract_word list -> (symbols, section_offset) gmap -> (symbols,
    Big_int_Z.big_int) gmap -> abstract_word error **)

let resolve_word h0 ws code_section data_section exports imports =
  match ws with
  | Inl w -> Ok (Inl w)
  | Inr s ->
    (match lookup0 (gmap_lookup string_eq_dec string_countable) s exports with
     | Some y ->
       let (sec, n0) = y in
       let data = match sec with
                  | Code -> code_section
                  | Data -> data_section
       in
       (match nth_error data n0 with
        | Some w ->
          (match w with
           | Inl w' ->
             (match lookup0 (gmap_lookup string_eq_dec string_countable) s
                      imports with
              | Some et ->
                mbind (Obj.magic (fun _ _ -> error_bind)) (fun _ -> Ok (Inl
                  w'))
                  (Obj.magic check_symbol_type h0 w' et
                    (app code_section data_section))
              | None -> Ok (Inl w'))
           | Inr _ ->
             error_msg1
               ('['::('r'::('e'::('s'::('o'::('l'::('v'::('e'::('_'::('w'::('o'::('r'::('d'::(']'::(' '::('i'::('m'::('p'::('o'::('r'::('t'::(' '::('a'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::('l'::('l'::('o'::('w'::('e'::('d'::[]))))))))))))))))))))))))))))))))))))))))))))))
        | None ->
          error_msg1
            ('['::('r'::('e'::('s'::('o'::('l'::('v'::('e'::('_'::('w'::('o'::('r'::('d'::(']'::(' '::('o'::('f'::('f'::('s'::('e'::('t'::(' '::('o'::('f'::(' '::('t'::('h'::('e'::(' '::('e'::('x'::('p'::('o'::('r'::('t'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[]))))))))))))))))))))))))))))))))))))))))))))))
     | None -> Ok (Inr s))

(** val resolve_data :
    compilerParameters -> abstract_word list -> abstract_word list ->
    abstract_word list -> (symbols, section_offset) gmap -> (symbols,
    Big_int_Z.big_int) gmap -> abstract_word list error **)

let rec resolve_data h0 left_data code_section data_section exports imports =
  match left_data with
  | [] -> Ok []
  | ws :: data' ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun solved_data ->
      mbind (Obj.magic (fun _ _ -> error_bind)) (fun solved_sym -> Ok
        (solved_sym :: solved_data))
        (Obj.magic resolve_word h0 ws code_section data_section exports
          imports))
      (resolve_data h0 data' code_section data_section exports imports)

(** val resolve_main :
    cerise_linkable_object -> cerise_linkable_object -> section_offset option
    error **)

let resolve_main p_left p_right =
  match p_left.c_main with
  | Some m ->
    (match p_right.c_main with
     | Some _ ->
       error_msg1
         ('['::('r'::('e'::('s'::('o'::('l'::('v'::('e'::('_'::('m'::('a'::('i'::('n'::(']'::(' '::('c'::('a'::('n'::('n'::('o'::('t'::(' '::('l'::('i'::('n'::('k'::(' '::('t'::('w'::('o'::(' '::('C'::('e'::('r'::('i'::('s'::('e'::(' '::('o'::('b'::('j'::('e'::('c'::('t'::('s'::(' '::('w'::('i'::('t'::('h'::(' '::('a'::(' '::('m'::('a'::('i'::('n'::(' '::('w'::('o'::('r'::('d'::(' '::('o'::('n'::(' '::('b'::('o'::('t'::('h'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     | None -> Ok (Some (relocate_export_left m p_left p_right)))
  | None ->
    (match p_right.c_main with
     | Some m -> Ok (Some (relocate_export_right m p_left p_right))
     | None -> Ok None)

(** val offset_otype :
    compilerParameters -> abstract_word -> oType -> abstract_word **)

let offset_otype h0 aw off =
  match aw with
  | Inl w ->
    (match w with
     | WSealed (ot, sb) ->
       Inl
         (if Coq_Nat.ltb ot h0.otype_module
          then WSealed (ot, sb)
          else WSealed
                 ((add
                    (add (sub ot h0.otype_module) (Big_int_Z.succ_big_int
                      Big_int_Z.zero_big_int)) off), sb))
     | _ -> Inl w)
  | Inr s -> Inr s

(** val offsets_otype :
    compilerParameters -> abstract_word list -> oType -> abstract_word list **)

let offsets_otype h0 l off =
  map (fun w -> offset_otype h0 w off) l

(** val get_max_otype : abstract_word list -> oType **)

let get_max_otype l =
  foldl (fun acc w ->
    match w with
    | Inl y -> (match y with
                | WSealed (o, _) -> max o acc
                | _ -> acc)
    | Inr _ -> acc) Big_int_Z.zero_big_int l

(** val initialize_init_entry :
    (Big_int_Z.big_int * word) -> word -> abstract_word list ->
    Big_int_Z.big_int -> abstract_word list error **)

let initialize_init_entry init_value tbl_cap data_left offset_left_data =
  match tbl_cap with
  | WSealable sb ->
    (match sb with
     | SCap (p, tb, te, _) ->
       let (p0, l) = p in
       (match p0 with
        | RO ->
          (match l with
           | Global ->
             let addr_entry = add tb (fst init_value) in
             if negb (nbar_ltb (Finite addr_entry) te)
             then error_msg1
                    ('['::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('_'::('s'::('y'::('m'::('b'::('o'::('l'::(']'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::(' '::('o'::('n'::('l'::('y'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::(' '::('w'::('i'::('t'::('h'::(' '::('s'::('u'::('f'::('f'::('i'::('c'::('i'::('e'::('n'::('t'::(' '::('s'::('i'::('z'::('e'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
             else let offset_tbl = sub addr_entry offset_left_data in
                  (match update_nth_error data_left offset_tbl (Inl
                           (snd init_value)) with
                   | Some updated_data -> Ok updated_data
                   | None ->
                     error_msg1
                       ('['::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('_'::('s'::('y'::('m'::('b'::('o'::('l'::(']'::(' '::('u'::('p'::('d'::('a'::('t'::('e'::(' '::('f'::('a'::('i'::('l'::('e'::('d'::[]))))))))))))))))))))))))))))))))))
           | _ ->
             error_msg1
               ('['::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('_'::('s'::('y'::('m'::('b'::('o'::('l'::(']'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::(' '::('o'::('n'::('l'::('y'::(' '::('R'::('O'::(','::(' '::('G'::('l'::('o'::('b'::('a'::('l'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
        | _ ->
          error_msg1
            ('['::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('_'::('s'::('y'::('m'::('b'::('o'::('l'::(']'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::(' '::('o'::('n'::('l'::('y'::(' '::('R'::('O'::(','::(' '::('G'::('l'::('o'::('b'::('a'::('l'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     | SSealRange (_, _, _, _) ->
       error_msg1
         ('['::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('_'::('s'::('y'::('m'::('b'::('o'::('l'::(']'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::(' '::('o'::('n'::('l'::('y'::(' '::('R'::('O'::(','::(' '::('G'::('l'::('o'::('b'::('a'::('l'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
  | _ ->
    error_msg1
      ('['::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('_'::('s'::('y'::('m'::('b'::('o'::('l'::(']'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::(' '::('o'::('n'::('l'::('y'::(' '::('R'::('O'::(','::(' '::('G'::('l'::('o'::('b'::('a'::('l'::(' '::('c'::('a'::('p'::('a'::('b'::('i'::('l'::('i'::('t'::('y'::[])))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val initialize_symbol :
    symbols -> (Big_int_Z.big_int * word) list -> (symbols, section_offset)
    gmap -> abstract_word list -> abstract_word list -> Big_int_Z.big_int ->
    abstract_word list error **)

let initialize_symbol sym init_values exports_left code_left data_left offset_left_data =
  match lookup0 (gmap_lookup string_eq_dec string_countable) sym exports_left with
  | Some y ->
    let (sec, off) = y in
    let data = match sec with
               | Code -> code_left
               | Data -> data_left in
    (match nth_error data off with
     | Some w ->
       (match w with
        | Inl w' ->
          foldl (fun acc ival ->
            mbind (Obj.magic (fun _ _ -> error_bind)) (fun ldata ->
              initialize_init_entry ival w' ldata offset_left_data) acc) (Ok
            data_left) init_values
        | Inr _ ->
          error_msg1
            ('['::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('_'::('s'::('y'::('m'::('b'::('o'::('l'::(']'::(' '::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::(' '::('w'::('i'::('t'::('h'::(' '::('a'::(' '::('s'::('y'::('m'::('b'::('o'::('l'::(' '::('i'::('s'::(' '::('n'::('o'::('t'::(' '::('a'::('l'::('l'::('o'::('w'::('e'::('d'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))
     | None ->
       error_msg1
         ('['::('i'::('n'::('i'::('t'::('i'::('a'::('l'::('i'::('z'::('e'::('_'::('s'::('y'::('m'::('b'::('o'::('l'::(']'::(' '::('o'::('f'::('f'::('s'::('e'::('t'::(' '::('o'::('f'::(' '::('t'::('h'::('e'::(' '::('e'::('x'::('p'::('o'::('r'::('t'::(' '::('n'::('o'::('t'::(' '::('f'::('o'::('u'::('n'::('d'::[])))))))))))))))))))))))))))))))))))))))))))))))))))
  | None -> Ok data_left

(** val initialize_symbols :
    (symbols, (Big_int_Z.big_int * word) list) gmap -> (symbols,
    section_offset) gmap -> abstract_word list -> abstract_word list ->
    Big_int_Z.big_int -> abstract_word list error **)

let initialize_symbols inits exports_left code_left data_left offset_left_data =
  map_fold (gmap_to_list string_eq_dec string_countable)
    (fun sym init_val acc ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun mem ->
      initialize_symbol sym init_val exports_left code_left mem
        offset_left_data) acc) (Ok data_left) inits

(** val relocate_init_right :
    (Big_int_Z.big_int * word) list -> cerise_linkable_object ->
    cerise_linkable_object -> (Big_int_Z.big_int * word) list **)

let relocate_init_right init_entry p_left p_right =
  map (fun i -> ((fst i), (relocate_word_right (snd i) p_left p_right)))
    init_entry

(** val link_seq :
    compilerParameters -> cerise_linkable_object -> cerise_linkable_object ->
    cerise_linkable_object error **)

let link_seq h0 p_left p_right =
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun main_word ->
    let relocated_right_code =
      map (fun w -> relocate_abstract_word_right w p_left p_right)
        p_right.c_code
    in
    let relocated_right_data =
      map (fun w -> relocate_abstract_word_right w p_left p_right)
        p_right.c_data
    in
    let relocated_left_code =
      map (fun w -> relocate_abstract_word_left w p_left p_right)
        p_left.c_code
    in
    let relocated_left_data =
      map (fun w -> relocate_abstract_word_left w p_left p_right)
        p_left.c_data
    in
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun linked_code_right ->
      mbind (Obj.magic (fun _ _ -> error_bind)) (fun linked_data_right ->
        let relocated_right_exports =
          fmap
            (Obj.magic (fun _ _ -> gmap_fmap string_eq_dec string_countable))
            (fun e -> relocate_export_right e p_left p_right)
            p_right.c_exports
        in
        let relocated_left_exports =
          fmap
            (Obj.magic (fun _ _ -> gmap_fmap string_eq_dec string_countable))
            (fun e -> relocate_export_left e p_left p_right) p_left.c_exports
        in
        let max_otype_left =
          get_max_otype (app relocated_left_code relocated_left_data)
        in
        let otypes_code_right =
          offsets_otype h0 linked_code_right max_otype_left
        in
        let otypes_data_right =
          offsets_otype h0 linked_data_right max_otype_left
        in
        let relocated_right_init =
          fmap
            (Obj.magic (fun _ _ -> gmap_fmap string_eq_dec string_countable))
            (fun i -> relocate_init_right i p_left p_right) p_right.c_init
        in
        let offset_left_data =
          length (app relocated_left_code otypes_code_right)
        in
        mbind (Obj.magic (fun _ _ -> error_bind))
          (fun initialized_data_left -> Ok { c_code =
          (app relocated_left_code otypes_code_right); c_data =
          (app initialized_data_left otypes_data_right); c_main = main_word;
          c_exports =
          (union0
            (map_union
              (Obj.magic (fun _ _ _ ->
                gmap_merge string_eq_dec string_countable)))
            relocated_left_exports relocated_right_exports); c_imports =
          (union0
            (map_union
              (Obj.magic (fun _ _ _ ->
                gmap_merge string_eq_dec string_countable))) p_left.c_imports
            p_right.c_imports); c_init =
          (union0
            (map_union
              (Obj.magic (fun _ _ _ ->
                gmap_merge string_eq_dec string_countable))) p_left.c_init
            p_right.c_init) })
          (Obj.magic initialize_symbols relocated_right_init
            relocated_left_exports relocated_left_code relocated_left_data
            offset_left_data))
        (Obj.magic resolve_data h0 relocated_right_data relocated_left_code
          relocated_left_data p_left.c_exports p_right.c_imports))
      (Obj.magic resolve_data h0 relocated_right_code relocated_left_code
        relocated_left_data p_left.c_exports p_right.c_imports))
    (Obj.magic resolve_main p_left p_right)

(** val instantiation' :
    compilerParameters -> cerise_linkable_object list ->
    cerise_linkable_object error **)

let rec instantiation' h0 = function
| [] ->
  error_msg1
    ('['::('i'::('n'::('s'::('t'::('a'::('n'::('t'::('i'::('a'::('t'::('i'::('o'::('n'::(']'::(' '::('r'::('e'::('q'::('u'::('i'::('r'::('e'::('s'::(' '::('a'::('t'::(' '::('l'::('e'::('a'::('s'::('t'::(' '::('o'::('n'::('e'::(' '::('c'::('e'::('r'::('i'::('s'::('e'::(' '::('o'::('b'::('j'::('e'::('c'::('t'::[])))))))))))))))))))))))))))))))))))))))))))))))))))
| p_left :: p_right' ->
  (match p_right' with
   | [] -> Ok p_left
   | _ :: _ ->
     mbind (Obj.magic (fun _ _ -> error_bind)) (fun p_right ->
       link_seq h0 p_left p_right) (instantiation' h0 p_right'))

(** val instantiation :
    compilerParameters -> cerise_linkable_object list ->
    cerise_linkable_object error **)

let instantiation h0 p_list =
  instantiation' h0 (rev1 p_list)

(** val error_msg2 : char list -> 'a1 error **)

let error_msg2 s =
  Error
    (append
      ('['::('c'::('e'::('r'::('i'::('s'::('e'::('_'::('l'::('o'::('a'::('d'::('e'::('r'::(']'::(' '::[]))))))))))))))))
      s)

(** val common_module :
    machineParameters -> compilerParameters -> cerise_linkable_object **)

let common_module mP cP =
  let allocate_mem = fun z0 n0 -> repeat (WInt z0) n0 in
  let safe_mem_sym =
    '_'::('C'::('o'::('m'::('m'::('o'::('n'::('.'::('s'::('a'::('f'::('e'::('_'::('m'::('e'::('m'::[])))))))))))))))
  in
  let link_tbl_sym =
    '_'::('C'::('o'::('m'::('m'::('o'::('n'::('.'::('l'::('i'::('n'::('k'::('_'::('t'::('b'::('l'::[])))))))))))))))
  in
  let trusted_macros_code =
    let lin_mem_macros =
      let sealing_lin_mem = WSealable (SSealRange (((true, true), Global),
        cP.otype_lin_mem,
        (add cP.otype_lin_mem (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
        cP.otype_lin_mem))
      in
      app (sealing_lin_mem :: [])
        (app (encodeInstrsW mP load_lin_mem)
          (app (encodeInstrsW mP (store_lin_mem mP))
            (app (encodeInstrsW mP (grow_lin_mem mP cP))
              (encodeInstrsW mP (current_lin_mem mP cP)))))
    in
    let global_macros =
      let sealing_global = WSealable (SSealRange (((true, true), Global),
        cP.otype_global,
        (add cP.otype_global (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
        cP.otype_global))
      in
      app (sealing_global :: [])
        (app (encodeInstrsW mP load_global)
          (app (encodeInstrsW mP (store_global mP))
            (app (encodeInstrsW mP (get_type_global mP))
              (encodeInstrsW mP (get_mut_global mP)))))
    in
    let safe_mem_macros =
      let sealing_safe_mem = WSealable (SSealRange (((true, true), Global),
        cP.otype_safe_mem,
        (add cP.otype_safe_mem (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)), cP.otype_safe_mem))
      in
      app (sealing_safe_mem :: []) (encodeInstrsW mP malloc_safe_mem)
    in
    app lin_mem_macros (app global_macros safe_mem_macros)
  in
  let linking_table =
    let offset_lin_mem_macros = Big_int_Z.zero_big_int in
    let offset_lin_mem_load0 =
      add offset_lin_mem_macros entry_point_load_lin_mem
    in
    let offset_lin_mem_store0 =
      add offset_lin_mem_macros entry_point_store_lin_mem
    in
    let offset_lin_mem_grow0 =
      add offset_lin_mem_macros (entry_point_grow_lin_mem mP)
    in
    let offset_lin_mem_current0 =
      add offset_lin_mem_macros (entry_point_current_lin_mem mP cP)
    in
    let offset_global_macros =
      add offset_lin_mem_current0 (length (current_lin_mem mP cP))
    in
    let offset_global_load0 = add offset_global_macros entry_point_load_global
    in
    let offset_global_store0 =
      add offset_global_macros entry_point_store_global
    in
    let offset_global_get_type =
      add offset_global_macros (entry_point_get_type_global mP)
    in
    let offset_global_get_mut =
      add offset_global_macros (entry_point_get_mut_global mP)
    in
    let offset_safe_mem_macros =
      add offset_global_get_mut (length (get_type_global mP))
    in
    let offset_safe_mem_malloc0 =
      add offset_safe_mem_macros entry_point_malloc_safe_mem
    in
    let offset_safe_mem_end =
      add offset_safe_mem_malloc0 (length malloc_safe_mem)
    in
    let lin_mem_sentry = fun offset_entry -> WSealable (SCap ((E, Global),
      offset_lin_mem_macros, (Finite offset_global_macros), offset_entry))
    in
    let global_sentry = fun offset_entry -> WSealable (SCap ((E, Global),
      offset_global_macros, (Finite offset_safe_mem_macros), offset_entry))
    in
    let safe_mem_sentry = fun offset_entry -> WSealable (SCap ((E, Global),
      offset_safe_mem_macros, (Finite offset_safe_mem_end), offset_entry))
    in
    (lin_mem_sentry offset_lin_mem_load0) :: ((lin_mem_sentry
                                                offset_lin_mem_store0) :: (
    (lin_mem_sentry offset_lin_mem_grow0) :: ((lin_mem_sentry
                                                offset_lin_mem_current0) :: (
    (global_sentry offset_global_load0) :: ((global_sentry
                                              offset_global_store0) :: (
    (global_sentry offset_global_get_type) :: ((global_sentry
                                                 offset_global_get_mut) :: (
    (safe_mem_sentry offset_safe_mem_malloc0) :: []))))))))
  in
  let start_link_tbl = length trusted_macros_code in
  let start_safe_mem = add start_link_tbl (length linking_table) in
  let end_safe_mem =
    add start_safe_mem
      (add (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
        cP.max_size_safe_mem)
  in
  let safe_mem =
    let free_space_malloc_cap = WSealable (SCap ((RW, Global),
      start_safe_mem, (Finite end_safe_mem),
      (add start_safe_mem (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))
    in
    free_space_malloc_cap :: (allocate_mem Big_int_Z.zero_big_int
                               cP.max_size_safe_mem)
  in
  let link_tbl_ro = WSealable (SCap ((RO, Global), start_link_tbl, (Finite
    start_safe_mem), start_link_tbl))
  in
  let safe_mem_sealed = WSealed (cP.otype_safe_mem, (SCap ((RW, Global),
    start_safe_mem, (Finite end_safe_mem), start_safe_mem)))
  in
  let exports_data = link_tbl_ro :: (safe_mem_sealed :: []) in
  let offset_exports_data = length (app linking_table safe_mem) in
  { c_code = (map (fun x -> Inl x) trusted_macros_code); c_data =
  (map (fun x -> Inl x) (app linking_table (app safe_mem exports_data)));
  c_main = None; c_exports =
  (insert0 (map_insert (gmap_partial_alter string_eq_dec string_countable))
    link_tbl_sym (Data, offset_exports_data)
    (singletonM0
      (map_singleton (gmap_partial_alter string_eq_dec string_countable)
        (gmap_empty string_eq_dec string_countable)) safe_mem_sym (Data,
      (add offset_exports_data (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))); c_imports =
  (gmap_empty string_eq_dec string_countable); c_init =
  (gmap_empty string_eq_dec string_countable) }

(** val get_word : abstract_word -> word error **)

let get_word = function
| Inl w -> Ok w
| Inr _ ->
  error_msg2
    ('['::('g'::('e'::('t'::('_'::('w'::('o'::('r'::('d'::(']'::(' '::('i'::('m'::('p'::('o'::('s'::('s'::('i'::('b'::('l'::('e'::(' '::('t'::('o'::(' '::('l'::('o'::('a'::('d'::(' '::('a'::('n'::(' '::('o'::('p'::('e'::('n'::(' '::('p'::('r'::('o'::('g'::('r'::('a'::('m'::[])))))))))))))))))))))))))))))))))))))))))))))

(** val loadable : abstract_word list -> word list error **)

let rec loadable = function
| [] -> Ok []
| ws :: l' ->
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun acc ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun w -> Ok (w :: acc))
      (Obj.magic get_word ws)) (loadable l')

(** val get_main_word :
    cerise_linkable_object -> word list -> word list -> word error **)

let get_main_word linked_obj code data =
  match linked_obj.c_main with
  | Some s ->
    let (sec, idx) = s in
    let sec_data = match sec with
                   | Code -> code
                   | Data -> data in
    (match nth_error sec_data idx with
     | Some w -> Ok w
     | None ->
       error_msg2
         ('['::('g'::('e'::('t'::('_'::('m'::('a'::('i'::('n'::('_'::('w'::('o'::('r'::('d'::(']'::(':'::(' '::('n'::('o'::(' '::('w'::('o'::('r'::('d'::(' '::('f'::('o'::('u'::('n'::('d'::(' '::('a'::('t'::(' '::('t'::('h'::('e'::(' '::('g'::('i'::('v'::('e'::('n'::(' '::('o'::('f'::('f'::('s'::('e'::('t'::[])))))))))))))))))))))))))))))))))))))))))))))))))))
  | None ->
    error_msg2
      ('['::('g'::('e'::('t'::('_'::('m'::('a'::('i'::('n'::('_'::('w'::('o'::('r'::('d'::(']'::(':'::(' '::('i'::('m'::('p'::('o'::('s'::('s'::('i'::('b'::('l'::('e'::(' '::('t'::('o'::(' '::('l'::('o'::('a'::('d'::(' '::('a'::(' '::('c'::('e'::('r'::('i'::('s'::('e'::(' '::('o'::('b'::('j'::('e'::('c'::('t'::(' '::('w'::('i'::('t'::('h'::('o'::('u'::('t'::(' '::('m'::('a'::('i'::('n'::[]))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))))

(** val linkable_to_executable :
    machineParameters -> compilerParameters -> cerise_linkable_object ->
    cerise_executable_object error **)

let linkable_to_executable mP cP cerise_obj =
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun linked_obj ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun code ->
      mbind (Obj.magic (fun _ _ -> error_bind)) (fun data ->
        mbind (Obj.magic (fun _ _ -> error_bind)) (fun main_word -> Ok
          { segment = (app code data); main = main_word })
          (Obj.magic get_main_word linked_obj code data))
        (Obj.magic loadable linked_obj.c_data))
      (Obj.magic loadable linked_obj.c_code))
    (Obj.magic link_seq cP (common_module mP cP) cerise_obj)

(** val boot_code : machineParameters -> word list **)

let boot_code h =
  encodeInstrsW h ((StoreU (STK, (Inl Big_int_Z.zero_big_int), (Inr (R
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))) :: ((Lea ((R
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
    Big_int_Z.unit_big_int))) :: ((StoreU (STK, (Inl Big_int_Z.zero_big_int),
    (Inr STK))) :: ((Load ((R (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)), (R Big_int_Z.zero_big_int))) :: ((Lea ((R
    Big_int_Z.zero_big_int), (Inl Big_int_Z.unit_big_int))) :: ((Load ((R
    Big_int_Z.zero_big_int), (R Big_int_Z.zero_big_int))) :: ((Invoke ((R
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (R
    Big_int_Z.zero_big_int))) :: [])))))))

(** val boot_code_section : machineParameters -> (addr, word) gmap **)

let boot_code_section h =
  list_to_mem (boot_code h) Big_int_Z.zero_big_int

(** val init_state :
    machineParameters -> cerise_executable_object -> Big_int_Z.big_int ->
    (regName, word) gmap * (addr, word) gmap **)

let init_state h prog start_stack =
  let loaded_prog = list_to_mem prog.segment Big_int_Z.zero_big_int in
  let len_boot = length (boot_code h) in
  let pre_heap =
    union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq0_Nat.eq_dec nat_countable)))
      (boot_code_section h) (shift_segment loaded_prog len_boot)
  in
  let len_pre_heap =
    length (gmap_to_list Coq0_Nat.eq_dec nat_countable pre_heap)
  in
  let end_code = encodeInstrsW h (Halt :: []) in
  let end_code_section = list_to_mem end_code len_pre_heap in
  let pc_cap = WSealable (SCap ((RX, Global), Big_int_Z.zero_big_int, (Finite
    len_boot), Big_int_Z.zero_big_int))
  in
  let end_cap = WSealable (SCap ((RX, Global), len_pre_heap, (Finite
    (add len_pre_heap (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
    len_pre_heap))
  in
  let main_cap = shift_word prog.main len_boot in
  let stk_cap = WSealable (SCap ((URWLX, Directed), start_stack, P_infty,
    start_stack))
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
        (Obj.magic (fun _ _ _ -> gmap_merge Coq0_Nat.eq_dec nat_countable)))
      pre_heap end_code_section
  in
  (regfile, heap)

(** val compile :
    machineParameters -> compilerParameters -> ws_module -> name ->
    cerise_linkable_object error **)

let compile mP cP m n0 =
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun labeled ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun reg_allocated ->
      compile_component mP reg_allocated)
      (Obj.magic register_allocation labeled))
    (Obj.magic compile_module cP mP m n0)

(** val compile_list :
    machineParameters -> compilerParameters -> (ws_module * name,
    cerise_linkable_object) sum list -> cerise_linkable_object error list **)

let compile_list mP cP ml =
  map (fun m ->
    match m with
    | Inl m' -> compile mP cP (fst m') (snd m')
    | Inr m' -> Ok m') ml

(** val load_in_memory :
    machineParameters -> compilerParameters -> Big_int_Z.big_int ->
    (ws_module * name, cerise_linkable_object) sum list -> ((regName, word)
    gmap * (addr, word) gmap) error **)

let load_in_memory mP cP start_stack modules =
  mbind (Obj.magic (fun _ _ -> error_bind)) (fun comps ->
    mbind (Obj.magic (fun _ _ -> error_bind)) (fun linked ->
      mbind (Obj.magic (fun _ _ -> error_bind)) (fun inst -> Ok
        (init_state mP inst start_stack))
        (Obj.magic linkable_to_executable mP cP linked))
      (Obj.magic instantiation cP comps))
    (Obj.magic list_error (compile_list mP cP modules))

(** val bank_prog : ws_basic_instruction list **)

let bank_prog =
  let account_ptr = Big_int_Z.zero_big_int in
  let account_id = Big_int_Z.succ_big_int Big_int_Z.zero_big_int in
  let account_balance = Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)
  in
  (BI_const (Val_int (Big_int_Z.mult_int_big_int 2
  (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
  Big_int_Z.unit_big_int))))) :: (BI_segalloc :: ((BI_set_local
  account_ptr) :: ((BI_get_local account_ptr) :: ((BI_const (Val_int
  Big_int_Z.zero_big_int)) :: ((BI_const (Val_int
  (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
  Big_int_Z.unit_big_int)))) :: (BI_slice :: ((BI_set_local
  account_id) :: ((BI_get_local account_ptr) :: ((BI_const (Val_int
  (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
  Big_int_Z.unit_big_int)))) :: ((BI_const (Val_int
  Big_int_Z.zero_big_int)) :: (BI_slice :: ((BI_set_local
  account_balance) :: ((BI_get_local account_balance) :: ((BI_const (Val_int
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  Big_int_Z.unit_big_int))))) :: ((BI_segstore T_int) :: ((BI_get_local
  account_id) :: ((BI_call
  Big_int_Z.zero_big_int) :: (BI_drop :: ((BI_get_local
  account_balance) :: ((BI_segload T_int) :: ((BI_const (Val_int
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  Big_int_Z.unit_big_int))))) :: ((BI_call (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))) :: (BI_drop :: [])))))))))))))))))))))))

(** val assert_prog : ws_basic_instruction list **)

let assert_prog =
  (BI_get_local Big_int_Z.zero_big_int) :: ((BI_get_local
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((BI_relop (T_int,
    ROI_ne)) :: ((BI_set_global Big_int_Z.zero_big_int) :: [])))

(** val bank_tf : function_type **)

let bank_tf =
  Tf ([], [])

(** val assert_tf : function_type **)

let assert_tf =
  Tf ((T_int :: (T_int :: [])), [])

(** val adv_tf : function_type **)

let adv_tf =
  Tf ((T_handle :: []), (T_int :: []))

(** val bank_module : ws_module **)

let bank_module =
  { mod_types = (bank_tf :: (assert_tf :: (adv_tf :: []))); mod_funcs =
    ({ modfunc_type = Big_int_Z.zero_big_int; modfunc_locals =
    (T_handle :: (T_handle :: (T_handle :: []))); modfunc_body =
    bank_prog } :: ({ modfunc_type = (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int); modfunc_locals = []; modfunc_body =
    assert_prog } :: [])); mod_tables = []; mod_mems = []; mod_globals =
    ({ modglob_type = { tg_mut = MUT_mut; tg_t = T_int }; modglob_init =
    (Val_int Big_int_Z.zero_big_int) } :: []); mod_elem = []; mod_start =
    (Some (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)); mod_imports =
    ({ imp_module = ('E'::('n'::('v'::[]))); imp_name =
    ('a'::('d'::('v'::[]))); imp_desc = (ID_func (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) } :: []); mod_exports =
    [] }

(** val env_adv_prog : ws_basic_instruction list **)

let env_adv_prog =
  (BI_get_local Big_int_Z.zero_big_int) :: (BI_drop :: ((BI_const (Val_int
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2
    Big_int_Z.unit_big_int))))) :: (BI_return :: [])))

(** val env_module : ws_module **)

let env_module =
  { mod_types = (adv_tf :: []); mod_funcs = ({ modfunc_type =
    Big_int_Z.zero_big_int; modfunc_locals = []; modfunc_body =
    env_adv_prog } :: []); mod_tables = []; mod_mems = []; mod_globals = [];
    mod_elem = []; mod_start = None; mod_imports = []; mod_exports =
    ({ modexp_name = ('a'::('d'::('v'::[]))); modexp_desc = (MED_func
    Big_int_Z.zero_big_int) } :: []) }

(** val bank_unsafe_prog : ws_basic_instruction list **)

let bank_unsafe_prog =
  let account_ptr = Big_int_Z.zero_big_int in
  let account_id = Big_int_Z.succ_big_int Big_int_Z.zero_big_int in
  let account_balance = Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)
  in
  let adv_fun = Big_int_Z.zero_big_int in
  (BI_const (Val_int
  Big_int_Z.unit_big_int)) :: (BI_grow_memory :: ((BI_const (Val_int
  Big_int_Z.zero_big_int)) :: ((BI_set_local account_ptr) :: ((BI_get_local
  account_ptr) :: ((BI_set_local account_id) :: ((BI_const (Val_int
  (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
  Big_int_Z.unit_big_int)))) :: ((BI_get_local account_ptr) :: ((BI_binop
  (T_int, BOI_add)) :: ((BI_set_local account_balance) :: ((BI_get_local
  account_balance) :: ((BI_const (Val_int
  Big_int_Z.zero_big_int)) :: ((BI_store T_int) :: ((BI_get_local
  account_id) :: ((BI_call adv_fun) :: ((BI_get_local
  account_balance) :: ((BI_load T_int) :: (BI_drop :: [])))))))))))))))))

(** val adv_unsafe_tf : function_type **)

let adv_unsafe_tf =
  Tf ((T_int :: []), [])

(** val bank_unsafe_module : ws_module **)

let bank_unsafe_module =
  { mod_types = (bank_tf :: (adv_unsafe_tf :: [])); mod_funcs =
    ({ modfunc_type = Big_int_Z.zero_big_int; modfunc_locals =
    (T_int :: (T_int :: (T_int :: []))); modfunc_body =
    bank_unsafe_prog } :: []); mod_tables = []; mod_mems = []; mod_globals =
    []; mod_elem = []; mod_start = (Some (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)); mod_imports = ({ imp_module =
    ('E'::('n'::('v'::[]))); imp_name =
    ('a'::('d'::('v'::('_'::('u'::('n'::('s'::('a'::('f'::('e'::[]))))))))));
    imp_desc = (ID_func (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)) } :: ({ imp_module = ('E'::('n'::('v'::[])));
    imp_name = ('m'::('e'::('m'::[]))); imp_desc = (ID_mem { lim_min =
    Big_int_Z.zero_big_int; lim_max = None }) } :: [])); mod_exports = [] }

(** val env_adv_unsafe_prog : ws_basic_instruction list **)

let env_adv_unsafe_prog =
  let account_id_ptr = Big_int_Z.zero_big_int in
  let account_balance_ptr = Big_int_Z.succ_big_int Big_int_Z.zero_big_int in
  (BI_get_local account_id_ptr) :: ((BI_const (Val_int
  (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
  Big_int_Z.unit_big_int)))) :: ((BI_binop (T_int,
  BOI_add)) :: ((BI_set_local account_balance_ptr) :: ((BI_get_local
  account_balance_ptr) :: ((BI_const (Val_int (Big_int_Z.mult_int_big_int 2
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  (Big_int_Z.mult_int_big_int 2
  ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
  (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))))) :: ((BI_store
  T_int) :: []))))))

(** val env_unsafe_module : ws_module **)

let env_unsafe_module =
  { mod_types = (adv_unsafe_tf :: []); mod_funcs = ({ modfunc_type =
    Big_int_Z.zero_big_int; modfunc_locals = []; modfunc_body =
    env_adv_unsafe_prog } :: []); mod_tables = []; mod_mems = ({ lim_min =
    Big_int_Z.zero_big_int; lim_max = None } :: []); mod_globals = [];
    mod_elem = []; mod_start = None; mod_imports = []; mod_exports =
    ({ modexp_name =
    ('a'::('d'::('v'::('_'::('u'::('n'::('s'::('a'::('f'::('e'::[]))))))))));
    modexp_desc = (MED_func Big_int_Z.zero_big_int) } :: ({ modexp_name =
    ('m'::('e'::('m'::[]))); modexp_desc = (MED_mem
    Big_int_Z.zero_big_int) } :: [])) }

(** val z_page_size0 : compilerParameters -> Big_int_Z.big_int **)

let z_page_size0 cP =
  Z.of_nat cP.page_size

(** val new_stack : compilerParameters -> ws_basic_instruction list **)

let new_stack cP =
  (BI_const (Val_int
    Big_int_Z.unit_big_int)) :: (BI_grow_memory :: ((BI_tee_local
    Big_int_Z.zero_big_int) :: ((BI_const (Val_int (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int))) :: ((BI_relop (T_int, ROI_eq)) :: ((BI_if
    ((T_int :: []), ((BI_const (Val_int (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int))) :: []), ((BI_get_local
    Big_int_Z.zero_big_int) :: ((BI_const (Val_int
    (z_page_size0 cP))) :: ((BI_binop (T_int, BOI_mul)) :: ((BI_tee_local
    Big_int_Z.zero_big_int) :: ((BI_get_local
    Big_int_Z.zero_big_int) :: ((BI_store T_int) :: ((BI_get_local
    Big_int_Z.zero_big_int) :: []))))))))) :: [])))))

(** val validate_stack :
    compilerParameters -> immediate -> ws_basic_instruction list **)

let validate_stack cP x =
  (BI_get_local x) :: ((BI_const (Val_int (z_page_size0 cP))) :: ((BI_binop
    (T_int, (BOI_rem SX_S))) :: ((BI_if ([], (BI_unreachable :: []),
    [])) :: [])))

(** val validate_stack_bound : immediate -> ws_basic_instruction list **)

let validate_stack_bound x =
  (BI_get_local x) :: ((BI_load T_int) :: (BI_drop :: []))

(** val is_full_op : compilerParameters -> ws_basic_instruction list **)

let is_full_op cP =
  (BI_const (Val_int Big_int_Z.unit_big_int)) :: ((BI_const (Val_int
    Big_int_Z.zero_big_int)) :: ((BI_get_local
    Big_int_Z.zero_big_int) :: ((BI_load T_int) :: ((BI_const (Val_int
    (z_page_size0 cP))) :: ((BI_binop (T_int, (BOI_rem SX_S))) :: ((BI_const
    (Val_int
    (Z.sub (z_page_size0 cP) (Big_int_Z.mult_int_big_int 2
      (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))) :: ((BI_relop
    (T_int, ROI_eq)) :: (BI_select :: []))))))))

(** val is_full : compilerParameters -> ws_basic_instruction list **)

let is_full cP =
  app (validate_stack cP Big_int_Z.zero_big_int)
    (app (validate_stack_bound Big_int_Z.zero_big_int) (is_full_op cP))

(** val is_empty_op : ws_basic_instruction list **)

let is_empty_op =
  (BI_get_local Big_int_Z.zero_big_int) :: ((BI_get_local
    Big_int_Z.zero_big_int) :: ((BI_load T_int) :: ((BI_relop (T_int,
    ROI_eq)) :: [])))

(** val is_empty : compilerParameters -> ws_basic_instruction list **)

let is_empty cP =
  app (validate_stack cP Big_int_Z.zero_big_int)
    (app (validate_stack_bound Big_int_Z.zero_big_int) is_empty_op)

(** val push_op : compilerParameters -> ws_basic_instruction list **)

let push_op cP =
  app (is_full_op cP) ((BI_if ([], (BI_unreachable :: []),
    [])) :: ((BI_get_local Big_int_Z.zero_big_int) :: ((BI_load
    T_int) :: ((BI_const (Val_int (Big_int_Z.mult_int_big_int 2
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))) :: ((BI_binop
    (T_int, BOI_add)) :: ((BI_tee_local (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) :: ((BI_get_local
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((BI_store
    T_int) :: ((BI_get_local Big_int_Z.zero_big_int) :: ((BI_get_local
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) :: ((BI_store T_int) :: [])))))))))))

(** val push : compilerParameters -> ws_basic_instruction list **)

let push cP =
  app (validate_stack cP Big_int_Z.zero_big_int)
    (app (validate_stack_bound Big_int_Z.zero_big_int) (push_op cP))

(** val pop_op : ws_basic_instruction list **)

let pop_op =
  app is_empty_op ((BI_if ([], (BI_unreachable :: []), [])) :: ((BI_get_local
    Big_int_Z.zero_big_int) :: ((BI_load T_int) :: ((BI_tee_local
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((BI_load
    T_int) :: ((BI_get_local Big_int_Z.zero_big_int) :: ((BI_get_local
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((BI_const (Val_int
    (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    Big_int_Z.unit_big_int)))) :: ((BI_binop (T_int, BOI_sub)) :: ((BI_store
    T_int) :: []))))))))))

(** val pop : compilerParameters -> ws_basic_instruction list **)

let pop cP =
  app (validate_stack cP Big_int_Z.zero_big_int)
    (app (validate_stack_bound Big_int_Z.zero_big_int) pop_op)

(** val stack_length_op : ws_basic_instruction list **)

let stack_length_op =
  (BI_get_local Big_int_Z.zero_big_int) :: ((BI_load T_int) :: ((BI_get_local
    Big_int_Z.zero_big_int) :: ((BI_binop (T_int, BOI_sub)) :: ((BI_const
    (Val_int (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    Big_int_Z.unit_big_int)))) :: ((BI_binop (T_int, (BOI_div
    SX_U))) :: [])))))

(** val stack_length : compilerParameters -> ws_basic_instruction list **)

let stack_length cP =
  app (validate_stack cP Big_int_Z.zero_big_int)
    (app (validate_stack_bound Big_int_Z.zero_big_int) stack_length_op)

(** val map_initialise : ws_basic_instruction list **)

let map_initialise =
  (BI_get_local Big_int_Z.zero_big_int) :: ((BI_load T_int) :: ((BI_set_local
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) :: ((BI_get_local
    Big_int_Z.zero_big_int) :: ((BI_set_local (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) :: []))))

(** val map_loop_body : ws_basic_instruction list **)

let map_loop_body =
  (BI_get_local (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) :: ((BI_get_local (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) :: ((BI_relop (T_int, ROI_eq)) :: ((BI_br_if
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((BI_get_local
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) :: ((BI_const (Val_int
    (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    Big_int_Z.unit_big_int)))) :: ((BI_binop (T_int,
    BOI_add)) :: ((BI_tee_local (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) :: ((BI_get_local
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))) :: ((BI_load T_int) :: ((BI_get_local
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((BI_call_indirect
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((BI_store
    T_int) :: ((BI_br Big_int_Z.zero_big_int) :: [])))))))))))))

(** val map_op : ws_basic_instruction list **)

let map_op =
  app map_initialise ((BI_block ([], ((BI_loop ([],
    map_loop_body)) :: []))) :: [])

(** val stack_map : compilerParameters -> ws_basic_instruction list **)

let stack_map cP =
  app (validate_stack cP Big_int_Z.zero_big_int)
    (app (validate_stack_bound Big_int_Z.zero_big_int) map_op)

(** val main_stack : ws_basic_instruction list **)

let main_stack =
  (BI_call Big_int_Z.zero_big_int) :: ((BI_tee_local
    Big_int_Z.zero_big_int) :: ((BI_const (Val_int (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int))) :: ((BI_relop (T_int, ROI_eq)) :: ((BI_if ([],
    ((BI_const (Val_int (Big_int_Z.minus_big_int
    Big_int_Z.unit_big_int))) :: ((BI_set_global
    Big_int_Z.zero_big_int) :: (BI_return :: []))), [])) :: ((BI_get_local
    Big_int_Z.zero_big_int) :: ((BI_const (Val_int
    (Big_int_Z.mult_int_big_int 2 (Big_int_Z.mult_int_big_int 2
    Big_int_Z.unit_big_int)))) :: ((BI_call (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((BI_get_local
    Big_int_Z.zero_big_int) :: ((BI_const (Val_int
    (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int)))) :: ((BI_call (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((BI_get_local
    Big_int_Z.zero_big_int) :: ((BI_const (Val_int
    Big_int_Z.zero_big_int)) :: ((BI_call (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))) :: ((BI_get_local
    Big_int_Z.zero_big_int) :: ((BI_call (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) :: ((BI_get_local
    Big_int_Z.zero_big_int) :: ((BI_call (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))) :: ((BI_binop (T_int,
    BOI_sub)) :: ((BI_set_global
    Big_int_Z.zero_big_int) :: [])))))))))))))))))))

(** val square0 : ws_basic_instruction list **)

let square0 =
  (BI_get_local Big_int_Z.zero_big_int) :: ((BI_get_local
    Big_int_Z.zero_big_int) :: ((BI_binop (T_int, BOI_mul)) :: []))

(** val stack_module : compilerParameters -> ws_module **)

let stack_module cP =
  { mod_types = ((Tf ([], (T_int :: []))) :: ((Tf ((T_int :: []),
    (T_int :: []))) :: ((Tf ((T_int :: (T_int :: [])), [])) :: [])));
    mod_funcs = ({ modfunc_type = Big_int_Z.zero_big_int; modfunc_locals =
    (T_int :: []); modfunc_body = (new_stack cP) } :: ({ modfunc_type =
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int); modfunc_locals = [];
    modfunc_body = (is_empty cP) } :: ({ modfunc_type =
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int); modfunc_locals = [];
    modfunc_body = (is_full cP) } :: ({ modfunc_type =
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int); modfunc_locals =
    (T_int :: []); modfunc_body = (pop cP) } :: ({ modfunc_type =
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int));
    modfunc_locals = (T_int :: []); modfunc_body =
    (push cP) } :: ({ modfunc_type = (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)); modfunc_locals =
    (T_int :: (T_int :: [])); modfunc_body =
    (stack_map cP) } :: ({ modfunc_type = (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int); modfunc_locals = []; modfunc_body =
    (stack_length cP) } :: []))))))); mod_tables = ({ lim_min =
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int); lim_max = None } :: []);
    mod_mems = ({ lim_min = Big_int_Z.zero_big_int; lim_max = None } :: []);
    mod_globals = []; mod_elem = []; mod_start = None; mod_imports = [];
    mod_exports = ({ modexp_name =
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
    modfunc_body = square0 } :: [])); mod_tables = []; mod_mems = [];
    mod_globals = ({ modglob_type = { tg_mut = MUT_mut; tg_t = T_int };
    modglob_init = (Val_int Big_int_Z.zero_big_int) } :: []); mod_elem =
    ({ modelem_table = Big_int_Z.zero_big_int; modelem_offset =
    Big_int_Z.zero_big_int; modelem_init = ((Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))))))) :: []) } :: []);
    mod_start = (Some (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
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

(** val tf_f_client : function_type **)

let tf_f_client =
  Tf ([], [])

(** val f_client : ws_basic_instruction list **)

let f_client =
  (BI_const (Val_int (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))))) :: ((BI_const
    (Val_int (Big_int_Z.minus_big_int
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int)))))) :: ((BI_call
    Big_int_Z.zero_big_int) :: ((BI_set_global
    Big_int_Z.zero_big_int) :: [])))

(** val tf_f_lib : function_type **)

let tf_f_lib =
  Tf ((T_int :: (T_int :: [])), (T_int :: []))

(** val f_lib : ws_basic_instruction list **)

let f_lib =
  (BI_get_local Big_int_Z.zero_big_int) :: ((BI_get_local
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((BI_binop (T_int,
    BOI_add)) :: ((BI_if ((T_int :: []), ((BI_const (Val_int
    Big_int_Z.zero_big_int)) :: []), ((BI_const (Val_int
    Big_int_Z.unit_big_int)) :: []))) :: [])))

(** val dummy_module_lib : ws_module **)

let dummy_module_lib =
  { mod_types = (tf_f_lib :: []); mod_funcs = ({ modfunc_type =
    Big_int_Z.zero_big_int; modfunc_locals = []; modfunc_body =
    f_lib } :: []); mod_tables = []; mod_mems = []; mod_globals = [];
    mod_elem = []; mod_start = None; mod_imports = []; mod_exports =
    ({ modexp_name = ('f'::('_'::('l'::('i'::('b'::[]))))); modexp_desc =
    (MED_func Big_int_Z.zero_big_int) } :: []) }

(** val dummy_module_client : ws_module **)

let dummy_module_client =
  { mod_types = (tf_f_lib :: (tf_f_client :: [])); mod_funcs =
    ({ modfunc_type = (Big_int_Z.succ_big_int Big_int_Z.zero_big_int);
    modfunc_locals = (T_int :: (T_int :: [])); modfunc_body =
    f_client } :: []); mod_tables = []; mod_mems = []; mod_globals =
    ({ modglob_type = { tg_mut = MUT_mut; tg_t = T_int }; modglob_init =
    (Val_int Big_int_Z.zero_big_int) } :: []); mod_elem = []; mod_start =
    (Some (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)); mod_imports =
    ({ imp_module = ('e'::('n'::('v'::[]))); imp_name =
    ('f'::('_'::('l'::('i'::('b'::[]))))); imp_desc = (ID_func
    Big_int_Z.zero_big_int) } :: []); mod_exports = ({ modexp_name =
    ('a'::('n'::('s'::('w'::('e'::('r'::[])))))); modexp_desc = (MED_global
    Big_int_Z.zero_big_int) } :: []) }

(** val tf_f_client0 : function_type **)

let tf_f_client0 =
  Tf ([], [])

(** val f_client0 : Big_int_Z.big_int -> ws_basic_instruction list **)

let f_client0 n0 =
  app (repeat (BI_const (Val_int Big_int_Z.unit_big_int)) n0)
    (app
      (repeat (BI_binop (T_int, BOI_add))
        (sub n0 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
      (BI_drop :: ((BI_const (Val_int (Big_int_Z.mult_int_big_int 2
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (Big_int_Z.mult_int_big_int 2
      Big_int_Z.unit_big_int))))) :: (BI_drop :: []))))

(** val reg_alloc_module_client : ws_module **)

let reg_alloc_module_client =
  let n0 = Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
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
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))))))))))))))))))))))))))
  in
  { mod_types = (tf_f_client0 :: []); mod_funcs = ({ modfunc_type =
  Big_int_Z.zero_big_int; modfunc_locals = []; modfunc_body =
  (f_client0 n0) } :: []); mod_tables = []; mod_mems = []; mod_globals = [];
  mod_elem = []; mod_start = (Some Big_int_Z.zero_big_int); mod_imports = [];
  mod_exports = [] }

(** val incr_prog : ws_basic_instruction list **)

let incr_prog =
  (BI_get_global Big_int_Z.zero_big_int) :: ((BI_const (Val_int
    Big_int_Z.unit_big_int)) :: ((BI_binop (T_int,
    BOI_add)) :: ((BI_set_global Big_int_Z.zero_big_int) :: [])))

(** val incr_tf : function_type **)

let incr_tf =
  Tf ([], [])

(** val incr_module : ws_module **)

let incr_module =
  { mod_types = (incr_tf :: []); mod_funcs = ({ modfunc_type =
    Big_int_Z.zero_big_int; modfunc_locals = []; modfunc_body =
    incr_prog } :: []); mod_tables = []; mod_mems = []; mod_globals =
    ({ modglob_type = { tg_mut = MUT_mut; tg_t = T_int }; modglob_init =
    (Val_int (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))) } :: []);
    mod_elem = []; mod_start = (Some Big_int_Z.zero_big_int); mod_imports =
    []; mod_exports = [] }

(** val main_prog : ws_basic_instruction list **)

let main_prog =
  (BI_get_global Big_int_Z.zero_big_int) :: ((BI_const (Val_int
    Big_int_Z.unit_big_int)) :: ((BI_binop (T_int,
    BOI_add)) :: ((BI_set_global Big_int_Z.zero_big_int) :: ((BI_call
    Big_int_Z.zero_big_int) :: []))))

(** val main_tf : function_type **)

let main_tf =
  Tf ([], [])

(** val main_module : ws_module **)

let main_module =
  { mod_types = (main_tf :: []); mod_funcs = ({ modfunc_type =
    Big_int_Z.zero_big_int; modfunc_locals = []; modfunc_body =
    main_prog } :: []); mod_tables = []; mod_mems = []; mod_globals =
    ({ modglob_type = { tg_mut = MUT_mut; tg_t = T_int }; modglob_init =
    (Val_int (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))) } :: []);
    mod_elem = []; mod_start = (Some (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)); mod_imports = ({ imp_module =
    ('E'::('n'::('v'::[]))); imp_name = ('a'::('d'::('v'::[]))); imp_desc =
    (ID_func Big_int_Z.zero_big_int) } :: ({ imp_module =
    ('E'::('n'::('v'::[]))); imp_name = ('h'::[]); imp_desc = (ID_global
    { tg_mut = MUT_mut; tg_t = T_int }) } :: [])); mod_exports =
    ({ modexp_name = ('g'::[]); modexp_desc = (MED_global
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) } :: []) }

(** val env_h : word list **)

let env_h =
  (WInt ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int)) :: ((WInt Big_int_Z.zero_big_int) :: ((WInt
    Big_int_Z.unit_big_int) :: []))

(** val env_frame :
    compilerParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    abstract_word list **)

let env_frame cP size_adv_fun offset_h =
  (Inl (WSealable (SCap ((E, Global), Big_int_Z.zero_big_int, (Finite
    size_adv_fun), (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))) :: ((Inl (WSealed (cP.otype_global, (SCap
    ((RW, Global), offset_h, (Finite
    (add offset_h (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))),
    offset_h))))) :: ((Inr
    ('M'::('a'::('i'::('n'::('.'::('g'::[]))))))) :: ((Inr
    ('_'::('C'::('o'::('m'::('m'::('o'::('n'::('.'::('l'::('i'::('n'::('k'::('_'::('t'::('b'::('l'::[]))))))))))))))))) :: [])))

(** val env_adv_fun : cerise_function **)

let env_adv_fun =
  (Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))), (Inr PC))) :: ((Lea ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))))), (Inl
    (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: ((Load ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))))), (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))))))) :: ((Lea ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))))), (Inl
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))) :: ((Load ((R
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))))))) :: ((Lea ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))))), (Inl
    Big_int_Z.unit_big_int))) :: ((Load ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))), (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))))) :: ((Lea ((R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))))))))))), (Inl
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))) :: ((Load ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))))), (R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))))))))) :: ((Mov ((R
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int))), (Inl
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))))) :: ((LoadU ((R Big_int_Z.zero_big_int),
    STK, (Inl (Big_int_Z.minus_big_int
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int))))) :: ((Jmp (R (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)))))))))))) :: [])))))))))))

(** val env_adv' :
    machineParameters -> compilerParameters -> Big_int_Z.big_int ->
    abstract_word list **)

let env_adv' mP cP offset_data =
  let s_data =
    length (env_frame cP Big_int_Z.zero_big_int Big_int_Z.zero_big_int)
  in
  let data_cap = WSealable (SCap ((RO, Global), offset_data, (Finite
    (add offset_data s_data)), offset_data))
  in
  let prog = data_cap :: (encodeInstrsW mP env_adv_fun) in
  map (fun x -> Inl x) prog

(** val env_adv :
    machineParameters -> compilerParameters -> abstract_word list **)

let env_adv mP cP =
  let o = length (env_adv' mP cP Big_int_Z.zero_big_int) in env_adv' mP cP o

(** val env_data :
    machineParameters -> compilerParameters -> abstract_word list **)

let env_data mP cP =
  let size_adv = length (env_adv mP cP) in
  let offset_h =
    add size_adv
      (length (env_frame cP Big_int_Z.zero_big_int Big_int_Z.zero_big_int))
  in
  app (env_frame cP size_adv offset_h) (map (fun x -> Inl x) env_h)

(** val env_object :
    machineParameters -> compilerParameters -> cerise_linkable_object **)

let env_object mP cP =
  { c_code = (env_adv mP cP); c_data = (env_data mP cP); c_main = None;
    c_exports =
    (insert0 (map_insert (gmap_partial_alter string_eq_dec string_countable))
      ('E'::('n'::('v'::('.'::('a'::('d'::('v'::[]))))))) (Data,
      Big_int_Z.zero_big_int)
      (singletonM0
        (map_singleton (gmap_partial_alter string_eq_dec string_countable)
          (gmap_empty string_eq_dec string_countable))
        ('E'::('n'::('v'::('.'::('h'::[]))))) (Data, (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int)))); c_imports =
    (gmap_empty string_eq_dec string_countable); c_init =
    (gmap_empty string_eq_dec string_countable) }

(** val load_test :
    machineParameters -> compilerParameters -> Big_int_Z.big_int ->
    (ws_module * name, cerise_linkable_object) sum list -> ((regName * word)
    list * (addr * word) list) error **)

let load_test mP cP start_stack modules =
  let loaded_mods = load_in_memory mP cP start_stack modules in
  (match loaded_mods with
   | Error m -> Error m
   | Ok a ->
     let (regs, mem) = a in
     Ok ((gmap_to_list reg_eq_dec reg_countable regs),
     (sort (gmap_to_list Coq0_Nat.eq_dec nat_countable mem))))

(** val load_example :
    machineParameters -> compilerParameters -> (ws_module * name,
    cerise_linkable_object) sum list -> Big_int_Z.big_int ->
    ((regName * word) list * (addr * word) list) error **)

let load_example mP cP modules start_stack =
  load_test mP cP start_stack modules

(** val loaded_bank_example :
    machineParameters -> compilerParameters -> Big_int_Z.big_int ->
    ((regName * word) list * (addr * word) list) error **)

let loaded_bank_example mP cP =
  load_example mP cP
    (map (fun x -> Inl x) ((bank_module,
      ('B'::('a'::('n'::('k'::[]))))) :: ((env_module,
      ('E'::('n'::('v'::[])))) :: [])))

(** val loaded_bank_unsafe_example :
    machineParameters -> compilerParameters -> Big_int_Z.big_int ->
    ((regName * word) list * (addr * word) list) error **)

let loaded_bank_unsafe_example mP cP =
  load_example mP cP
    (map (fun x -> Inl x) ((bank_unsafe_module,
      ('B'::('a'::('n'::('k'::[]))))) :: ((env_unsafe_module,
      ('E'::('n'::('v'::[])))) :: [])))

(** val loaded_stack_example :
    machineParameters -> compilerParameters -> Big_int_Z.big_int ->
    ((regName * word) list * (addr * word) list) error **)

let loaded_stack_example mP cP =
  load_example mP cP
    (map (fun x -> Inl x) ((client_module,
      ('C'::('l'::('i'::('e'::('n'::('t'::[]))))))) :: (((stack_module cP),
      ('S'::('t'::('a'::('c'::('k'::[])))))) :: [])))

(** val loaded_dummy_example :
    machineParameters -> compilerParameters -> Big_int_Z.big_int ->
    ((regName * word) list * (addr * word) list) error **)

let loaded_dummy_example mP cP =
  load_example mP cP
    (map (fun x -> Inl x) ((dummy_module_client,
      ('d'::('u'::('m'::('m'::('y'::[])))))) :: ((dummy_module_lib,
      ('e'::('n'::('v'::[])))) :: [])))

(** val loaded_reg_alloc_example :
    machineParameters -> compilerParameters -> Big_int_Z.big_int ->
    ((regName * word) list * (addr * word) list) error **)

let loaded_reg_alloc_example mP cP =
  load_example mP cP
    (map (fun x -> Inl x) ((reg_alloc_module_client,
      ('m'::('a'::('i'::('n'::[]))))) :: []))

(** val loaded_incr_example :
    machineParameters -> compilerParameters -> Big_int_Z.big_int ->
    ((regName * word) list * (addr * word) list) error **)

let loaded_incr_example mP cP =
  load_example mP cP
    (map (fun x -> Inl x) ((incr_module,
      ('i'::('n'::('c'::('r'::[]))))) :: []))

(** val loaded_external_example :
    machineParameters -> compilerParameters -> Big_int_Z.big_int ->
    ((regName * word) list * (addr * word) list) error **)

let loaded_external_example mP cP =
  load_example mP cP ((Inl (main_module,
    ('M'::('a'::('i'::('n'::[])))))) :: ((Inr (env_object mP cP)) :: []))

(** val gmap_insert :
    (symbols, 'a1) gmap -> symbols -> 'a1 -> (symbols, 'a1) gmap **)

let gmap_insert g k v =
  insert0 (map_insert (gmap_partial_alter string_eq_dec string_countable)) k
    v g
