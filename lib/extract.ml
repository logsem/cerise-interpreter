
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



(** val eqb0 : char list -> char list -> bool **)

let rec eqb0 s1 s2 =
  match s1 with
  | [] -> (match s2 with
           | [] -> true
           | _::_ -> false)
  | c1::s1' ->
    (match s2 with
     | [] -> false
     | c2::s2' -> if (=) c1 c2 then eqb0 s1' s2' else false)

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

(** val list_opt : 'a1 option list -> 'a1 list option **)

let list_opt l_opt =
  fold_right (fun e_opt acc_opt ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun e ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun acc -> Some (e :: acc))
        acc_opt) (Obj.magic e_opt)) (Some []) l_opt

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
| SCap of (perm * locality) * addr * addr * addr
| SSealRange of (sealPerms * locality) * oType * oType * oType

type word =
| WInt of Big_int_Z.big_int
| WSealable of sealable
| WSealed of oType * sealable

type cerise_instruction =
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
| Fail
| Halt
| LoadU of regName * regName * (Big_int_Z.big_int, regName) sum
| StoreU of regName * (Big_int_Z.big_int, regName) sum
   * (Big_int_Z.big_int, regName) sum
| PromoteU of regName

(** val nop : cerise_instruction **)

let nop =
  Mov ((R Big_int_Z.zero_big_int), (Inr (R Big_int_Z.zero_big_int)))

type reg = (regName, word) gmap

type mem = (addr, word) gmap

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
| LoadU (r1, r2, zr3) ->
  let tmp = max (get_reg_num r2) (get_zreg_num zr3) in
  max (get_reg_num r1) tmp
| StoreU (r1, zr2, zr3) ->
  let tmp = max (get_zreg_num zr2) (get_zreg_num zr3) in
  max (get_reg_num r1) tmp
| PromoteU r0 -> get_reg_num r0
| _ -> Big_int_Z.zero_big_int

type symbols = char list

(** val symbols_encode : char list -> char list -> symbols **)

let symbols_encode module_name symbol_name =
  append (append module_name ('.'::[])) symbol_name

type cerise_function = cerise_instruction list

type data = word list

type id0 = char list

type common_frame_entry =
| SafeMem
| LinkTbl

type frame_entry =
| Define of id0 * Big_int_Z.big_int
| Imports of symbols
| Common of common_frame_entry

type cerise_frame = { cframe_functions_imported : frame_entry list;
                      cframe_functions_defined : frame_entry list;
                      cframe_lin_mem_imported : frame_entry list;
                      cframe_lin_mem_defined : frame_entry list;
                      cframe_globals_imported : frame_entry list;
                      cframe_globals_defined : frame_entry list;
                      cframe_itable_imported : frame_entry list;
                      cframe_itable_defined : frame_entry list;
                      cframe_safe_mem : frame_entry list;
                      cframe_link_tbl : frame_entry list }

type cerise_subcomponent = { c_id : id0; c_functions : cerise_function list;
                             c_frame : cerise_frame; c_lin_mem : data list;
                             c_globals : data list;
                             c_indirect_table : data list;
                             c_elements : (Big_int_Z.big_int, frame_entry)
                                          gmap;
                             c_exports : (symbols, Big_int_Z.big_int) gmap }

type cerise_component = { c_subcomp : cerise_subcomponent list;
                          c_main : (id0 * Big_int_Z.big_int) option }

type cerise_program = { segment : (addr, word) gmap; main : word }

(** val list_to_mem : 'a1 list -> addr -> (addr, 'a1) gmap **)

let rec list_to_mem l a =
  match l with
  | [] -> gmap_empty Coq0_Nat.eq_dec nat_countable
  | h :: t0 ->
    insert0 (map_insert (gmap_partial_alter Coq0_Nat.eq_dec nat_countable)) a
      h
      (list_to_mem t0 (add a (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))

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
  | SCap (p0, b, e, a) -> SCap (p0, (add b n0), (add e n0), (add a n0))
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

(** val encodeInstrsW :
    machineParameters -> cerise_instruction list -> word list **)

let encodeInstrsW h =
  map (encodeInstrW h)

(** val wt_cap : word **)

let wt_cap =
  WSealable (SCap ((O, Global), Big_int_Z.zero_big_int,
    Big_int_Z.zero_big_int, Big_int_Z.zero_big_int))

(** val wt_sealrange : word **)

let wt_sealrange =
  WSealable (SSealRange (((false, false), Global), Big_int_Z.zero_big_int,
    Big_int_Z.zero_big_int, Big_int_Z.zero_big_int))

(** val wt_sealed : word **)

let wt_sealed =
  WSealed (Big_int_Z.zero_big_int, (SCap ((O, Global),
    Big_int_Z.zero_big_int, Big_int_Z.zero_big_int, Big_int_Z.zero_big_int)))

(** val wt_int : word **)

let wt_int =
  WInt Big_int_Z.zero_big_int

type labeled_instr =
| Label of Big_int_Z.big_int list
| BInstr of cerise_instruction
| Br_Jmp of Big_int_Z.big_int list
| Br_Jnz of Big_int_Z.big_int list * Big_int_Z.big_int

(** val instrs : cerise_instruction list -> labeled_instr list **)

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

type labeled_cerise_subcomponent = { l_id : id0;
                                     l_functions : labeled_function list;
                                     l_frame : cerise_frame;
                                     l_lin_mem : data list;
                                     l_globals : data list;
                                     l_indirect_table : data list;
                                     l_elements : (Big_int_Z.big_int,
                                                  frame_entry) gmap;
                                     l_exports : (symbols, Big_int_Z.big_int)
                                                 gmap }

type labeled_cerise_component = { l_subcomp : labeled_cerise_subcomponent list;
                                  l_main : (id0 * Big_int_Z.big_int) option }

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
  (Lt0 (res, (Inr r2), (Inr r1))) :: ((Mov (tmp, (Inr PC))) :: ((Lea (tmp,
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
    machineParameters -> regName -> regName list -> cerise_instruction list **)

let call_instrs h r0 rargs =
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
                    (app
                      (rclear_instrs
                        (list_difference reg_eq_dec all_registers
                          (PC :: (r0 :: (r_stk :: [])))))
                      (app ((Jmp r0) :: []) pop_env_instrs))))))))))

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
    machineParameters -> regName -> value_type -> cerise_instruction list **)

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

(** val malloc_subroutine_instrs : cerise_instruction list **)

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

(** val malloc_safe_mem : cerise_instruction list **)

let malloc_safe_mem =
  malloc_subroutine_instrs

(** val entry_point_load_lin_mem : Big_int_Z.big_int **)

let entry_point_load_lin_mem =
  Big_int_Z.succ_big_int Big_int_Z.zero_big_int

(** val load_lin_mem : cerise_instruction list **)

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

(** val store_lin_mem : machineParameters -> cerise_instruction list **)

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

(** val grow_lin_mem : machineParameters -> cerise_instruction list **)

let grow_lin_mem h =
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
  (Inl z_page_size))) :: ((Add ((R (Big_int_Z.succ_big_int
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
  z_page_size))) :: ((Lea ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
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
    machineParameters -> Big_int_Z.big_int **)

let entry_point_current_lin_mem h =
  add (entry_point_grow_lin_mem h) (length (grow_lin_mem h))

(** val current_lin_mem : machineParameters -> cerise_instruction list **)

let current_lin_mem h =
  let offset_seal_cap = Z.opp (Z.of_nat (entry_point_current_lin_mem h)) in
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
  z_page_size))) :: ((Mov ((R (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int))), (Inl Big_int_Z.zero_big_int))) :: ((Mov ((R
  (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
  Big_int_Z.zero_big_int)))), (Inl Big_int_Z.zero_big_int))) :: ((Jmp (R
  Big_int_Z.zero_big_int)) :: []))))))))))))

(** val entry_point_load_global : Big_int_Z.big_int **)

let entry_point_load_global =
  Big_int_Z.succ_big_int Big_int_Z.zero_big_int

(** val load_global : cerise_instruction list **)

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

(** val store_global : machineParameters -> cerise_instruction list **)

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

(** val get_type_global : machineParameters -> cerise_instruction list **)

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

(** val get_mut_global : machineParameters -> cerise_instruction list **)

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
  let lEN_GLOBAL_ENTRIES = Big_int_Z.succ_big_int Big_int_Z.zero_big_int in
  let rec len_imports_globals' = function
  | [] -> Big_int_Z.zero_big_int
  | import :: imports' ->
    add
      (match import.imp_desc with
       | ID_global _ -> lEN_GLOBAL_ENTRIES
       | _ -> Big_int_Z.zero_big_int) (len_imports_globals' imports')
  in len_imports_globals' module0.mod_imports

(** val len_defined_globals : ws_module -> Big_int_Z.big_int **)

let len_defined_globals module0 =
  let lEN_GLOBAL_ENTRIES = Big_int_Z.succ_big_int Big_int_Z.zero_big_int in
  mul lEN_GLOBAL_ENTRIES (length module0.mod_globals)

(** val len_defined_lin_mem : ws_module -> Big_int_Z.big_int **)

let len_defined_lin_mem module0 =
  let lEN_LIN_MEM_ENTRIES = Big_int_Z.succ_big_int Big_int_Z.zero_big_int in
  mul lEN_LIN_MEM_ENTRIES (length module0.mod_mems)

(** val len_imports_lin_mem : ws_module -> Big_int_Z.big_int **)

let len_imports_lin_mem module0 =
  let lEN_LIN_MEM_ENTRIES = Big_int_Z.succ_big_int Big_int_Z.zero_big_int in
  let rec len_imports_lin_mem' = function
  | [] -> Big_int_Z.zero_big_int
  | import :: imports' ->
    (match import.imp_desc with
     | ID_mem _ -> lEN_LIN_MEM_ENTRIES
     | _ -> len_imports_lin_mem' imports')
  in len_imports_lin_mem' module0.mod_imports

(** val len_defined_itable : ws_module -> Big_int_Z.big_int **)

let len_defined_itable module0 =
  length module0.mod_tables

(** val len_imports_itable : ws_module -> Big_int_Z.big_int **)

let len_imports_itable module0 =
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
    add idx_imports_lin_mem0 (len_imports_lin_mem module0)
  in
  let idx_imports_global =
    add idx_defined_lin_mem0 (len_defined_lin_mem module0)
  in
  let idx_defined_globals0 =
    add idx_imports_global (len_imports_globals module0)
  in
  let idx_imports_itable0 =
    add idx_defined_globals0 (len_defined_globals module0)
  in
  let idx_defined_itable0 =
    add idx_imports_itable0 (len_imports_itable module0)
  in
  let idx_safe_mem0 = add idx_defined_itable0 (len_defined_itable module0) in
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

type compilation_state = { regidx : Big_int_Z.big_int;
                           current_scope : scope_state }

(** val new_state : Big_int_Z.big_int -> scope_state -> compilation_state **)

let new_state reg0 st =
  { regidx = reg0; current_scope = st }

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
    machineParameters -> regName -> regName -> labeled_instr list -> regName
    list -> result_type -> result_type -> compilation_state -> (labeled_instr
    list * compilation_state) option **)

let call_template h r_fun r_res prologue args _ ret_type new_state0 =
  let call = instrs (call_instrs h r_fun args) in
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
              (app ((LoadU (r_res, r_stk0, (Inl (Big_int_Z.minus_big_int
                Big_int_Z.unit_big_int)))) :: ((Lea (r_stk0, (Inl
                (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: []))
                (dyn_typecheck_instrs h r_res ret_type0)))
        | _ :: _ -> None))

(** val local_offset : Big_int_Z.big_int **)

let local_offset =
  (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)

(** val prologue_return :
    function_type -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> cerise_instruction list option **)

let prologue_return f_type _ ret off tmp =
  let ret_type = let Tf (_, r0) = f_type in r0 in
  (match ret_type with
   | [] -> Some []
   | _ :: l ->
     (match l with
      | [] ->
        Some ((GetB ((R tmp), r_stk0)) :: ((GetA ((R off), r_stk0)) :: ((Sub
          ((R off), (r tmp), (r off))) :: ((Add ((R off), (r off), (Inl
          (Z.sub local_offset Big_int_Z.unit_big_int)))) :: ((LoadU ((R tmp),
          r_stk0, (r off))) :: ((StoreU ((R tmp), (Inl
          Big_int_Z.zero_big_int), (r ret))) :: []))))))
      | _ :: _ -> None))

(** val compile_basic_instr :
    machineParameters -> ws_module -> typeidx -> value_type list ->
    ws_basic_instruction -> compilation_state -> cfg option **)

let rec compile_basic_instr h module0 f_typeidx f_locals i s =
  let nreg = s.regidx in
  let module_frame = define_module_frame module0 in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun f_type ->
    match i with
    | I_unreachable -> Some ((instrs (Fail :: [])), s)
    | I_nop -> Some ((instrs (nop :: [])), s)
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
      Some (((Br_Jnz (lbl_v1, c)) :: ((BInstr (Mov ((R res), (Inr (R
      v2))))) :: ((Br_Jmp lbl_v2) :: ((Label lbl_v1) :: ((BInstr (Mov ((R
      res), (Inr (R v1))))) :: ((Label lbl_v2) :: [])))))),
      (sub_reg new_state0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
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
            ((app ((Br_Jnz (lbl_true,
               (sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) :: [])
               (app bodyF
                 (app ((Br_Jmp lbl_end) :: ((Label lbl_true) :: []))
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
         | [] -> Some (((Br_Jmp lbl) :: []), s)
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
              Some (code, s)
            | _ :: _ -> None))) (Obj.magic getn_scope s.current_scope n0)
    | I_br_if n0 ->
      let lbl = generate_label s.current_scope n0 in
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun scopen ->
        let rt = scopen.scope_ret_type in
        (match rt with
         | [] ->
           Some (((Br_Jnz (lbl,
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
              Some (code,
              (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
            | _ :: _ -> None))) (Obj.magic getn_scope s.current_scope n0)
    | I_return ->
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
      mbind (Obj.magic (fun _ _ -> option_bind))
        (fun prepare_return_instrs -> Some
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
    | I_call i0 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
        let Tf (arg_type, ret_type) = pat in
        let len_arg_type = length arg_type in
        let tmp_fun =
          add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int))))))
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
        let new_state0 = sub_reg (add_reg s (length ret_type)) len_arg_type in
        call_template h (R tmp_fun) (R res) prologue args arg_type ret_type
          new_state0) (Obj.magic get_function_type module0 i0)
    | I_call_indirect i0 ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
        let Tf (arg_type, ret_type) = pat in
        let len_arg_type = length arg_type in
        let nfun = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
        let res = sub nfun len_arg_type in
        let tmp_fun =
          add r_tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int))))))
        in
        let args' = seq (sub nfun len_arg_type) len_arg_type in
        let args = map (fun x -> R x) args' in
        let prologue =
          instrs ((Mov ((R tmp_fun), (Inr r_frame))) :: ((Lea ((R tmp_fun),
            (Inl (Z.of_nat module_frame.idx_imports_itable)))) :: ((Load ((R
            tmp_fun), (R tmp_fun))) :: ((Lea ((R tmp_fun),
            (r nfun))) :: ((Load ((R tmp_fun), (R tmp_fun))) :: [])))))
        in
        let new_state0 =
          sub_reg (add_reg s (length ret_type))
            (add len_arg_type (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
        in
        call_template h (R tmp_fun) (R res) prologue args arg_type ret_type
          new_state0) (Obj.magic get_type module0 i0)
    | I_get_local i0 ->
      let off = add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
      Some
      ((instrs ((GetB ((R r_tmp), r_stk0)) :: ((Add ((R r_tmp), (r r_tmp),
         (Inl (Z.of_nat i0)))) :: ((GetA ((R off), r_stk0)) :: ((Sub ((R
         off), (r r_tmp), (r off))) :: ((Add ((R off), (r off), (Inl
         local_offset))) :: ((LoadU ((R nreg), r_stk0, (r off))) :: []))))))),
      (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
    | I_set_local i0 ->
      let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
      let off = add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
      Some
      ((instrs ((GetB ((R r_tmp), r_stk0)) :: ((Add ((R r_tmp), (r r_tmp),
         (Inl (Z.of_nat i0)))) :: ((GetA ((R off), r_stk0)) :: ((Sub ((R
         off), (r r_tmp), (r off))) :: ((Add ((R off), (r off), (Inl
         local_offset))) :: ((StoreU (r_stk0, (r off), (r v))) :: []))))))),
      (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
    | I_tee_local i0 ->
      let v = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
      let off = add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
      Some
      ((instrs ((GetB ((R r_tmp), r_stk0)) :: ((Add ((R r_tmp), (r r_tmp),
         (Inl (Z.of_nat i0)))) :: ((GetA ((R off), r_stk0)) :: ((Sub ((R
         off), (r r_tmp), (r off))) :: ((Add ((R off), (r off), (Inl
         local_offset))) :: ((StoreU (r_stk0, (r off), (r v))) :: []))))))),
      s)
    | I_get_global i0 ->
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
      let offset_global = Z.of_nat (add module_frame.idx_defined_globals i0)
      in
      Some
      ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
         tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry),
         (R tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
         offset_load_global))) :: ((Load ((R tmp_sentry), (R
         tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
         Big_int_Z.zero_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int)), (Inr r_frame))) :: ((Lea ((R
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
         offset_global))) :: ((Load ((R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int)))) :: ((Mov ((R Big_int_Z.zero_big_int),
         (Inr PC))) :: ((Lea ((R Big_int_Z.zero_big_int), (Inl
         ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
         Big_int_Z.unit_big_int)))) :: ((Jmp (R tmp_sentry)) :: ((Mov ((R
         nreg), (Inr (R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int),
         (Inr (R tmp0)))) :: []))))))))))))))),
      (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
    | I_set_global i0 ->
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
      let offset_global = Z.of_nat (add module_frame.idx_defined_globals i0)
      in
      Some
      ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
         tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry),
         (R tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
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
    | I_load _ ->
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
      Some
      ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
         tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry),
         (R tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
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
    | I_store _ ->
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
      Some
      ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
         tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry),
         (R tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
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
    | I_segload _UU03c4_ ->
      let h0 = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
      let res = sub nreg (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
      Some
      ((instrs
         (app ((Load ((R res), (R h0))) :: [])
           (dyn_typecheck_instrs h (R res) _UU03c4_))), s)
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
      let tmp2 = add r_tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int) in
      let res =
        sub nreg (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
      in
      Some
      ((instrs ((GetB ((R r_tmp), (R h0))) :: ((Add ((R o1), (r o1),
         (r r_tmp))) :: ((GetE ((R tmp2), (R h0))) :: ((Sub ((R o2),
         (r tmp2), (r o2))) :: ((Subseg ((R h0), (r o1), (r o2))) :: ((Mov
         ((R res), (r h0))) :: []))))))),
      (sub_reg s (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
        Big_int_Z.zero_big_int))))
    | I_segalloc ->
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
      Some
      ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
         tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry),
         (R tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
         offset_malloc_safe_mem))) :: ((Load ((R tmp_sentry), (R
         tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
         Big_int_Z.zero_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr (R
         size)))) :: ((Mov ((R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int)), (Inr r_frame))) :: ((Lea ((R
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
         offset_safe_mem))) :: ((Load ((R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int)))) :: ((Mov ((R Big_int_Z.zero_big_int),
         (Inr PC))) :: ((Lea ((R Big_int_Z.zero_big_int), (Inl
         ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
         Big_int_Z.unit_big_int)))) :: ((Jmp (R tmp_sentry)) :: ((Mov ((R
         res), (Inr (R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int),
         (Inr (R tmp0)))) :: [])))))))))))))))), s)
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
      Some
      ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
         tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry),
         (R tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
         offset_current_lin_mem))) :: ((Load ((R tmp_sentry), (R
         tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
         Big_int_Z.zero_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int)), (Inr r_frame))) :: ((Lea ((R
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
         offset_lin_mem))) :: ((Load ((R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int)))) :: ((Mov ((R Big_int_Z.zero_big_int),
         (Inr PC))) :: ((Lea ((R Big_int_Z.zero_big_int), (Inl
         ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
         Big_int_Z.unit_big_int)))) :: ((Jmp (R tmp_sentry)) :: ((Mov ((R
         nreg), (Inr (R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int),
         (Inr (R tmp0)))) :: []))))))))))))))),
      (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
    | I_grow_memory ->
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
      Some
      ((instrs ((Mov ((R tmp_sentry), (Inr r_frame))) :: ((Lea ((R
         tmp_sentry), (Inl offset_linking_tbl))) :: ((Load ((R tmp_sentry),
         (R tmp_sentry))) :: ((Lea ((R tmp_sentry), (Inl
         offset_grow_lin_mem))) :: ((Load ((R tmp_sentry), (R
         tmp_sentry))) :: ((Mov ((R tmp0), (Inr (R
         Big_int_Z.zero_big_int)))) :: ((Mov ((R (Big_int_Z.succ_big_int
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (Inr (R
         size)))) :: ((Mov ((R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int)), (Inr r_frame))) :: ((Lea ((R
         (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (Inl
         offset_lin_mem))) :: ((Load ((R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int)), (R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int)))) :: ((Mov ((R Big_int_Z.zero_big_int),
         (Inr PC))) :: ((Lea ((R Big_int_Z.zero_big_int), (Inl
         ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
         Big_int_Z.unit_big_int)))) :: ((Jmp (R tmp_sentry)) :: ((Mov ((R
         res), (Inr (R (Big_int_Z.succ_big_int
         Big_int_Z.zero_big_int))))) :: ((Mov ((R Big_int_Z.zero_big_int),
         (Inr (R tmp0)))) :: [])))))))))))))))), s)
    | I_const v ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun val0 -> Some
        ((instrs ((Mov ((R nreg), (Inl val0))) :: [])),
        (add_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))
        (match v with
         | Val_int z0 -> Some (Obj.magic z0)
         | Val_handle _ -> None)
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
      Some ((instrs (eqz_instrs (R res) (R v) (R r_tmp))), s)
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
          | ROI_eq -> eq_instrs (R res) (R v1) (R v2) (R r_tmp)
          | ROI_ne -> neq_instrs (R res) (R v1) (R v2) (R r_tmp)
          | ROI_lt _ -> (Lt0 ((R res), (Inr (R v1)), (Inr (R v2)))) :: []
          | ROI_gt _ -> (Lt0 ((R res), (Inr (R v2)), (Inr (R v1)))) :: []
          | ROI_le _ -> ge_instrs (R res) (R v2) (R v1) (R r_tmp)
          | ROI_ge _ -> ge_instrs (R res) (R v1) (R v2) (R r_tmp))),
      (sub_reg s (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))
    (Obj.magic get_type module0 f_typeidx)

(** val labeled_compile_expr' :
    machineParameters -> ws_module -> typeidx -> value_type list ->
    ws_basic_instruction list -> compilation_state -> cfg option **)

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
    ws_basic_instruction list -> compilation_state -> cfg option **)

let labeled_compile_expr =
  labeled_compile_expr'

(** val load_args :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> result_type ->
    cerise_instruction list **)

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
  app
    (instrs
      (app ((Mov ((R (tmp Big_int_Z.zero_big_int)), (Inr PC))) :: ((GetB ((R
        (tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))), (R
        (tmp Big_int_Z.zero_big_int)))) :: ((GetA ((R
        (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))), (R
        (tmp Big_int_Z.zero_big_int)))) :: ((Sub ((R
        (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
          Big_int_Z.zero_big_int)))),
        (r (tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))),
        (r
          (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int)))))) :: ((Lea ((R
        (tmp Big_int_Z.zero_big_int)),
        (r
          (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
            Big_int_Z.zero_big_int)))))) :: ((Load ((R
        Big_int_Z.zero_big_int), (R
        (tmp Big_int_Z.zero_big_int)))) :: []))))))
        (app
          (match rt with
           | [] -> []
           | _ :: _ ->
             (LoadU ((R (tmp Big_int_Z.zero_big_int)), r_stk0, (Inl
               (Big_int_Z.minus_big_int Big_int_Z.unit_big_int)))) :: ((Lea
               (r_stk0, (Inl (Big_int_Z.minus_big_int
               Big_int_Z.unit_big_int)))) :: []))
          (load_args mP (tmp Big_int_Z.zero_big_int)
            (tmp (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
            (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
              Big_int_Z.zero_big_int)))
            (tmp (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
              (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))) rt))))
    (instrs (prepare_stack_pointer (add size_locals max_spilled_reg)))

(** val allocate_data : Big_int_Z.big_int -> Big_int_Z.big_int -> data **)

let allocate_data size z0 =
  repeat (WInt z0) size

(** val get_start : ws_module -> name -> (id0 * Big_int_Z.big_int) option **)

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
  let mf_full_body = app mf_body (I_return :: []) in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun pat ->
    let (body, _) = pat in
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun f_type ->
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
      Some (app prologue body)) (Obj.magic get_type m mf_type))
    (Obj.magic labeled_compile_expr mP m mf_type mf_locals mf_full_body
      init_state0)

(** val compile_funcs :
    machineParameters -> ws_module -> labeled_function list option **)

let compile_funcs mP m =
  foldl (fun acc_opt f ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun acc ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_f -> Some
        (compiled_f :: acc)) (Obj.magic compile_func mP f m)) acc_opt) (Some
    []) m.mod_funcs

(** val compile_lin_mem : memory_type -> Big_int_Z.big_int -> data **)

let compile_lin_mem m max_nb_page =
  let size_min = mul m.lim_min page_size0 in
  let size_max =
    mul
      (match m.lim_max with
       | Some n0 -> min n0 max_nb_page
       | None -> max_nb_page) page_size0
  in
  let m_b = Big_int_Z.zero_big_int in
  let l_m =
    add m_b (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
      Big_int_Z.zero_big_int))
  in
  let l_a = add l_m size_min in
  let m_e = add l_m size_max in
  let full_cap = WSealable (SCap ((RW, Global), l_m, m_e, l_a)) in
  let current_cap = WSealable (SCap ((RW, Global), l_m, l_a, l_m)) in
  let linear_memory = allocate_data size_max Big_int_Z.zero_big_int in
  full_cap :: (current_cap :: linear_memory)

(** val compile_lin_mems :
    memory_type list -> Big_int_Z.big_int -> data list **)

let compile_lin_mems llm max_nb_page =
  map (fun m -> compile_lin_mem m max_nb_page) llm

(** val compile_global : module_glob -> data **)

let compile_global g =
  let gtype = g.modglob_type in
  let init_value =
    match gtype.tg_t with
    | T_int ->
      (match g.modglob_init with
       | Val_int z0 -> WInt z0
       | Val_handle _ -> WInt Big_int_Z.zero_big_int)
    | T_handle ->
      WSealable (SCap ((RW, Global), Big_int_Z.zero_big_int,
        Big_int_Z.zero_big_int, Big_int_Z.zero_big_int))
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
  init_value :: (type_val :: (mut_val :: []))

(** val compile_globals : module_glob list -> data list **)

let compile_globals lg =
  map compile_global lg

(** val compile_indirect_table : module_table -> Big_int_Z.big_int -> data **)

let compile_indirect_table it max_nb_entry =
  let nb_entry =
    match (tt_limits (modtab_type it)).lim_max with
    | Some n0 -> min n0 max_nb_entry
    | None -> max_nb_entry
  in
  allocate_data nb_entry Big_int_Z.zero_big_int

(** val compile_indirect_tables :
    module_table list -> Big_int_Z.big_int -> data list **)

let compile_indirect_tables lit max_nb_entry =
  map (fun it -> compile_indirect_table it max_nb_entry) lit

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
  let acc = imports_functions imps' in
  (match import_function imp with
   | Some s -> s :: acc
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
   | Some s -> app (s :: []) acc
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

(** val compile_frame : ws_module -> frame -> name -> cerise_frame option **)

let compile_frame m frm module_name =
  let imported_functions =
    map (fun s -> Imports s) (imports_functions m.mod_imports)
  in
  let defined_functions =
    map (fun i -> Define (module_name, i))
      (seq frm.idx_defined_functions (len_defined_functions m))
  in
  let imported_lin_mem =
    map (fun s -> Imports s) (imports_lin_mems m.mod_imports)
  in
  let defined_lin_mem =
    map (fun i -> Define (module_name, i))
      (seq frm.idx_defined_lin_mem (len_defined_lin_mem m))
  in
  let imported_globals =
    map (fun s -> Imports s) (imports_globals m.mod_imports)
  in
  let defined_globals =
    map (fun i -> Define (module_name, i))
      (seq frm.idx_defined_globals (len_defined_globals m))
  in
  let imported_itable =
    map (fun s -> Imports s) (imports_itables m.mod_imports)
  in
  let defined_itable =
    map (fun i -> Define (module_name, i))
      (seq frm.idx_defined_itable (len_defined_itable m))
  in
  let safe_memory = (Common SafeMem) :: [] in
  let link_tbl = (Common LinkTbl) :: [] in
  Some { cframe_functions_imported = imported_functions;
  cframe_functions_defined = defined_functions; cframe_lin_mem_imported =
  imported_lin_mem; cframe_lin_mem_defined = defined_lin_mem;
  cframe_globals_imported = imported_globals; cframe_globals_defined =
  defined_globals; cframe_itable_imported = imported_itable;
  cframe_itable_defined = defined_itable; cframe_safe_mem = safe_memory;
  cframe_link_tbl = link_tbl }

(** val compile_element :
    funcidx list -> Big_int_Z.big_int -> frame -> name -> (Big_int_Z.big_int,
    frame_entry) gmap **)

let rec compile_element fidxs offset0 frm module_name =
  match fidxs with
  | [] -> gmap_empty Coq0_Nat.eq_dec nat_countable
  | f :: fidxs' ->
    insert0 (map_insert (gmap_partial_alter Coq0_Nat.eq_dec nat_countable))
      offset0 (Define (module_name, f))
      (compile_element fidxs'
        (add offset0 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) frm
        module_name)

(** val compile_elements :
    module_element list -> frame -> name -> (Big_int_Z.big_int, frame_entry)
    gmap **)

let rec compile_elements ele frm module_name =
  match ele with
  | [] -> gmap_empty Coq0_Nat.eq_dec nat_countable
  | e :: ele' ->
    union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq0_Nat.eq_dec nat_countable)))
      (compile_element e.modelem_init e.modelem_offset frm module_name)
      (compile_elements ele' frm module_name)

(** val compile_export :
    module_export -> frame -> id0 -> (symbols, Big_int_Z.big_int) gmap **)

let compile_export exp frm module_name =
  let s = symbols_encode module_name exp.modexp_name in
  (match exp.modexp_desc with
   | MED_func f ->
     singletonM0
       (map_singleton (gmap_partial_alter string_eq_dec string_countable)
         (gmap_empty string_eq_dec string_countable)) s f
   | MED_table t0 ->
     singletonM0
       (map_singleton (gmap_partial_alter string_eq_dec string_countable)
         (gmap_empty string_eq_dec string_countable)) s
       (add frm.idx_imports_itable t0)
   | MED_mem m ->
     singletonM0
       (map_singleton (gmap_partial_alter string_eq_dec string_countable)
         (gmap_empty string_eq_dec string_countable)) s
       (add frm.idx_imports_lin_mem m)
   | MED_global g ->
     singletonM0
       (map_singleton (gmap_partial_alter string_eq_dec string_countable)
         (gmap_empty string_eq_dec string_countable)) s
       (add frm.idx_imports_globals g))

(** val compile_exports :
    module_export list -> frame -> name -> (symbols, Big_int_Z.big_int) gmap **)

let rec compile_exports exps frm module_name =
  match exps with
  | [] -> gmap_empty string_eq_dec string_countable
  | exp :: exps' ->
    union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge string_eq_dec string_countable)))
      (compile_export exp frm module_name)
      (compile_exports exps' frm module_name)

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
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_frame ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_functions ->
      let lin_mem = compile_lin_mems m.mod_mems mAX_LIN_MEM in
      let indirect_table =
        compile_indirect_tables m.mod_tables mAX_INDIRECT_TABLE
      in
      let globals = compile_globals m.mod_globals in
      let initial_elements = compile_elements m.mod_elem frm module_name in
      let exports = compile_exports m.mod_exports frm module_name in
      let subcomp = { l_id = module_name; l_functions = compiled_functions;
        l_frame = compiled_frame; l_lin_mem = lin_mem; l_globals = globals;
        l_indirect_table = indirect_table; l_elements = initial_elements;
        l_exports = exports }
      in
      Some { l_subcomp = (subcomp :: []); l_main = (get_start m module_name) })
      (Obj.magic compile_funcs mP m))
    (Obj.magic compile_frame m frm module_name)

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
     | Br_Jmp lbl ->
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun o ->
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun next ->
           let off = Z.sub (Z.of_nat o) (Z.of_nat addr0) in
           Some
           (app ((Mov ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
             (Inr PC))) :: ((Lea ((R (Big_int_Z.succ_big_int
             Big_int_Z.zero_big_int)), (Inl off))) :: ((Jmp (R
             (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))) :: []))) next))
           (branch_labels' il' label_map
             (add addr0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
               (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))
         (lookup0
           (Obj.magic gmap_lookup (list_eq_dec0 Coq0_Nat.eq_dec)
             (list_countable Coq0_Nat.eq_dec nat_countable)) lbl label_map)
     | Br_Jnz (lbl, reg_cond) ->
       mbind (Obj.magic (fun _ _ -> option_bind)) (fun o ->
         mbind (Obj.magic (fun _ _ -> option_bind)) (fun next ->
           let off = Z.sub (Z.of_nat o) (Z.of_nat addr0) in
           Some
           (app ((Mov ((R (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
             (Inr PC))) :: ((Lea ((R (Big_int_Z.succ_big_int
             Big_int_Z.zero_big_int)), (Inl off))) :: ((Jnz ((R
             (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)), (R
             reg_cond))) :: []))) next))
           (branch_labels' il' label_map
             (add addr0 (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int
               (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))))))
         (lookup0
           (Obj.magic gmap_lookup (list_eq_dec0 Coq0_Nat.eq_dec)
             (list_countable Coq0_Nat.eq_dec nat_countable)) lbl label_map))

(** val branch_labels :
    labeled_instr list -> cerise_instruction list option **)

let branch_labels il =
  branch_labels' il (addresses_labels il) Big_int_Z.zero_big_int

(** val compile_functions :
    labeled_function list -> cerise_function list option **)

let compile_functions progs =
  foldl (fun acc_opt p ->
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun acc ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_prog -> Some
        (compiled_prog :: acc)) (Obj.magic branch_labels p)) acc_opt) (Some
    []) progs

(** val compile_subcomponent :
    labeled_cerise_subcomponent -> cerise_subcomponent option **)

let compile_subcomponent m =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_functions -> Some
    { c_id = m.l_id; c_functions = compiled_functions; c_frame = m.l_frame;
    c_lin_mem = m.l_lin_mem; c_globals = m.l_globals; c_indirect_table =
    m.l_indirect_table; c_elements = m.l_elements; c_exports = m.l_exports })
    (Obj.magic compile_functions m.l_functions)

(** val compile_component :
    labeled_cerise_component -> cerise_component option **)

let compile_component m =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun subcomp -> Some
    { c_subcomp = subcomp; c_main = m.l_main })
    (foldl (fun acc_opt s ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun acc ->
        mbind (Obj.magic (fun _ _ -> option_bind)) (fun compiled_subcomp ->
          Some (Obj.magic (compiled_subcomp :: acc)))
          (Obj.magic compile_subcomponent s)) acc_opt) (Some (Obj.magic []))
      m.l_subcomp)

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
    Big_int_Z.big_int -> Big_int_Z.big_int -> cerise_instruction
    list * Big_int_Z.big_int **)

let load_spilled reg_num tmp =
  match get_spilling_offset reg_num with
  | Some offset0 -> (((LoadU ((R tmp), r_stk0, (Inl offset0))) :: []), tmp)
  | None -> ([], reg_num)

(** val load_spilled_reg :
    regName -> Big_int_Z.big_int -> cerise_instruction list * regName **)

let load_spilled_reg reg0 tmp =
  match reg0 with
  | R n0 -> let (li, n') = load_spilled n0 tmp in (li, (R n'))
  | _ -> ([], reg0)

(** val load_spilled_zreg :
    (Big_int_Z.big_int, regName) sum -> Big_int_Z.big_int ->
    cerise_instruction list * (Big_int_Z.big_int, regName) sum **)

let load_spilled_zreg zreg tmp =
  match zreg with
  | Inl z0 -> ([], (Inl z0))
  | Inr r0 -> let (li, r') = load_spilled_reg r0 tmp in (li, (Inr r'))

(** val store_spilled_reg :
    regName -> regName -> cerise_instruction list * regName **)

let store_spilled_reg dst tmp =
  match dst with
  | R n0 ->
    (match get_spilling_offset n0 with
     | Some offset0 ->
       (((StoreU (r_stk0, (Inl offset0), (Inr tmp))) :: []), tmp)
     | None -> ([], (R n0)))
  | _ -> ([], dst)

(** val spill_instruction : cerise_instruction -> cerise_instruction list **)

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
| Br_Jnz (lbl, reg0) ->
  let (il, r0) = load_spilled reg0 r_tmp in
  app (instrs il) ((Br_Jnz (lbl, r0)) :: [])
| x -> x :: []

(** val spilling : labeled_instr list -> labeled_instr list **)

let rec spilling = function
| [] -> []
| i :: il' -> app (spill_labeled_instr i) (spilling il')

(** val reg_alloc_subcomponent :
    labeled_cerise_subcomponent -> labeled_cerise_subcomponent **)

let reg_alloc_subcomponent m =
  { l_id = m.l_id; l_functions = (map spilling m.l_functions); l_frame =
    m.l_frame; l_lin_mem = m.l_lin_mem; l_globals = m.l_globals;
    l_indirect_table = m.l_indirect_table; l_elements = m.l_elements;
    l_exports = m.l_exports }

(** val register_allocation :
    labeled_cerise_component -> labeled_cerise_component **)

let register_allocation c =
  { l_subcomp = (map reg_alloc_subcomponent c.l_subcomp); l_main = c.l_main }

(** val resolve_import :
    frame_entry -> (symbols, id0 * Big_int_Z.big_int) gmap -> frame_entry **)

let resolve_import e exports =
  match e with
  | Imports s ->
    (match lookup0 (gmap_lookup string_eq_dec string_countable) s exports with
     | Some y -> let (m, idx) = y in Define (m, idx)
     | None -> e)
  | _ -> e

(** val resolve_frame_link :
    cerise_frame -> (symbols, id0 * Big_int_Z.big_int) gmap -> cerise_frame **)

let resolve_frame_link frm exports =
  let resolve = fun sf -> map (fun e -> resolve_import e exports) sf in
  { cframe_functions_imported = (resolve frm.cframe_functions_imported);
  cframe_functions_defined = (resolve frm.cframe_functions_defined);
  cframe_lin_mem_imported = (resolve frm.cframe_lin_mem_imported);
  cframe_lin_mem_defined = (resolve frm.cframe_lin_mem_defined);
  cframe_globals_imported = (resolve frm.cframe_globals_imported);
  cframe_globals_defined = (resolve frm.cframe_globals_defined);
  cframe_itable_imported = (resolve frm.cframe_itable_imported);
  cframe_itable_defined = (resolve frm.cframe_itable_defined);
  cframe_safe_mem = (resolve frm.cframe_safe_mem); cframe_link_tbl =
  (resolve frm.cframe_link_tbl) }

(** val resolve_frame_link_map :
    cerise_frame -> cerise_subcomponent list -> cerise_frame **)

let rec resolve_frame_link_map frm = function
| [] -> frm
| l :: l' ->
  let lib_exports =
    fmap (Obj.magic (fun _ _ -> gmap_fmap string_eq_dec string_countable))
      (fun n0 -> (l.c_id, n0)) l.c_exports
  in
  let frm' = resolve_frame_link_map frm l' in
  resolve_frame_link frm' (Obj.magic lib_exports)

(** val link_subcomps :
    cerise_subcomponent list -> cerise_subcomponent list ->
    cerise_subcomponent list **)

let link_subcomps p_lib p_client =
  app
    (map (fun c -> { c_id = c.c_id; c_functions = c.c_functions; c_frame =
      (resolve_frame_link_map c.c_frame p_lib); c_lin_mem = c.c_lin_mem;
      c_globals = c.c_globals; c_indirect_table = c.c_indirect_table;
      c_elements = c.c_elements; c_exports = c.c_exports }) p_client) p_lib

(** val resolve_main :
    cerise_component -> cerise_component -> (id0 * Big_int_Z.big_int) option
    option **)

let resolve_main p_lib p_client =
  match p_lib.c_main with
  | Some m ->
    (match p_client.c_main with
     | Some _ -> None
     | None -> Some (Some m))
  | None -> Some p_client.c_main

(** val link_seq :
    cerise_component -> cerise_component -> cerise_component option **)

let link_seq p_lib p_client =
  let subcomps = link_subcomps p_lib.c_subcomp p_client.c_subcomp in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun main_word -> Some
    { c_subcomp = subcomps; c_main = main_word })
    (Obj.magic resolve_main p_lib p_client)

(** val instantiation : cerise_component list -> cerise_component option **)

let rec instantiation = function
| [] -> None
| p_client :: p_lib' ->
  (match p_lib' with
   | [] -> Some p_client
   | _ :: _ ->
     mbind (Obj.magic (fun _ _ -> option_bind)) (fun p_lib ->
       link_seq p_lib p_client) (instantiation p_lib'))

(** val len_frame_cerise_subcomponent :
    cerise_subcomponent -> Big_int_Z.big_int **)

let len_frame_cerise_subcomponent s =
  add
    (add
      (add
        (add
          (add
            (add
              (add
                (add
                  (add (length s.c_frame.cframe_functions_imported)
                    (length s.c_frame.cframe_functions_defined))
                  (length s.c_frame.cframe_lin_mem_imported))
                (length s.c_frame.cframe_lin_mem_defined))
              (length s.c_frame.cframe_globals_imported))
            (length s.c_frame.cframe_globals_defined))
          (length s.c_frame.cframe_itable_imported))
        (length s.c_frame.cframe_itable_defined))
      (length s.c_frame.cframe_safe_mem)) (length s.c_frame.cframe_link_tbl)

(** val len_code_cerise_subcomponent :
    cerise_subcomponent -> Big_int_Z.big_int **)

let len_code_cerise_subcomponent s =
  foldl (fun acc f -> add acc (length f)) (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int) s.c_functions

(** val len_code : cerise_subcomponent list -> Big_int_Z.big_int **)

let len_code cerise_subcomponent_list =
  foldl (fun acc s -> add acc (len_code_cerise_subcomponent s))
    Big_int_Z.zero_big_int cerise_subcomponent_list

(** val len_frames : cerise_subcomponent list -> Big_int_Z.big_int **)

let len_frames cerise_subcomponent_list =
  foldl (fun acc s -> add acc (len_frame_cerise_subcomponent s))
    Big_int_Z.zero_big_int cerise_subcomponent_list

(** val len_linear_memories :
    cerise_subcomponent list -> Big_int_Z.big_int **)

let len_linear_memories cerise_subcomponent_list =
  foldl (fun acc s -> add acc (length (concat s.c_lin_mem)))
    Big_int_Z.zero_big_int cerise_subcomponent_list

(** val len_globals : cerise_subcomponent list -> Big_int_Z.big_int **)

let len_globals cerise_subcomponent_list =
  foldl (fun acc s -> add acc (length (concat s.c_globals)))
    Big_int_Z.zero_big_int cerise_subcomponent_list

(** val len_indirect_table : cerise_subcomponent list -> Big_int_Z.big_int **)

let len_indirect_table cerise_subcomponent_list =
  foldl (fun acc s -> add acc (length (concat s.c_indirect_table)))
    Big_int_Z.zero_big_int cerise_subcomponent_list

(** val functions_entry_points :
    cerise_function list -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> word list **)

let rec functions_entry_points functions b e entry_point =
  match functions with
  | [] -> []
  | f :: functions' ->
    let next_entry_point = add entry_point (length f) in
    (WSealable (SCap ((E, Global), b, e,
    entry_point))) :: (functions_entry_points functions' b e next_entry_point)

(** val global_entry_point : data -> Big_int_Z.big_int -> oType -> word **)

let global_entry_point global offset0 ot_g =
  let e = add offset0 (length global) in
  WSealed (ot_g, (SCap ((RW, Global), offset0, e, offset0)))

(** val globals_entry_points :
    data list -> Big_int_Z.big_int -> oType -> word list **)

let rec globals_entry_points globals offset_globals ot_g =
  match globals with
  | [] -> []
  | g :: globals' ->
    let sealed_entry = global_entry_point g offset_globals ot_g in
    let new_offset = add offset_globals (length g) in
    sealed_entry :: (globals_entry_points globals' new_offset ot_g)

(** val lin_mem_entry_point : data -> Big_int_Z.big_int -> oType -> word **)

let lin_mem_entry_point lin_mem offset0 ot_lm =
  let e = add offset0 (length lin_mem) in
  WSealed (ot_lm, (SCap ((RW, Global), offset0, e, offset0)))

(** val lin_mems_entry_points :
    data list -> Big_int_Z.big_int -> oType -> word list **)

let rec lin_mems_entry_points lin_mems offset_lin_mems ot_lm =
  match lin_mems with
  | [] -> []
  | m :: lin_mems' ->
    let sealed_entry = lin_mem_entry_point m offset_lin_mems ot_lm in
    let new_offset = add offset_lin_mems (length m) in
    sealed_entry :: (lin_mems_entry_points lin_mems' new_offset ot_lm)

(** val itable_entry_point : data -> Big_int_Z.big_int -> word **)

let itable_entry_point itable offset0 =
  let e = add offset0 (length itable) in
  WSealable (SCap ((RO, Global), offset0, e, offset0))

(** val itables_entry_points : data list -> Big_int_Z.big_int -> word list **)

let rec itables_entry_points itables offset0 =
  match itables with
  | [] -> []
  | it :: itables' ->
    let entry = itable_entry_point it offset0 in
    let new_offset = add offset0 (length it) in
    entry :: (itables_entry_points itables' new_offset)

type aword =
| Abstract of id0 * Big_int_Z.big_int
| Concrete of word

type preMem = (addr, aword) gmap

(** val frame_entry_to_word :
    frame_entry -> id0 -> mem -> Big_int_Z.big_int -> word -> word -> aword **)

let frame_entry_to_word entry comp_id frame_inst offset_frame cap_safe_mem cap_linking_table =
  match entry with
  | Define (module0, idx) ->
    if eqb0 comp_id module0
    then Concrete
           (match lookup0 (gmap_lookup Coq0_Nat.eq_dec nat_countable)
                    (add offset_frame idx) frame_inst with
            | Some w -> w
            | None ->
              WInt
                ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
                ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
                (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))
    else Abstract (module0, idx)
  | Imports _ ->
    Concrete (WInt (Big_int_Z.mult_int_big_int 2
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))
  | Common c ->
    (match c with
     | SafeMem -> Concrete cap_safe_mem
     | LinkTbl -> Concrete cap_linking_table)

(** val cerise_frame_to_data :
    cerise_frame -> id0 -> mem -> Big_int_Z.big_int -> word -> word -> aword
    list **)

let cerise_frame_to_data frm comp_id frame_inst offset_frame cap_safe_mem cap_linking_table =
  let subframe_to_data = fun sf ->
    map (fun e ->
      frame_entry_to_word e comp_id frame_inst offset_frame cap_safe_mem
        cap_linking_table) sf
  in
  app (subframe_to_data frm.cframe_functions_imported)
    (app (subframe_to_data frm.cframe_functions_defined)
      (app (subframe_to_data frm.cframe_lin_mem_imported)
        (app (subframe_to_data frm.cframe_lin_mem_defined)
          (app (subframe_to_data frm.cframe_globals_imported)
            (app (subframe_to_data frm.cframe_globals_defined)
              (app (subframe_to_data frm.cframe_itable_imported)
                (app (subframe_to_data frm.cframe_itable_defined)
                  (app (subframe_to_data frm.cframe_safe_mem)
                    (subframe_to_data frm.cframe_link_tbl)))))))))

(** val instantiate_data : data list -> Big_int_Z.big_int -> preMem **)

let rec instantiate_data datas offset0 =
  match datas with
  | [] -> gmap_empty Coq0_Nat.eq_dec nat_countable
  | d :: datas' ->
    union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq0_Nat.eq_dec nat_countable)))
      (list_to_mem (map (fun x -> Concrete x) (shift_data d offset0)) offset0)
      (instantiate_data datas' (add offset0 (length d)))

(** val preload_cerise_subcomponent_in_memory :
    machineParameters -> cerise_subcomponent -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> word -> word -> oType -> oType -> preMem **)

let preload_cerise_subcomponent_in_memory mP s offset_code offset_frame offset_lin_mem offset_globals offset_indirect_table cap_safe_mem cap_linking_table ot_g ot_lm =
  let frame_capability = SCap ((RO, Global), offset_frame, offset_lin_mem,
    offset_frame)
  in
  let code = (WSealable
    frame_capability) :: (encodeInstrsW mP (concat s.c_functions))
  in
  let code_section = list_to_mem (map (fun x -> Concrete x) code) offset_code
  in
  let local_ftable =
    let e_code = add offset_code (length code) in
    let a_code_init =
      add offset_code (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
    in
    functions_entry_points s.c_functions offset_code e_code a_code_init
  in
  let len_imported_functions = length s.c_frame.cframe_functions_imported in
  let offset_local_ftable = add offset_frame len_imported_functions in
  let local_ftable_section = list_to_mem local_ftable offset_local_ftable in
  let lin_mem_table = lin_mems_entry_points s.c_lin_mem offset_lin_mem ot_lm
  in
  let offset_lin_mem_table =
    let len_imported_lin_mem = length s.c_frame.cframe_lin_mem_imported in
    add (add offset_local_ftable (length local_ftable)) len_imported_lin_mem
  in
  let lin_mem_table_section = list_to_mem lin_mem_table offset_lin_mem_table
  in
  let globals_table = globals_entry_points s.c_globals offset_globals ot_g in
  let offset_globals_table =
    let len_globals_imports = length s.c_frame.cframe_globals_imported in
    add (add offset_lin_mem_table (length lin_mem_table)) len_globals_imports
  in
  let globals_table_section = list_to_mem globals_table offset_globals_table
  in
  let indirection_table_cap =
    itables_entry_points s.c_indirect_table offset_indirect_table
  in
  let offset_indirection_table_cap =
    let len_imported_itables = length s.c_frame.cframe_itable_imported in
    add (add offset_globals_table (length globals_table)) len_imported_itables
  in
  let indirection_table_cap_section =
    list_to_mem indirection_table_cap offset_indirection_table_cap
  in
  let frame_inst =
    union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq0_Nat.eq_dec nat_countable)))
      (union0
        (map_union
          (Obj.magic (fun _ _ _ -> gmap_merge Coq0_Nat.eq_dec nat_countable)))
        (union0
          (map_union
            (Obj.magic (fun _ _ _ ->
              gmap_merge Coq0_Nat.eq_dec nat_countable)))
          local_ftable_section lin_mem_table_section) globals_table_section)
      indirection_table_cap_section
  in
  let frame_data =
    cerise_frame_to_data s.c_frame s.c_id frame_inst offset_frame
      cap_safe_mem cap_linking_table
  in
  let frame0 = list_to_mem frame_data offset_frame in
  let lin_mem = instantiate_data s.c_lin_mem offset_lin_mem in
  let globals = instantiate_data s.c_globals offset_globals in
  let indirect_table =
    instantiate_data s.c_indirect_table offset_indirect_table
  in
  let itable =
    foldl (fun acc ele ->
      let (fidx, entry) = ele in
      (match entry with
       | Define (m, idx) ->
         insert0
           (map_insert (gmap_partial_alter Coq0_Nat.eq_dec nat_countable))
           (add offset_indirect_table fidx) (Abstract (m, idx)) acc
       | _ ->
         insert0
           (map_insert (gmap_partial_alter Coq0_Nat.eq_dec nat_countable))
           (add offset_indirect_table fidx) (Concrete (WInt
           ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
           Big_int_Z.unit_big_int))) acc)) indirect_table
      (map_to_list (gmap_to_list Coq0_Nat.eq_dec nat_countable) s.c_elements)
  in
  union0
    (map_union
      (Obj.magic (fun _ _ _ -> gmap_merge Coq0_Nat.eq_dec nat_countable)))
    (union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq0_Nat.eq_dec nat_countable)))
      (union0
        (map_union
          (Obj.magic (fun _ _ _ -> gmap_merge Coq0_Nat.eq_dec nat_countable)))
        (union0
          (map_union
            (Obj.magic (fun _ _ _ ->
              gmap_merge Coq0_Nat.eq_dec nat_countable))) code_section frame0)
        lin_mem) globals) itable

(** val preload_cerise_subcomponents_in_memory :
    machineParameters -> cerise_subcomponent list -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> word -> word -> oType -> oType -> preMem * (id0,
    Big_int_Z.big_int) gmap **)

let rec preload_cerise_subcomponents_in_memory mP cerise_subcomponent_list offset_code offset_frame offset_lin_mem offset_globals offset_indirect_table cap_safe_mem cap_linking_table ot_g ot_lm =
  match cerise_subcomponent_list with
  | [] ->
    ((gmap_empty Coq0_Nat.eq_dec nat_countable),
      (gmap_empty string_eq_dec string_countable))
  | s :: cerise_subcomponent_list' ->
    let offset_code' = add offset_code (len_code_cerise_subcomponent s) in
    let offset_frame' = add offset_frame (len_frame_cerise_subcomponent s) in
    let offset_lin_mem' = add offset_lin_mem (length s.c_lin_mem) in
    let offset_globals' = add offset_globals (length s.c_globals) in
    let offset_indirect_table' =
      add offset_indirect_table (length s.c_indirect_table)
    in
    let new_module =
      preload_cerise_subcomponent_in_memory mP s offset_code offset_frame
        offset_lin_mem offset_globals offset_indirect_table cap_safe_mem
        cap_linking_table ot_g ot_lm
    in
    let (instantiated, offset_frames_modules) =
      preload_cerise_subcomponents_in_memory mP cerise_subcomponent_list'
        offset_code' offset_frame' offset_lin_mem' offset_globals'
        offset_indirect_table' cap_safe_mem cap_linking_table ot_g ot_lm
    in
    ((union0
       (map_union
         (Obj.magic (fun _ _ _ -> gmap_merge Coq0_Nat.eq_dec nat_countable)))
       new_module instantiated),
    (insert0 (map_insert (gmap_partial_alter string_eq_dec string_countable))
      s.c_id offset_frame offset_frames_modules))

(** val solve_undefined :
    aword -> (id0, Big_int_Z.big_int) gmap -> preMem -> aword **)

let solve_undefined e offset_submodules_frames premem =
  match e with
  | Abstract (module0, idx) ->
    (match lookup0 (gmap_lookup string_eq_dec string_countable) module0
             offset_submodules_frames with
     | Some idx_frame ->
       (match lookup0 (gmap_lookup Coq0_Nat.eq_dec nat_countable)
                (add idx_frame idx) premem with
        | Some y ->
          (match y with
           | Abstract (_, _) ->
             Concrete (WInt
               ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
               ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
               Big_int_Z.unit_big_int)))
           | Concrete w -> Concrete w)
        | None ->
          Concrete (WInt (Big_int_Z.mult_int_big_int 2
            ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
            Big_int_Z.unit_big_int))))
     | None ->
       Concrete (WInt
         ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
         (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))
  | Concrete w -> Concrete w

(** val load_cerise_subcomponents_in_memory :
    machineParameters -> cerise_subcomponent list -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> word -> word -> oType -> oType -> mem * (id0,
    Big_int_Z.big_int) gmap **)

let load_cerise_subcomponents_in_memory mP cerise_subcomponent_list offset_code offset_frame offset_lin_mem offset_globals offset_indirect_table cap_safe_mem cap_linking_table ot_g ot_lm =
  let (premem, offset_cerise_subcomponents_frame) =
    preload_cerise_subcomponents_in_memory mP cerise_subcomponent_list
      offset_code offset_frame offset_lin_mem offset_globals
      offset_indirect_table cap_safe_mem cap_linking_table ot_g ot_lm
  in
  let solved_premem =
    map_fold (gmap_to_list Coq0_Nat.eq_dec nat_countable)
      (fun k w acc_premem ->
      insert0 (map_insert (gmap_partial_alter Coq0_Nat.eq_dec nat_countable))
        k (solve_undefined w offset_cerise_subcomponents_frame acc_premem)
        acc_premem) premem premem
  in
  let mem0 =
    fmap (Obj.magic (fun _ _ -> gmap_fmap Coq0_Nat.eq_dec nat_countable))
      (fun w ->
      match w with
      | Abstract (_, _) ->
        WInt (Big_int_Z.mult_int_big_int 2
          ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
          (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int)))
      | Concrete w0 -> w0) solved_premem
  in
  ((Obj.magic mem0), offset_cerise_subcomponents_frame)

(** val get_main_word :
    cerise_component -> (id0, Big_int_Z.big_int) gmap -> mem -> word option **)

let get_main_word comp offset_frames memory =
  match comp.c_main with
  | Some p ->
    let (module0, idx) = p in
    mbind (Obj.magic (fun _ _ -> option_bind)) (fun offset_module_frame ->
      mbind (Obj.magic (fun _ _ -> option_bind)) (fun main_word -> Some
        main_word)
        (lookup0 (gmap_lookup Coq0_Nat.eq_dec nat_countable)
          (add offset_module_frame idx) memory))
      (lookup0 (Obj.magic gmap_lookup string_eq_dec string_countable) module0
        offset_frames)
  | None -> None

(** val load_in_memory :
    machineParameters -> cerise_component -> Big_int_Z.big_int -> oType ->
    oType -> oType -> cerise_program option **)

let load_in_memory mP comp size_safe_mem ot_g ot_lm ot_sm =
  let allocate_mem = fun z0 n0 -> repeat (WInt z0) n0 in
  let subcomp = comp.c_subcomp in
  let trusted_macros =
    let lin_mem_macros =
      let sealing_lin_mem = WSealable (SSealRange (((true, true), Global),
        ot_lm, (add ot_lm (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
        ot_lm))
      in
      app (sealing_lin_mem :: [])
        (app (encodeInstrsW mP load_lin_mem)
          (app (encodeInstrsW mP (store_lin_mem mP))
            (app (encodeInstrsW mP (grow_lin_mem mP))
              (encodeInstrsW mP (current_lin_mem mP)))))
    in
    let global_macros =
      let sealing_global = WSealable (SSealRange (((true, true), Global),
        ot_g, (add ot_g (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
        ot_g))
      in
      app (sealing_global :: [])
        (app (encodeInstrsW mP load_global)
          (app (encodeInstrsW mP (store_global mP))
            (app (encodeInstrsW mP (get_type_global mP))
              (encodeInstrsW mP (get_mut_global mP)))))
    in
    let safe_mem_macros =
      let sealing_safe_mem = WSealable (SSealRange (((true, true), Global),
        ot_sm, (add ot_sm (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
        ot_sm))
      in
      app (sealing_safe_mem :: []) (encodeInstrsW mP malloc_safe_mem)
    in
    app lin_mem_macros (app global_macros safe_mem_macros)
  in
  let len_trusted_macros = length trusted_macros in
  let offset_code = Big_int_Z.zero_big_int in
  let offset_trusted_macros = add offset_code (len_code subcomp) in
  let offset_frames = add offset_trusted_macros len_trusted_macros in
  let offset_lin_mems = add offset_frames (len_frames subcomp) in
  let offset_globals = add offset_lin_mems (len_linear_memories subcomp) in
  let offset_indirect_tables = add offset_globals (len_globals subcomp) in
  let offset_safe_mem =
    add offset_indirect_tables (len_indirect_table subcomp)
  in
  let offset_linking_table =
    add (add offset_safe_mem (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
      size_safe_mem
  in
  let safe_mem =
    let free_space_malloc_cap = SCap ((RW, Global), offset_safe_mem,
      offset_linking_table,
      (add offset_safe_mem (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
    in
    app ((WSealable free_space_malloc_cap) :: [])
      (allocate_mem Big_int_Z.zero_big_int size_safe_mem)
  in
  let linking_table =
    let offset_lin_mem_load0 =
      add offset_trusted_macros entry_point_load_lin_mem
    in
    let offset_lin_mem_store0 =
      add offset_trusted_macros entry_point_store_lin_mem
    in
    let offset_lin_mem_grow0 =
      add offset_trusted_macros (entry_point_grow_lin_mem mP)
    in
    let offset_lin_mem_current0 =
      add offset_trusted_macros (entry_point_current_lin_mem mP)
    in
    let offset_global_macros =
      add offset_lin_mem_current0 (length (current_lin_mem mP))
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
      offset_trusted_macros, offset_global_macros, offset_entry))
    in
    let global_sentry = fun offset_entry -> WSealable (SCap ((E, Global),
      offset_global_macros, offset_safe_mem_macros, offset_entry))
    in
    let safe_mem_sentry = fun offset_entry -> WSealable (SCap ((E, Global),
      offset_safe_mem_macros, offset_safe_mem_end, offset_entry))
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
  let safe_mem_section = list_to_mem safe_mem offset_safe_mem in
  let trusted_macros_section =
    list_to_mem trusted_macros offset_trusted_macros
  in
  let linking_table_section = list_to_mem linking_table offset_linking_table
  in
  let cap_safe_mem = WSealed (ot_sm, (SCap ((RW, Global), offset_safe_mem,
    offset_linking_table, offset_safe_mem)))
  in
  let e_linking_table = add offset_linking_table (length linking_table) in
  let cap_linking_table = WSealable (SCap ((RO, Global),
    offset_linking_table, e_linking_table, offset_linking_table))
  in
  let (loaded_subcomp, offset_submodules_frame) =
    load_cerise_subcomponents_in_memory mP subcomp offset_code offset_frames
      offset_lin_mems offset_globals offset_indirect_tables cap_safe_mem
      cap_linking_table ot_g ot_lm
  in
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun main_word -> Some
    { segment =
    (union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq0_Nat.eq_dec nat_countable)))
      (union0
        (map_union
          (Obj.magic (fun _ _ _ -> gmap_merge Coq0_Nat.eq_dec nat_countable)))
        (union0
          (map_union
            (Obj.magic (fun _ _ _ ->
              gmap_merge Coq0_Nat.eq_dec nat_countable))) loaded_subcomp
          safe_mem_section) trusted_macros_section) linking_table_section);
    main = main_word })
    (Obj.magic get_main_word comp offset_submodules_frame loaded_subcomp)

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
    machineParameters -> cerise_program -> Big_int_Z.big_int ->
    Big_int_Z.big_int -> reg * mem **)

let init_state h prog start_stack end_stack =
  let len_boot = length (boot_code h) in
  let pre_heap =
    union0
      (map_union
        (Obj.magic (fun _ _ _ -> gmap_merge Coq0_Nat.eq_dec nat_countable)))
      (boot_code_section h) (shift_segment prog.segment len_boot)
  in
  let len_pre_heap =
    length (gmap_to_list Coq0_Nat.eq_dec nat_countable pre_heap)
  in
  let end_code = encodeInstrsW h (Halt :: []) in
  let end_code_section =
    list_to_mem end_code
      (add len_pre_heap (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
  in
  let pc_cap = WSealable (SCap ((RX, Global), Big_int_Z.zero_big_int,
    len_boot, Big_int_Z.zero_big_int))
  in
  let end_cap = WSealable (SCap ((RX, Global), len_pre_heap,
    (add len_pre_heap (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)),
    len_pre_heap))
  in
  let main_cap = shift_word prog.main len_boot in
  let stk_cap = WSealable (SCap ((URWLX, Directed), start_stack, end_stack,
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
  account_balance) :: ((I_segload
  T_int) :: (I_drop :: [])))))))))))))))))))))))))

(** val bank_tf : function_type **)

let bank_tf =
  Tf ([], [])

(** val adv_tf : function_type **)

let adv_tf =
  Tf ((T_handle :: []), (T_int :: []))

(** val bank_module : ws_module **)

let bank_module =
  { mod_types = (bank_tf :: (adv_tf :: [])); mod_funcs = ({ modfunc_type =
    Big_int_Z.zero_big_int; modfunc_locals =
    (T_handle :: (T_handle :: (T_handle :: []))); modfunc_body =
    bank_prog } :: []); mod_tables = []; mod_mems = []; mod_globals = [];
    mod_elem = []; mod_start = (Some (Big_int_Z.succ_big_int
    Big_int_Z.zero_big_int)); mod_imports = ({ imp_module =
    ('E'::('n'::('v'::[]))); imp_name = ('a'::('d'::('v'::[]))); imp_desc =
    (ID_func (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) } :: []);
    mod_exports = [] }

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
    env_adv_prog } :: []); mod_tables = []; mod_mems = []; mod_globals = [];
    mod_elem = []; mod_start = None; mod_imports = []; mod_exports =
    ({ modexp_name = ('a'::('d'::('v'::[]))); modexp_desc = (MED_func
    Big_int_Z.zero_big_int) } :: []) }

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

(** val square0 : ws_basic_instruction list **)

let square0 =
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
  (I_const (Val_int (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2 Big_int_Z.unit_big_int))))))) :: ((I_const
    (Val_int (Big_int_Z.minus_big_int
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    (Big_int_Z.mult_int_big_int 2
    ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
    Big_int_Z.unit_big_int)))))) :: ((I_call
    Big_int_Z.zero_big_int) :: ((I_set_global Big_int_Z.zero_big_int) :: [])))

(** val tf_f_lib : function_type **)

let tf_f_lib =
  Tf ((T_int :: (T_int :: [])), (T_int :: []))

(** val f_lib : ws_basic_instruction list **)

let f_lib =
  (I_get_local Big_int_Z.zero_big_int) :: ((I_get_local
    (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)) :: ((I_binop (T_int,
    BOI_add)) :: ((I_if ((T_int :: []), ((I_const (Val_int
    Big_int_Z.zero_big_int)) :: []), ((I_const (Val_int
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
  app (repeat (I_const (Val_int Big_int_Z.unit_big_int)) n0)
    (app
      (repeat (I_binop (T_int, BOI_add))
        (sub n0 (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)))
      (I_drop :: ((I_const (Val_int (Big_int_Z.mult_int_big_int 2
      ((fun x -> Big_int_Z.succ_big_int (Big_int_Z.mult_int_big_int 2 x))
      (Big_int_Z.mult_int_big_int 2
      Big_int_Z.unit_big_int))))) :: (I_drop :: []))))

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
  (I_get_global Big_int_Z.zero_big_int) :: ((I_const (Val_int
    Big_int_Z.unit_big_int)) :: ((I_binop (T_int, BOI_add)) :: ((I_set_global
    Big_int_Z.zero_big_int) :: [])))

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

(** val compile :
    machineParameters -> ws_module -> name -> cerise_component option **)

let compile mP m n0 =
  mbind (Obj.magic (fun _ _ -> option_bind)) (fun labeled ->
    compile_component (register_allocation labeled))
    (Obj.magic compile_module mP m n0)

(** val compile_list :
    machineParameters -> (ws_module * name) list -> cerise_component option
    list **)

let compile_list mP ml =
  map (fun m -> compile mP (fst m) (snd m)) ml

(** val load_test :
    machineParameters -> Big_int_Z.big_int -> oType -> oType -> oType ->
    Big_int_Z.big_int -> Big_int_Z.big_int -> (ws_module * name) list ->
    (regName * word) list * (addr * word) list **)

let load_test mP size_safe_mem ot_g ot_lm ot_sm start_stack end_stack modules =
  match mbind (Obj.magic (fun _ _ -> option_bind)) (fun comps ->
          mbind (Obj.magic (fun _ _ -> option_bind)) (fun linked ->
            mbind (Obj.magic (fun _ _ -> option_bind)) (fun inst ->
              let (regs, mem0) = init_state mP inst start_stack end_stack in
              Some
              (Obj.magic ((gmap_to_list reg_eq_dec reg_countable regs),
                (sort (gmap_to_list Coq0_Nat.eq_dec nat_countable mem0)))))
              (Obj.magic load_in_memory mP linked size_safe_mem ot_g ot_lm
                ot_sm)) (Obj.magic instantiation comps))
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
    Big_int_Z.zero_big_int))))))))))))))))))))))))))))))))
    Big_int_Z.zero_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
    start_stack end_stack ((bank_module,
    ('B'::('a'::('n'::('k'::[]))))) :: ((env_module,
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
    Big_int_Z.zero_big_int))))))))))))))))))))))))))))))))
    Big_int_Z.zero_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
    start_stack end_stack ((client_module,
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
    Big_int_Z.zero_big_int))))))))))))))))))))))))))))))))
    Big_int_Z.zero_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
    start_stack end_stack ((dummy_module_client,
    ('d'::('u'::('m'::('m'::('y'::[])))))) :: ((dummy_module_lib,
    ('e'::('n'::('v'::[])))) :: []))

(** val loaded_reg_alloc_example :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    (regName * word) list * (addr * word) list **)

let loaded_reg_alloc_example mP start_stack end_stack =
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
    Big_int_Z.zero_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
    start_stack end_stack ((reg_alloc_module_client,
    ('m'::('a'::('i'::('n'::[]))))) :: [])

(** val loaded_incr_example :
    machineParameters -> Big_int_Z.big_int -> Big_int_Z.big_int ->
    (regName * word) list * (addr * word) list **)

let loaded_incr_example mP start_stack end_stack =
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
    Big_int_Z.zero_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int)
    (Big_int_Z.succ_big_int (Big_int_Z.succ_big_int Big_int_Z.zero_big_int))
    start_stack end_stack ((incr_module,
    ('i'::('n'::('c'::('r'::[]))))) :: [])
