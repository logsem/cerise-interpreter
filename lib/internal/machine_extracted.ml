open Ast

module E = Griotte_extracted
module MemMap = Machine.MemMap
module RegMap = Machine.RegMap
module SRegMap = Machine.SRegMap

(** Trust boundary and differences from the Rocq program

    - [E.machine_step] is extracted and proved sound/complete for valid Rocq
      states and a lawful [MachineParameters] instance.
    - Extraction, its [Obj.magic]-using [gmap] runtime, and erased dependent
      invariants are trusted.
    - Functions tagged [UNTRUSTED ADAPTER] are handwritten boundary code.
    - Differences from Rocq: handwritten loading, [Rem]/[Div] become [Fail],
      [NextI] is hidden, sparse UI reads use zero, and invalid public values
      become a delayed [Failed] state. *)

type exec_state = Running | Halted | Failed
type reg_state = word RegMap.t
type sreg_state = word SRegMap.t
type mem_state = word MemMap.t
type exec_conf = { reg : reg_state; sreg : sreg_state; mem : mem_state }
type t = { raw : E.conf; conversion_failed : bool }

exception DecodeException = Encode.DecodeException
exception CheckInitFailed = Machine.CheckInitFailed

(** UNTRUSTED ADAPTER: the four [init_*] values below reuse the existing loader
    because Griotte's stepper does not initialize memory or roots. *)
let init_reg_state = Machine.init_reg_state
let init_reg_state_zeros = Machine.init_reg_state_zeros
let init_sreg_state_zeros = Machine.init_sreg_state_zeros
let init_mem_state = Machine.init_mem_state

(** UNTRUSTED ADAPTER: the conversion functions through [e_instr] are needed
    because the public AST and extracted datatypes differ.  Their round trips
    are unproved; [reg_e] also assumes register numbers are in 0--31. *)
let rec e_nat_of_int n = if n <= 0 then E.O else E.S (e_nat_of_int (n - 1))

let rec int_of_e_nat = function E.O -> 0 | E.S n -> 1 + int_of_e_nat n

let rec e_positive_of_z n =
  if Z.equal n Z.one then E.XH
  else
    let q, r = Z.ediv_rem n (Z.of_int 2) in
    if Z.equal r Z.zero then E.XO (e_positive_of_z q) else E.XI (e_positive_of_z q)

let e_z_of_z n =
  match Z.sign n with
  | 0 -> E.Z0
  | 1 -> E.Zpos (e_positive_of_z n)
  | _ -> E.Zneg (e_positive_of_z (Z.neg n))

let rec z_of_e_positive = function
  | E.XH -> Z.one
  | E.XO p -> Z.mul (Z.of_int 2) (z_of_e_positive p)
  | E.XI p -> Z.succ (Z.mul (Z.of_int 2) (z_of_e_positive p))

let z_of_e_z = function
  | E.Z0 -> Z.zero
  | E.Zpos p -> z_of_e_positive p
  | E.Zneg p -> Z.neg (z_of_e_positive p)

let e_reg = function E.PC -> PC | E.R n -> Reg (int_of_e_nat n)
let reg_e = function PC -> E.PC | Reg n -> E.R (e_nat_of_int n)
let e_sreg E.MTDC = MTDC
let sreg_e MTDC = E.MTDC

let perm_e (rx, w, dl, dro) =
  let rx = match rx with Orx -> E.Orx | R -> E.R0 | X -> E.X | XSR -> E.XSR in
  let w = match w with Ow -> E.Ow | W -> E.W | WL -> E.WL in
  let dl = match dl with LG -> E.LG | DL -> E.DL in
  let dro = match dro with LM -> E.LM | DRO -> E.DRO in
  E.BPerm (rx, w, dl, dro)

let e_perm (E.BPerm (rx, w, dl, dro)) =
  let rx = match rx with E.Orx -> Orx | E.R0 -> R | E.X -> X | E.XSR -> XSR in
  let w = match w with E.Ow -> Ow | E.W -> W | E.WL -> WL in
  let dl = match dl with E.LG -> LG | E.DL -> DL in
  let dro = match dro with E.LM -> LM | E.DRO -> DRO in
  (rx, w, dl, dro)

let locality_e = function Global -> E.Global | Local -> E.Local
let e_locality = function E.Global -> Global | E.Local -> Local
let griotte_bound = Z.of_int 2_000_000

(** UNTRUSTED ADAPTER: addresses and object types are [finz] values in
    Rocq, hence malformed values cannot be supplied to the extracted function.
    OCaml's public AST uses unrestricted [Z.t], so reject values outside
    Griotte's fixed range.  The duplicated bound must match [MemNum]/[ONum]. *)
let finz_e z =
  if Z.(z < zero || z >= griotte_bound) then
    invalid_arg "Machine_extracted: value outside Griotte's finite range"
  else E.Coq_finz.FinZ (e_z_of_z z)

let e_finz (E.Coq_finz.FinZ z) = z_of_e_z z

let sealable_e = function
  | Cap (p, l, b, e, a) -> E.SCap (perm_e p, locality_e l, finz_e b, finz_e e, finz_e a)
  | SealRange (p, l, b, e, a) ->
      E.SSealRange (p, locality_e l, finz_e b, finz_e e, finz_e a)

let e_sealable = function
  | E.SCap (p, l, b, e, a) -> Cap (e_perm p, e_locality l, e_finz b, e_finz e, e_finz a)
  | E.SSealRange (p, l, b, e, a) ->
      SealRange (p, e_locality l, e_finz b, e_finz e, e_finz a)

let word_e = function
  | I z -> E.WInt (e_z_of_z z)
  | Sealable sb -> E.WSealable (sealable_e sb)
  | Sentry (p, l, b, e, a) ->
      E.WSentry (perm_e p, locality_e l, finz_e b, finz_e e, finz_e a)
  | Sealed (o, sb) -> E.WSealed (finz_e o, sealable_e sb)

let e_word = function
  | E.WInt z -> I (z_of_e_z z)
  | E.WSealable sb -> Sealable (e_sealable sb)
  | E.WSentry (p, l, b, e, a) ->
      Sentry (e_perm p, e_locality l, e_finz b, e_finz e, e_finz a)
  | E.WSealed (o, sb) -> Sealed (e_finz o, e_sealable sb)

let arg_e = function Register r -> E.Inr (reg_e r) | Const z -> E.Inl (e_z_of_z z)
let e_arg = function E.Inr r -> Register (e_reg r) | E.Inl z -> Const (z_of_e_z z)

let instr_e = function
  | Jalr (a, b) -> E.Jalr (reg_e a, reg_e b)
  | Jmp a -> E.Jmp (arg_e a)
  | Jnz (r, a) -> E.Jnz (arg_e a, reg_e r)
  | ReadSR (r, sr) -> E.ReadSR (reg_e r, sreg_e sr)
  | WriteSR (sr, r) -> E.WriteSR (sreg_e sr, reg_e r)
  | Move (r, a) -> E.Mov (reg_e r, arg_e a)
  | Load (a, b) -> E.Load (reg_e a, reg_e b)
  | Store (r, a) -> E.Store (reg_e r, arg_e a)
  | Lt (r, a, b) -> E.Lt0 (reg_e r, arg_e a, arg_e b)
  | Add (r, a, b) -> E.Add (reg_e r, arg_e a, arg_e b)
  | Sub (r, a, b) -> E.Sub (reg_e r, arg_e a, arg_e b)
  | Mul (r, a, b) -> E.Mul (reg_e r, arg_e a, arg_e b)
  | LAnd (r, a, b) -> E.LAnd (reg_e r, arg_e a, arg_e b)
  | LOr (r, a, b) -> E.LOr (reg_e r, arg_e a, arg_e b)
  | LShiftL (r, a, b) -> E.LShiftL (reg_e r, arg_e a, arg_e b)
  | LShiftR (r, a, b) -> E.LShiftR (reg_e r, arg_e a, arg_e b)
  | Lea (r, a) -> E.Lea (reg_e r, arg_e a)
  | Restrict (r, a) -> E.Restrict (reg_e r, arg_e a)
  | SubSeg (r, a, b) -> E.Subseg (reg_e r, arg_e a, arg_e b)
  | GetB (a, b) -> E.GetB (reg_e a, reg_e b)
  | GetE (a, b) -> E.GetE (reg_e a, reg_e b)
  | GetA (a, b) -> E.GetA (reg_e a, reg_e b)
  | GetP (a, b) -> E.GetP (reg_e a, reg_e b)
  | GetL (a, b) -> E.GetL (reg_e a, reg_e b)
  | GetWType (a, b) -> E.GetWType (reg_e a, reg_e b)
  | GetOType (a, b) -> E.GetOType (reg_e a, reg_e b)
  | Seal (a, b, c) -> E.Seal (reg_e a, reg_e b, reg_e c)
  | UnSeal (a, b, c) -> E.UnSeal (reg_e a, reg_e b, reg_e c)
  | Fail -> E.Fail
  (* UNTRUSTED ADAPTER: [Rem]/[Div] have no Griotte constructors, so the
     compatibility boundary maps them to [Fail]. *)
  | Rem _ | Div _ -> E.Fail
  | Halt -> E.Halt

let e_instr = function
  | E.Jmp a -> Jmp (e_arg a)
  | E.Jnz (a, r) -> Jnz (e_reg r, e_arg a)
  | E.Jalr (a, b) -> Jalr (e_reg a, e_reg b)
  | E.Mov (r, a) -> Move (e_reg r, e_arg a)
  | E.Load (a, b) -> Load (e_reg a, e_reg b)
  | E.Store (r, a) -> Store (e_reg r, e_arg a)
  | E.Lt0 (r, a, b) -> Lt (e_reg r, e_arg a, e_arg b)
  | E.Add (r, a, b) -> Add (e_reg r, e_arg a, e_arg b)
  | E.Sub (r, a, b) -> Sub (e_reg r, e_arg a, e_arg b)
  | E.Mul (r, a, b) -> Mul (e_reg r, e_arg a, e_arg b)
  | E.LAnd (r, a, b) -> LAnd (e_reg r, e_arg a, e_arg b)
  | E.LOr (r, a, b) -> LOr (e_reg r, e_arg a, e_arg b)
  | E.LShiftL (r, a, b) -> LShiftL (e_reg r, e_arg a, e_arg b)
  | E.LShiftR (r, a, b) -> LShiftR (e_reg r, e_arg a, e_arg b)
  | E.Lea (r, a) -> Lea (e_reg r, e_arg a)
  | E.Restrict (r, a) -> Restrict (e_reg r, e_arg a)
  | E.Subseg (r, a, b) -> SubSeg (e_reg r, e_arg a, e_arg b)
  | E.GetB (a, b) -> GetB (e_reg a, e_reg b)
  | E.GetE (a, b) -> GetE (e_reg a, e_reg b)
  | E.GetA (a, b) -> GetA (e_reg a, e_reg b)
  | E.GetP (a, b) -> GetP (e_reg a, e_reg b)
  | E.GetL (a, b) -> GetL (e_reg a, e_reg b)
  | E.GetWType (a, b) -> GetWType (e_reg a, e_reg b)
  | E.GetOType (a, b) -> GetOType (e_reg a, e_reg b)
  | E.Seal (a, b, c) -> Seal (e_reg a, e_reg b, e_reg c)
  | E.UnSeal (a, b, c) -> UnSeal (e_reg a, e_reg b, e_reg c)
  | E.ReadSR (r, sr) -> ReadSR (e_reg r, e_sreg sr)
  | E.WriteSR (sr, r) -> WriteSR (e_sreg sr, e_reg r)
  | E.Fail -> Fail
  | E.Halt -> Halt

let dummy_finz = finz_e Z.zero
let dummy_perm = perm_e null_perm

(** UNTRUSTED ADAPTER: extraction erases the [MachineParameters] laws.  These
    [Encode] callbacks are necessary to execute encoded programs, but are not
    yet proved lawful or total; only [decodeInstr] catches decoder errors. *)
let parameters : E.machineParameters =
  {
    (* Semantic difference on malformed encodings: Rocq's decoder is total.
       We totalize exceptions from the OCaml decoder by decoding to [Fail]. *)
    decodeInstr = (fun z -> try instr_e (Encode.decode_machine_op (z_of_e_z z)) with _ -> E.Fail);
    encodeInstr = (fun i -> e_z_of_z (Encode.encode_machine_op (e_instr i)));
    encodePerm = (fun p -> e_z_of_z (Encode.encode_perm (e_perm p)));
    decodePerm = (fun z -> perm_e (Encode.decode_perm (z_of_e_z z)));
    encodeLoc = (fun l -> e_z_of_z (Encode.encode_locality (e_locality l)));
    decodePermPair =
      (fun z -> let p, l = Encode.decode_perm_loc_pair (z_of_e_z z) in (perm_e p, locality_e l));
    encodePermPair =
      (fun (p, l) -> e_z_of_z (Encode.encode_perm_loc_pair (e_perm p) (e_locality l)));
    encodeSealPerms = (fun p -> e_z_of_z (Encode.encode_seal_perm p));
    decodeSealPerms = (fun z -> Encode.decode_seal_perm (z_of_e_z z));
    decodeSealPermPair =
      (fun z -> let p, l = Encode.decode_seal_perm_loc_pair (z_of_e_z z) in (p, locality_e l));
    encodeSealPermPair =
      (fun (p, l) -> e_z_of_z (Encode.encode_seal_perm_loc_pair p (e_locality l)));
    encodeWordType =
      (fun w ->
        e_z_of_z
          (Encode.encode_wtype
             (match w with
             | E.WInt _ -> W_I
             | E.WSealable (E.SCap _) -> W_Cap
             | E.WSealable (E.SSealRange _) -> W_SealRange
             | E.WSentry _ -> W_Sentry
             | E.WSealed _ -> W_Sealed)));
    decodeWordType =
      (fun z ->
        match Encode.decode_wtype (z_of_e_z z) with
        | W_I -> E.WInt E.Z0
        | W_Cap -> E.WSealable (E.SCap (dummy_perm, E.Global, dummy_finz, dummy_finz, dummy_finz))
        | W_SealRange ->
            E.WSealable (E.SSealRange ((false, false), E.Global, dummy_finz, dummy_finz, dummy_finz))
        | W_Sentry -> E.WSentry (dummy_perm, E.Global, dummy_finz, dummy_finz, dummy_finz)
        | W_Sealed ->
            E.WSealed (dummy_finz, E.SCap (dummy_perm, E.Global, dummy_finz, dummy_finz, dummy_finz)));
  }

(** UNTRUSTED ADAPTER: [exec_conf_e] builds extracted maps through their typed
    API; [e_exec_conf] converts the element snapshots needed by whole-map UI
    views. *)
let exec_conf_e { reg; sreg; mem } =
  let reg = RegMap.fold (fun r w rs -> E.reg_insert rs (reg_e r) (word_e w)) reg E.reg_empty in
  let sreg =
    SRegMap.fold (fun sr w srs -> E.sreg_insert srs (sreg_e sr) (word_e w)) sreg E.sreg_empty
  in
  let mem =
    MemMap.fold (fun a w m -> E.mem_insert m (finz_e a) (word_e w)) mem E.mem_empty
  in
  ((reg, sreg), mem)

let e_exec_conf (reg, sreg, mem) =
  {
    reg = List.fold_left (fun m (r, w) -> RegMap.add (e_reg r) (e_word w) m) RegMap.empty reg;
    sreg = List.fold_left (fun m (r, w) -> SRegMap.add (e_sreg r) (e_word w) m) SRegMap.empty sreg;
    mem = List.fold_left (fun m (a, w) -> MemMap.add (e_finz a) (e_word w) m) MemMap.empty mem;
  }

(** UNTRUSTED ADAPTER: Griotte returns [NextI] after an ordinary
    instruction and uses a separate administrative transition back to
    [Executable].  The OCaml [Machine] interface exposes only [Running], so the
    adapter identifies both flags without executing another instruction. *)
let e_flag = function E.Executable | E.NextI -> Running | E.Halted -> Halted | E.Failed -> Failed

(** UNTRUSTED ADAPTER: constructs the initial extracted configuration from the
    handwritten loader state and enforces the public CNULL policy. *)
let init reg sreg mem =
  let conf = { reg = RegMap.add cnull (I Z.zero) reg; sreg; mem } in
  { raw = (E.Executable, exec_conf_e conf); conversion_failed = false }

(** UNTRUSTED ADAPTER: the [get_*] functions translate extracted state to the
    legacy UI.  Whole maps are snapshots; stepping still uses extracted maps. *)
let get_exec_conf { raw = (_, conf); _ } =
  e_exec_conf
    (E.reg_elements (E.reg0 conf), E.sreg_elements (E.sreg conf), E.mem_elements (E.mem0 conf))
let get_regfile m = (get_exec_conf m).reg
let get_sregfile m = (get_exec_conf m).sreg
let get_memory m = (get_exec_conf m).mem
let get_exec_state { raw = (state, _); _ } = e_flag state

(** UNTRUSTED ADAPTER: [as_handwritten]/[check_init_config] reuse handwritten
    root validation because it is not in the extracted semantics. *)
let as_handwritten m =
  let { reg; sreg; mem } = get_exec_conf m in
  Machine.init reg sreg mem

let check_init_config m = Machine.check_init_config (as_handwritten m)

(** UNTRUSTED ADAPTER: [read_reg]/[read_sreg] convert extracted point lookups
    to the legacy inspection API. *)
let read_reg r { raw = (_, conf); _ } =
  match r with
  | Reg 0 -> I Z.zero
  | _ -> (
      match E.reg_lookup (E.reg0 conf) (reg_e r) with
      | Some w -> e_word w
      | None -> raise Not_found)

let read_sreg sr { raw = (_, conf); _ } =
  match E.sreg_lookup (E.sreg conf) (sreg_e sr) with
  | Some w -> e_word w
  | None -> raise Not_found

(** UNTRUSTED ADAPTER: [read_mem] displays an absent in-range cell as zero for
    compatibility; extracted execution sees the sparse map's [None]. *)
let read_mem addr { raw = (_, conf); _ } =
  if Z.(addr < zero || addr >= Parameters.get_max_addr ()) then None
  else
    match E.mem_lookup (E.mem0 conf) (finz_e addr) with
    | Some w -> Some (e_word w)
    | None -> Some (I Z.zero)

(** UNTRUSTED ADAPTER: [set_reg]/[set_mem] require AST conversion.  Invalid
    [finz] values are remembered and turned into [Failed] by [step]. *)
let set_reg r w m =
  try
    let state, conf = m.raw in
    let r, w =
      match r with Reg 0 -> (reg_e cnull, word_e (I Z.zero)) | _ -> (reg_e r, word_e w)
    in
    { raw = (state, E.update_reg conf r w); conversion_failed = false }
  with Invalid_argument _ -> { m with conversion_failed = true }

let set_mem addr w m =
  try
    let state, conf = m.raw in
    { raw = (state, E.update_mem conf (finz_e addr) (word_e w)); conversion_failed = false }
  with Invalid_argument _ -> { m with conversion_failed = true }

(** UNTRUSTED ADAPTER: calls the proved extracted step, but hides Griotte's
    administrative [NextI -> Executable] transition and applies the delayed
    conversion-failure policy required by the legacy interface. *)
let step m =
  let state, conf = m.raw in
  match state with
  | E.Halted | E.Failed -> None
  | E.Executable | E.NextI ->
      (* Semantic difference for ill-formed OCaml configurations: values
         outside Griotte's [finz] range are unrepresentable in Rocq.  A failed
         point update is remembered and exposed as a failed machine step. *)
      if m.conversion_failed then
        Some { raw = (E.Failed, conf); conversion_failed = false }
      else
        (* [NextI -> Executable] is Griotte's administrative transition.  The
           public interface performs it implicitly before the next instruction
           while retaining the exact [machine_step] result in [raw]. *)
        Option.map
          (fun raw -> { raw; conversion_failed = false })
          (E.machine_step parameters (E.Executable, conf))

(** UNTRUSTED ADAPTER: [step_n]/[run]/[decode_machine_op] are handwritten
    convenience functions belonging to the OCaml interface. *)
let rec step_n m n =
  if n > 0 then match step m with Some m' -> step_n m' (n - 1) | None -> Some m else Some m

let rec run m = match step m with Some m' -> run m' | None -> m

let decode_machine_op = Encode.decode_machine_op
