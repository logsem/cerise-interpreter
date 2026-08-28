(* GENERATED FILE -- DO NOT EDIT. ExtrOcamlBasic maps basic Rocq datatypes to
   OCaml; otherwise there are no custom extraction mappings, and gmap is
   stdpp's extracted trie. Extraction erases proofs and refinements, so OCaml
   can construct out-of-bounds finz and R values. It also erases the laws of
   MachineParameters: the OCaml-supplied encoding functions and adapter must
   satisfy those laws and are trusted. Generated Obj.magic casts rely on the
   extractor and on well-formed inputs. *)


type __ = Obj.t

val negb : bool -> bool

type nat =
| O
| S of nat

val option_map : ('a1 -> 'a2) -> 'a1 option -> 'a2 option

type ('a, 'b) sum =
| Inl of 'a
| Inr of 'b

val fst : ('a1 * 'a2) -> 'a1

val snd : ('a1 * 'a2) -> 'a2

type comparison =
| Eq
| Lt
| Gt

val compOpp : comparison -> comparison

val id : __ -> __

val add : nat -> nat -> nat

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

module Nat :
 sig
  val eq_dec : nat -> nat -> bool
 end

module Pos :
 sig
  val succ : positive -> positive

  val add : positive -> positive -> positive

  val add_carry : positive -> positive -> positive

  val pred_double : positive -> positive

  val pred_N : positive -> n

  val mul : positive -> positive -> positive

  val iter : ('a1 -> 'a1) -> 'a1 -> positive -> 'a1

  val div2 : positive -> positive

  val div2_up : positive -> positive

  val compare_cont : comparison -> positive -> positive -> comparison

  val compare : positive -> positive -> comparison

  val eqb : positive -> positive -> bool

  val coq_Nsucc_double : n -> n

  val coq_Ndouble : n -> n

  val coq_lor : positive -> positive -> positive

  val coq_land : positive -> positive -> n

  val ldiff : positive -> positive -> n

  val iter_op : ('a1 -> 'a1 -> 'a1) -> positive -> 'a1 -> 'a1

  val to_nat : positive -> nat

  val of_succ_nat : nat -> positive
 end

module Coq_Pos :
 sig
  val succ : positive -> positive

  val pred_double : positive -> positive

  val pred : positive -> positive

  val eq_dec : positive -> positive -> bool
 end

module N :
 sig
  val succ_pos : n -> positive

  val coq_lor : n -> n -> n

  val coq_land : n -> n -> n

  val ldiff : n -> n -> n
 end

module Coq_N :
 sig
  val to_nat : n -> nat

  val of_nat : nat -> n

  val eq_dec : n -> n -> bool
 end

module Z :
 sig
  val double : z -> z

  val succ_double : z -> z

  val pred_double : z -> z

  val pos_sub : positive -> positive -> z

  val add : z -> z -> z

  val opp : z -> z

  val sub : z -> z -> z

  val mul : z -> z -> z

  val compare : z -> z -> comparison

  val leb : z -> z -> bool

  val ltb : z -> z -> bool

  val eqb : z -> z -> bool

  val of_N : n -> z

  val div2 : z -> z

  val shiftl : z -> z -> z

  val shiftr : z -> z -> z

  val coq_lor : z -> z -> z

  val coq_land : z -> z -> z

  val eq_dec : z -> z -> bool

  val b2z : bool -> z
 end

val le_lt_dec : nat -> nat -> bool

val le_gt_dec : nat -> nat -> bool

val le_dec : nat -> nat -> bool

val z_lt_dec : z -> z -> bool

val z_le_dec : z -> z -> bool

type decision = bool

val decide : decision -> bool

type ('a, 'b) relDecision = 'a -> 'b -> decision

val decide_rel : ('a1, 'a2) relDecision -> 'a1 -> 'a2 -> decision

type 'a empty = 'a

val empty0 : 'a1 empty -> 'a1

type 'm mBind = __ -> __ -> (__ -> 'm) -> 'm -> 'm

val mbind : 'a1 mBind -> ('a2 -> 'a1) -> 'a1 -> 'a1

type 'm fMap = __ -> __ -> (__ -> __) -> 'm -> 'm

val fmap : 'a1 fMap -> ('a2 -> 'a3) -> 'a1 -> 'a1

type ('k, 'a, 'm) lookup = 'k -> 'm -> 'a option

val lookup0 : ('a1, 'a2, 'a3) lookup -> 'a1 -> 'a3 -> 'a2 option

type ('k, 'a, 'm) insert = 'k -> 'a -> 'm -> 'm

val insert0 : ('a1, 'a2, 'a3) insert -> 'a1 -> 'a2 -> 'a3 -> 'a3

type ('k, 'a, 'm) partialAlter = ('a option -> 'a option) -> 'k -> 'm -> 'm

val partial_alter :
  ('a1, 'a2, 'a3) partialAlter -> ('a2 option -> 'a2 option) -> 'a1 -> 'a3 ->
  'a3

val and_dec : decision -> decision -> decision

val bool_eq_dec : (bool, bool) relDecision

val unit_eq_dec : (unit, unit) relDecision

val option_bind : (__ -> __ option) -> __ option -> __ option

val option_fmap : (__ -> __) -> __ option -> __ option

module Coq_Nat :
 sig
  val eq_dec : (nat, nat) relDecision

  val le_dec : (nat, nat) relDecision
 end

module Coq0_Pos :
 sig
  val eq_dec : (positive, positive) relDecision

  val reverse_go : positive -> positive -> positive

  val reverse : positive -> positive
 end

module Coq0_N :
 sig
  val eq_dec : (n, n) relDecision
 end

module Coq_Z :
 sig
  val eq_dec : (z, z) relDecision

  val le_dec : (z, z) relDecision

  val lt_dec : (z, z) relDecision
 end

type 'a countable = { encode : ('a -> positive);
                      decode : (positive -> 'a option) }

val unit_countable : unit countable

val option_countable :
  ('a1, 'a1) relDecision -> 'a1 countable -> 'a1 option countable

val sum_countable :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a2, 'a2) relDecision -> 'a2
  countable -> ('a1, 'a2) sum countable

val n_countable : n countable

val z_countable : z countable

val nat_countable : nat countable

type ('k, 'a, 'm) mapFold = __ -> ('k -> 'a -> __ -> __) -> __ -> 'm -> __

val map_fold :
  ('a1, 'a2, 'a3) mapFold -> ('a1 -> 'a2 -> 'a4 -> 'a4) -> 'a4 -> 'a3 -> 'a4

val map_insert : ('a1, 'a2, 'a3) partialAlter -> ('a1, 'a2, 'a3) insert

val map_to_list : ('a1, 'a2, 'a3) mapFold -> 'a3 -> ('a1 * 'a2) list

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

val gmap_dep_ne_case :
  'a1 gmap_dep_ne -> ('a1 gmap_dep -> (__ * 'a1) option -> 'a1 gmap_dep ->
  'a2) -> 'a2

val gmap_dep_ne_lookup : positive -> 'a1 gmap_dep_ne -> 'a1 option

val gmap_dep_lookup : positive -> 'a1 gmap_dep -> 'a1 option

val gmap_lookup :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2, ('a1, 'a2) gmap)
  lookup

val gmap_empty :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2) gmap empty

val gmap_dep_ne_singleton : positive -> 'a1 -> 'a1 gmap_dep_ne

val gmap_partial_alter_aux :
  (positive -> __ -> 'a1 gmap_dep_ne -> 'a1 gmap_dep) -> ('a1 option -> 'a1
  option) -> positive -> 'a1 gmap_dep -> 'a1 gmap_dep

val gmap_dep_ne_partial_alter :
  ('a1 option -> 'a1 option) -> positive -> 'a1 gmap_dep_ne -> 'a1 gmap_dep

val gmap_dep_partial_alter :
  ('a1 option -> 'a1 option) -> positive -> 'a1 gmap_dep -> 'a1 gmap_dep

val gmap_partial_alter :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a1, 'a2, ('a1, 'a2) gmap)
  partialAlter

val gmap_fold_aux :
  (positive -> 'a2 -> 'a1 gmap_dep_ne -> 'a2) -> positive -> 'a2 -> 'a1
  gmap_dep -> 'a2

val gmap_dep_ne_fold :
  (positive -> 'a1 -> 'a2 -> 'a2) -> positive -> 'a2 -> 'a1 gmap_dep_ne -> 'a2

val gmap_dep_fold :
  (positive -> 'a1 -> 'a2 -> 'a2) -> positive -> 'a2 -> 'a1 gmap_dep -> 'a2

val gmap_fold :
  ('a1, 'a1) relDecision -> 'a1 countable -> ('a1 -> 'a2 -> __ -> __) -> __
  -> ('a1, 'a2) gmap -> __

module Coq_finz :
 sig
  type finz =
  | FinZ of z

  val to_z : z -> finz -> z

  val of_z : z -> z -> finz option

  val leb : z -> finz -> finz -> bool

  val ltb : z -> finz -> finz -> bool

  val incr : z -> finz -> z -> finz option
 end

val finz_eq_dec : z -> (Coq_finz.finz, Coq_finz.finz) relDecision

val finz_countable : z -> Coq_finz.finz countable

val memNum : z

val withinBounds :
  z -> Coq_finz.finz -> Coq_finz.finz -> Coq_finz.finz -> bool

val isWithin :
  z -> Coq_finz.finz -> Coq_finz.finz -> Coq_finz.finz -> Coq_finz.finz ->
  bool

val oNum : z

val regNum : nat

type regName =
| PC
| R of nat

type sRegName =
| MTDC

val reg_eq_dec : (regName, regName) relDecision

val n_to_regname : nat -> regName option

val reg_countable : regName countable

val sreg_eq_dec : (sRegName, sRegName) relDecision

val sreg_countable : sRegName countable

val cnull : regName

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

val permit_seal : sealPerms -> bool

val permit_unseal : sealPerms -> bool

val executeAllowed : perm -> bool

val readAllowed : perm -> bool

val writeAllowed : perm -> bool

val has_sreg_access : perm -> bool

val isWL : perm -> bool

val isDL : perm -> bool

val isDRO : perm -> bool

val isLocal : locality -> bool

val rXPermFlowsTo : rXperm -> rXperm -> bool

val wPermFlowsTo : wperm -> wperm -> bool

val dLPermFlowsTo : dLperm -> dLperm -> bool

val dROPermFlowsTo : dROperm -> dROperm -> bool

val permFlowsTo : perm -> perm -> bool

val localityFlowsTo : locality -> locality -> bool

val sealPermFlowsTo : sealPerms -> sealPerms -> bool

type sealable =
| SCap of perm * locality * Coq_finz.finz * Coq_finz.finz * Coq_finz.finz
| SSealRange of sealPerms * locality * Coq_finz.finz * Coq_finz.finz
   * Coq_finz.finz

type word =
| WInt of z
| WSealable of sealable
| WSentry of perm * locality * Coq_finz.finz * Coq_finz.finz * Coq_finz.finz
| WSealed of Coq_finz.finz * sealable

val isLocalSealable : sealable -> bool

val isLocalWord : word -> bool

val canStore : perm -> word -> bool

val updatePcPerm : word -> word

val nonZero : word -> bool

val deeplocal_perm : perm -> perm

val deeplocal_sb : sealable -> sealable

val deeplocal : word -> word

val borrow_sb : sealable -> sealable

val borrow : word -> word

val readonly_perm : perm -> perm

val readonly_sb : sealable -> sealable

val readonly : word -> word

val load_word : perm -> word -> word

type mem = (Coq_finz.finz, word) gmap

type reg = (regName, word) gmap

type sReg = (sRegName, word) gmap

val lookup_reg : regName -> reg -> word option

val insert_reg : regName -> word -> reg -> reg

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

val decodeInstrW : machineParameters -> word -> instr

type execConf = (reg * sReg) * mem

type confFlag =
| Executable
| Halted
| Failed
| NextI

type conf = confFlag * execConf

val reg0 : execConf -> reg

val sreg : execConf -> sReg

val mem0 : execConf -> mem

val update_reg : execConf -> regName -> word -> execConf

val update_sreg : execConf -> sRegName -> word -> execConf

val update_mem : execConf -> Coq_finz.finz -> word -> execConf

val updatePC_gen : execConf -> z -> conf option

val updatePC : execConf -> conf option

val z_of_argument : reg -> (z, regName) sum -> z option

val word_of_argument : reg -> (z, regName) sum -> word option

val addr_of_argument : reg -> (z, regName) sum -> Coq_finz.finz option

val otype_of_argument : reg -> (z, regName) sum -> Coq_finz.finz option

val exec_opt : machineParameters -> instr -> perm -> execConf -> conf option

val exec : machineParameters -> instr -> perm -> execConf -> conf

val isCorrectPCb : word -> bool

val machine_step : machineParameters -> conf -> conf option

val reg_empty : reg

val sreg_empty : sReg

val mem_empty : mem

val reg_lookup : reg -> regName -> word option

val sreg_lookup : sReg -> sRegName -> word option

val mem_lookup : mem -> Coq_finz.finz -> word option

val reg_insert : reg -> regName -> word -> reg

val sreg_insert : sReg -> sRegName -> word -> sReg

val mem_insert : mem -> Coq_finz.finz -> word -> mem

val reg_elements : reg -> (regName * word) list

val sreg_elements : sReg -> (sRegName * word) list

val mem_elements : mem -> (Coq_finz.finz * word) list
