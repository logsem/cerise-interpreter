open Extract
open Libmachine
open Misc
open Wasm

let translate_perm (p : perm) : Ast.perm =
  match p with
  | O -> Ast.O
  | E -> Ast.E
  | RO -> Ast.RO
  | RX -> Ast.RX
  | RW -> Ast.RW
  | RWX -> Ast.RWX
  | RWL -> Ast.RWL
  | RWLX -> Ast.RWLX
  | URW -> Ast.URW
  | URWL -> Ast.URWL
  | URWX -> Ast.URWX
  | URWLX -> Ast.URWLX

let translate_locality (g : locality) : Ast.locality =
  match g with
  | Local -> Ast.Local
  | Global -> Ast.Global
  | Directed -> Ast.Directed

let translate_wtype (wt : word) : Ast.wtype =
  match wt with
  | WInt _ -> Ast.W_I
  | WSealable (SCap _) -> Ast.W_Cap
  | WSealable (SSealRange _) -> Ast.W_SealRange
  | WSealed _ -> Ast.W_Sealed

let translate_regname (r : regName) : Ast.regname =
  match r with
  | PC -> Ast.PC
  | STK -> Ast.STK
  | R n -> Ast.Reg (Big_int_Z.int_of_big_int n)

let translate_sum (s : (Big_int_Z.big_int, regName) sum) : Ast.reg_or_const =
match s with
| Inr r -> Ast.Register (translate_regname r)
| Inl z -> Ast.Const z

let translate_instr (i : cerise_instruction) : Ast.machine_op =
  match i with
  | Jmp r -> Ast.Jmp (translate_regname r)
  | Jnz (r1, r2) -> Ast.Jnz (translate_regname r1, translate_regname r2)
  | Mov (r, c) -> Ast.Move (translate_regname r,
                             translate_sum c)
  | Load (r1, r2) -> Ast.Load (translate_regname r1,
                               translate_regname r2)
  | Store (r, c) -> Ast.Store (translate_regname r,
                               translate_sum c)
  | Add (r, c1, c2) -> Ast.Add (translate_regname r,
                                translate_sum c1,
                                translate_sum c2)
  | Sub (r, c1, c2) -> Ast.Sub (translate_regname r,
                                translate_sum c1,
                                translate_sum c2)
  | Mul (r, c1, c2) -> Ast.Mul (translate_regname r,
                                translate_sum c1,
                                translate_sum c2)
  | Rem (r, c1, c2) -> Ast.Rem (translate_regname r,
                                translate_sum c1,
                                translate_sum c2)
  | Div (r, c1, c2) -> Ast.Div (translate_regname r,
                                translate_sum c1,
                                translate_sum c2)
  | Lt0 (r, c1, c2) -> Ast.Lt (translate_regname r,
                              translate_sum c1,
                              translate_sum c2)
  | Lea (r, c) -> Ast.Lea (translate_regname r, translate_sum c)
  | Restrict (r, c) -> Ast.Restrict (translate_regname r,
                                     translate_sum c)
  | Subseg (r, c1, c2) -> Ast.SubSeg (translate_regname r,
                                      translate_sum c1,
                                      translate_sum c2)
  | GetL (r1, r2) -> Ast.GetL (translate_regname r1, translate_regname r2)
  | GetB (r1, r2) -> Ast.GetB (translate_regname r1, translate_regname r2)
  | GetE (r1, r2) -> Ast.GetE (translate_regname r1, translate_regname r2)
  | GetA (r1, r2) -> Ast.GetA (translate_regname r1, translate_regname r2)
  | GetP (r1, r2) -> Ast.GetP (translate_regname r1, translate_regname r2)
  | GetWType (r1, r2) -> Ast.GetWType (translate_regname r1, translate_regname r2)
  | GetOType (r1, r2) -> Ast.GetWType (translate_regname r1, translate_regname r2)
  | Seal (r1, r2, r3) -> Ast.Seal (translate_regname r1, translate_regname r2, translate_regname r3)
  | UnSeal (r1, r2, r3) -> Ast.UnSeal (translate_regname r1, translate_regname r2, translate_regname r3)

  | LoadU (r1, r2, c) -> Ast.LoadU (translate_regname r1,
                                    translate_regname r2,
                                    translate_sum c)
  | StoreU (r, c1, c2) -> Ast.StoreU (translate_regname r,
                                      translate_sum c1,
                                      translate_sum c2)
  | PromoteU r -> Ast.PromoteU (translate_regname r)
  | Fail -> Ast.Fail
  | Halt -> Ast.Halt

let tr_reg (r : Ast.regname) : regName =
  match r with
  | Ast.PC -> PC
  | Ast.STK -> STK
  | Ast.Reg n -> R (Big_int_Z.big_int_of_int n)

let tr_sum (c : Ast.reg_or_const) : (Big_int_Z.big_int, regName) sum =
  match c with
  | Register r -> Inr (tr_reg r)
  | Const z -> Inl z
    (* TODO might require to use decode/encode from Ir *)
    (* Ir.decode *)
    (* match cp with *)
    (* | Const n -> Inl n *)
    (* | Perm (p,g)-> *)
    (*   let n = Encode.encode_perm_pair p g in *)
    (*   Inl n *)

let tr_machine_op (s : Ast.machine_op) : cerise_instruction =
  match s with
  | Ast.Jmp r -> Jmp (tr_reg r)
  | Ast.Jnz (r1, r2) -> Jnz (tr_reg r1, tr_reg r2)
  | Ast.Move (r, c) -> Mov (tr_reg r, tr_sum c)
  | Ast.Load (r1, r2) -> Load (tr_reg r1, tr_reg r2)
  | Ast.Store (r, c) -> Store (tr_reg r, tr_sum c)
  | Ast.Add (r, c1, c2) -> Add (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Sub (r, c1, c2) -> Sub (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Mul (r, c1, c2) -> Mul (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Rem (r, c1, c2) -> Rem (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Div (r, c1, c2) -> Div (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Lt (r, c1, c2) -> Lt0 (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.Lea (r, c) -> Lea (tr_reg r, tr_sum c)
  | Ast.Restrict (r, c) -> Restrict (tr_reg r, tr_sum c)
  | Ast.SubSeg (r, c1, c2) -> Subseg (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.GetL (r1, r2) -> GetL (tr_reg r1, tr_reg r2)
  | Ast.GetB (r1, r2) -> GetB (tr_reg r1, tr_reg r2)
  | Ast.GetE (r1, r2) -> GetE (tr_reg r1, tr_reg r2)
  | Ast.GetA (r1, r2) -> GetA (tr_reg r1, tr_reg r2)
  | Ast.GetP (r1, r2) -> GetP (tr_reg r1, tr_reg r2)
  | Ast.GetOType (r1, r2) -> GetOType (tr_reg r1, tr_reg r2)
  | Ast.GetWType (r1, r2) -> GetWType (tr_reg r1, tr_reg r2)
  | Ast.Seal (r1, r2, r3) -> Seal (tr_reg r1, tr_reg r2, tr_reg r3)
  | Ast.UnSeal (r1, r2, r3) -> UnSeal (tr_reg r1, tr_reg r2, tr_reg r3)
  | Ast.LoadU (r1, r2, c) -> LoadU (tr_reg r1, tr_reg r2, tr_sum c)
  | Ast.StoreU (r, c1, c2) -> StoreU (tr_reg r, tr_sum c1, tr_sum c2)
  | Ast.PromoteU r -> PromoteU (tr_reg r)
  | Ast.Fail -> Fail
  | Ast.Halt -> Halt

let tr_perm (p : Ast.perm) : perm =
  match p with
  | Ast.O -> O
  | Ast.E -> E
  | Ast.RO -> RO
  | Ast.RX -> RX
  | Ast.RW -> RW
  | Ast.RWX -> RWX
  | Ast.RWL -> RWL
  | Ast.RWLX -> RWLX
  | Ast.URW -> URW
  | Ast.URWL -> URWL
  | Ast.URWX -> URWX
  | Ast.URWLX -> URWLX

let tr_loc (g : Ast.locality) : locality =
  match g with
  | Ast.Local -> Local
  | Ast.Global -> Global
  | Ast.Directed -> Directed

let tr_wtype (wt : Ast.wtype) : word =
  match wt with
  | Ast.W_I -> wt_int
  | Ast.W_Cap -> wt_cap
  | Ast.W_SealRange -> wt_sealrange
  | Ast.W_Sealed -> wt_sealed

let encode_label_name (l : Big_int_Z.big_int list) : Big_int_Z.big_int =
  let rec encode_label_name' (l : Big_int_Z.big_int list)
    : Big_int_Z.big_int =
    match l with
    | [] -> Big_int_Z.big_int_of_int (-1)
    | n::l'-> Encode.encode_int_int n (encode_label_name' l')
    in
    encode_label_name' l

let encode_labeled_instr (i : labeled_instr) : Big_int_Z.big_int =
  let (^!) (opcode : Big_int_Z.big_int) (args : Big_int_Z.big_int) =
    Big_int_Z.add_big_int opcode (Big_int_Z.shift_left_big_int args 8)
  in
  match i with
  | BInstr i' ->
    (Big_int_Z.big_int_of_int (0x1)) ^! Encode.encode_machine_op (translate_instr i')
  | Label n -> (Big_int_Z.big_int_of_int (0x2)) ^! (encode_label_name n)
  | Br_Jmp n -> (Big_int_Z.big_int_of_int (0x3)) ^! (encode_label_name n)
  | Br_Jnz (n,r) -> (Big_int_Z.big_int_of_int (0x4))
                    ^! ( Encode.encode_int_int (encode_label_name n) r)

let decode_label_name (l : Big_int_Z.big_int) : Big_int_Z.big_int list =
  let rec decode_label_name' (l : Big_int_Z.big_int)
    : Big_int_Z.big_int list =
    if (Big_int_Z.eq_big_int l (Big_int_Z.big_int_of_int (-1)))
    then []
    else
      let (n, l') = Encode.decode_int l in
      n::(decode_label_name' l')
  in
  decode_label_name' l

let decode_labeled_instr (z : Big_int_Z.big_int) : labeled_instr =
    let opc = Big_int_Z.extract_big_int z 0 8 in
    let payload = Big_int_Z.shift_right_big_int z 8 in
    (* Label *)
    if opc = (Big_int_Z.big_int_of_int 0x1)
    then
      match (Encode.decode_machine_op payload) with
    | i -> BInstr (tr_machine_op i)
    | exception Encode.DecodeException s ->
      raise @@ Encode.DecodeException (s ^ " Error decoding labeled instruction: unrecognized BInstr")
    else

    if opc = (Big_int_Z.big_int_of_int 0x2)
    then Label (decode_label_name payload)
    else

    if opc = (Big_int_Z.big_int_of_int 0x3)
    then Br_Jmp (decode_label_name payload)
    else

    if opc = (Big_int_Z.big_int_of_int 0x4)
    then
      let (l_enc, r) = Encode.decode_int payload in
      Br_Jnz (decode_label_name l_enc, r)
    else
      raise @@ Encode.DecodeException "Error decoding labeled instruction: unrecognized opcode"

let machine_param = {
  decodeInstr = (function z -> tr_machine_op (Encode.decode_machine_op z));
  encodeInstr = (function i -> Encode.encode_machine_op (translate_instr i));
  encodePerm = (function p -> Encode.encode_perm (translate_perm p));
  encodeLoc = (function g -> Encode.encode_locality (translate_locality g));

  decodePermPair = (function z -> let (p,g) = Encode.decode_perm_loc_pair z in (tr_perm p, tr_loc g));
  encodePermPair = (function p -> let (p,g) = p in Encode.encode_perm_loc_pair (translate_perm p) (translate_locality g));

  decodeSealPermPair = (function z -> let (p,g) = Encode.decode_seal_perm_loc_pair z in (p, tr_loc g));
  encodeSealPermPair = (function p -> let (p,g) = p in Encode.encode_seal_perm_loc_pair p (translate_locality g));

  encodeSealPerms = (function p -> Encode.encode_seal_perm p);
  decodeSealPerms = (function p -> Encode.decode_seal_perm p);
  encodeWordType = (function wt -> Encode.encode_wtype (translate_wtype wt));
  decodeWordType = (function wt -> tr_wtype (Encode.decode_wtype wt));
}

let translate_nbar (nb : Extract.nbar) =
  match nb with | Finite z -> Infinite_z.Int z | P_infty -> Infinite_z.Inf

let translate_sealable (sb : Extract.sealable) =
  match sb with
  | SCap ((p,l), b, e, a) ->
    Ast.Cap (translate_perm p, translate_locality l, b, translate_nbar e, a)
  | SSealRange ((p,l), b, e, a) -> Ast.SealRange (p, translate_locality l, b, e, a)


let translate_word (w : Extract.word) =
  match w with
  | WInt z -> Ast.I z
  | WSealable sb -> Ast.Sealable (translate_sealable sb)
  | WSealed (ot, sb) -> Ast.Sealed (ot, translate_sealable sb)

let convert_error_msg (m : errorMsg) = (String.concat "" (List.map (String.make 1) m))

(** Ir_wasm *)

open Ir_wasm
let extract_value_type (vt : value_type) : Extract.value_type =
  match vt with | T_int -> Extract.T_int | T_handle -> Extract.T_handle

let extract_result_type (rt : result_type) : Extract.result_type =
  List.map extract_value_type rt

let extract_mod_type (ft : function_type) : Extract.function_type =
 match ft with
   | Tf (rt1, rt2) -> Extract.Tf (extract_result_type rt1,
                                  extract_result_type rt2)

let extract_limits (l : limits) : Extract.limits =
  { Extract.lim_min = l.lim_min;
    Extract.lim_max = l.lim_max; }

let extract_handle (h : handle) : Extract.handle =
  { Extract.base = h.base;
    Extract.offset = h.offset;
    Extract.bound = h.bound;
    Extract.valid = h.valid;
    Extract.id0 = h.id0;
  }

let extract_value (v : value) : Extract.value =
  (match v with
   | Val_int n -> Extract.Val_int n
   | Val_handle h -> Extract.Val_handle (extract_handle h))

let extract_table_type (tt : table_type) : Extract.table_type =
  extract_limits tt

let extract_mod_table (mt : module_table) : Extract.module_table =
  extract_table_type mt

let extract_memory_type (mt : memory_type) : Extract.memory_type =
  extract_limits mt

let extract_mutability (m : mutability) : Extract.mutability =
  (match m with | MUT_immut -> Extract.MUT_immut | MUT_mut -> Extract.MUT_mut)

let extract_global_type (gt : global_type) : Extract.global_type =
  { Extract.tg_mut = extract_mutability gt.tg_mut;
    Extract.tg_t = extract_value_type gt.tg_t}

let extract_mod_global (mg : module_glob) : Extract.module_glob =
  { Extract.modglob_type = extract_global_type mg.modglob_type;
    Extract.modglob_init = extract_value mg.modglob_init }

let extract_memidx (tidx : memidx) : Extract.memidx = tidx
let extract_tableidx (tidx : tableidx) : Extract.tableidx = tidx
let extract_funidx (tidx : funcidx) : Extract.funcidx = tidx
let extract_typeidx (tidx : typeidx) : Extract.typeidx = tidx
let extract_globalidx (tidx : globalidx) : Extract.globalidx = tidx

let extract_mod_elem (me : module_element) : Extract.module_element =
{ Extract.modelem_table = extract_tableidx me.modelem_table;
  Extract.modelem_offset = me.modelem_offset;
  Extract.modelem_init = me.modelem_init }

let extract_module_start (ms : module_start) : Extract.module_start =
extract_funidx ms

let extract_import_desc (d : import_desc) : Extract.import_desc =
  (match d with
   | ID_func tidx -> Extract.ID_func tidx
   | ID_table tt -> Extract.ID_table (extract_table_type tt)
   | ID_mem mt -> Extract.ID_mem (extract_memory_type mt)
   | ID_global gt -> Extract.ID_global (extract_global_type gt))

let extract_mod_import (imp : module_import ) : Extract.module_import =
  { Extract.imp_module = Utils.explode_string imp.imp_module;
    Extract.imp_name = Utils.explode_string imp.imp_name;
    Extract.imp_desc = extract_import_desc imp.imp_desc }

let extract_export_desc (d : module_export_desc) : Extract.module_export_desc =
  (match d with
   | MED_func fidx -> Extract.MED_func (extract_funidx fidx)
   | MED_table tidx -> Extract.MED_table (extract_tableidx tidx)
   | MED_mem midx -> Extract.MED_mem (extract_memidx midx)
   | MED_global gidx -> Extract.MED_global (extract_globalidx gidx))

let extract_mod_export (exp : module_export) : Extract.module_export =
  { Extract.modexp_name = Utils.explode_string exp.modexp_name;
    Extract.modexp_desc = extract_export_desc exp.modexp_desc }


let extract_sx (s : sx) : Extract.sx =
  match s with
  | SX_S -> Extract.SX_S
  | SX_U -> Extract.SX_U

let extract_relop (op : relop) : Extract.relop =
  (match op with
   | ROI_eq -> Extract.ROI_eq
   | ROI_ne -> Extract.ROI_ne
   | ROI_lt s -> Extract.ROI_lt (extract_sx s)
   | ROI_gt s -> Extract.ROI_gt (extract_sx s)
   | ROI_le s -> Extract.ROI_le (extract_sx s)
   | ROI_ge s -> Extract.ROI_ge (extract_sx s)
  )

let extract_binop (op : binop) : Extract.binop =
  (match op with
   | BOI_add -> Extract.BOI_add
   | BOI_sub -> Extract.BOI_sub
   | BOI_mul -> Extract.BOI_mul
   | BOI_rem s -> Extract.BOI_rem (extract_sx s)
   | BOI_div s -> Extract.BOI_div (extract_sx s))

let rec extract_ws_binstr (i : ws_basic_instruction) : Extract.ws_basic_instruction =
  (match i with
   | I_unreachable -> Extract.I_unreachable
   | I_nop -> Extract.I_nop
   | I_drop -> Extract.I_drop
   | I_select -> Extract.I_select
   | I_block (rt, e) ->
     Extract.I_block (extract_result_type rt, List.map extract_ws_binstr e)
   | I_loop (rt, e) ->
     Extract.I_loop (extract_result_type rt, List.map extract_ws_binstr e)
   | I_if (rt, e1, e2) ->
     Extract.I_if
       (extract_result_type rt, List.map extract_ws_binstr e1, List.map extract_ws_binstr e2)
   | I_br i -> Extract.I_br i
   | I_br_if i -> Extract.I_br_if i
   | I_return -> Extract.I_return
   | I_call i -> Extract.I_call i
   | I_call_indirect i -> Extract.I_call_indirect i
   | I_get_local i -> Extract.I_get_local i
   | I_set_local i -> Extract.I_set_local i
   | I_tee_local i -> Extract.I_tee_local i
   | I_get_global i -> Extract.I_get_global i
   | I_set_global i -> Extract.I_set_global i
   | I_load vt -> Extract.I_load (extract_value_type vt)
   | I_store vt -> Extract.I_store (extract_value_type vt)
   | I_current_memory -> Extract.I_current_memory
   | I_grow_memory -> Extract.I_grow_memory
   | I_segload vt -> Extract.I_segload (extract_value_type vt)
   | I_segstore vt -> Extract.I_segstore (extract_value_type vt)
   | I_segalloc -> Extract.I_segalloc
   | I_segfree -> Extract.I_segfree
   | I_slice -> Extract.I_slice
   | I_handleadd -> Extract.I_handleadd
   | I_const v -> Extract.I_const (extract_value v)
   | I_binop (vt, op) -> Extract.I_binop (extract_value_type vt, extract_binop op)
   | I_testop vt -> Extract.I_testop (extract_value_type vt)
   | I_relop (vt, op) -> Extract.I_relop (extract_value_type vt, extract_relop op))

let extract_expr (e : expr) : Extract.expr =
  List.map extract_ws_binstr e

let extract_mod_func (f : module_func) : Extract.module_func =
 { Extract.modfunc_type = (extract_typeidx f.modfunc_type);
   Extract.modfunc_locals = List.map extract_value_type f.modfunc_locals;
   Extract.modfunc_body = extract_expr f.modfunc_body}

let extract_module (m : ws_module) : Extract.ws_module =
  { Extract.mod_types = List.map extract_mod_type m.mod_types;
    Extract.mod_funcs = List.map extract_mod_func m.mod_funcs;
    Extract.mod_tables = List.map extract_mod_table m.mod_tables;
    Extract.mod_mems = List.map extract_memory_type m.mod_mems;
    Extract.mod_globals = List.map extract_mod_global m.mod_globals;
    Extract.mod_elem = List.map extract_mod_elem m.mod_elem;
    Extract.mod_start =
      begin
      match m.mod_start with
      | None -> None
      | Some mstart -> Some (extract_module_start mstart)
    end ;
    Extract.mod_imports = List.map extract_mod_import m.mod_imports;
    Extract.mod_exports = List.map extract_mod_export m.mod_exports;
  }


let compile (l : Ir_wasm.ws_module list)
    mp start_stack ot_lm ot_g ot_sm max_lin_mem max_indirect_table size_safe_mem
  =
  let open Ir_wasm in
  let mods =
    List.map
      (fun m ->
         (extract_module m, Utils.explode_string m.mod_name))
      l
  in
  Extract.load_test
    mp start_stack mods [] ot_lm ot_g ot_sm max_lin_mem max_indirect_table size_safe_mem
