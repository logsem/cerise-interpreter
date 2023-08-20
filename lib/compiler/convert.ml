open Libmachine
open Misc
open Wasm

(** Convert Extract programs into Ast programs *)

module ConvertExtract = struct
  open Extract

  let perm (p : perm) : Ast.perm =
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

  let locality (g : locality) : Ast.locality =
    match g with
    | Local -> Ast.Local
    | Global -> Ast.Global
    | Directed -> Ast.Directed

  let wtype (wt : word) : Ast.wtype =
    match wt with
    | WInt _ -> Ast.W_I
    | WSealable (SCap _) -> Ast.W_Cap
    | WSealable (SSealRange _) -> Ast.W_SealRange
    | WSealed _ -> Ast.W_Sealed

  let regname (r : regName) : Ast.regname =
    match r with
    | PC -> Ast.PC
    | STK -> Ast.STK
    | R n -> Ast.Reg (Big_int_Z.int_of_big_int n)

  let sum (s : (Big_int_Z.big_int, regName) sum) : Ast.reg_or_const =
    match s with
    | Inr r -> Ast.Register (regname r)
    | Inl z -> Ast.Const z

  let instr (i : cerise_instruction) : Ast.machine_op =
    match i with
    | Jmp r -> Ast.Jmp (regname r)
    | Jnz (r1, r2) -> Ast.Jnz (regname r1, regname r2)
    | Mov (r, c) -> Ast.Move (regname r,
                              sum c)
    | Load (r1, r2) -> Ast.Load (regname r1,
                                 regname r2)
    | Store (r, c) -> Ast.Store (regname r,
                                 sum c)
    | Add (r, c1, c2) -> Ast.Add (regname r,
                                  sum c1,
                                  sum c2)
    | Sub (r, c1, c2) -> Ast.Sub (regname r,
                                  sum c1,
                                  sum c2)
    | Mul (r, c1, c2) -> Ast.Mul (regname r,
                                  sum c1,
                                  sum c2)
    | Rem (r, c1, c2) -> Ast.Rem (regname r,
                                  sum c1,
                                  sum c2)
    | Div (r, c1, c2) -> Ast.Div (regname r,
                                  sum c1,
                                  sum c2)
    | Lt0 (r, c1, c2) -> Ast.Lt (regname r,
                                 sum c1,
                                 sum c2)
    | Lea (r, c) -> Ast.Lea (regname r, sum c)
    | Restrict (r, c) -> Ast.Restrict (regname r,
                                       sum c)
    | Subseg (r, c1, c2) -> Ast.SubSeg (regname r,
                                        sum c1,
                                        sum c2)
    | GetL (r1, r2) -> Ast.GetL (regname r1, regname r2)
    | GetB (r1, r2) -> Ast.GetB (regname r1, regname r2)
    | GetE (r1, r2) -> Ast.GetE (regname r1, regname r2)
    | GetA (r1, r2) -> Ast.GetA (regname r1, regname r2)
    | GetP (r1, r2) -> Ast.GetP (regname r1, regname r2)
    | GetWType (r1, r2) -> Ast.GetWType (regname r1, regname r2)
    | GetOType (r1, r2) -> Ast.GetWType (regname r1, regname r2)
    | Seal (r1, r2, r3) -> Ast.Seal (regname r1, regname r2, regname r3)
    | UnSeal (r1, r2, r3) -> Ast.UnSeal (regname r1, regname r2, regname r3)

    | LoadU (r1, r2, c) -> Ast.LoadU (regname r1,
                                      regname r2,
                                      sum c)
    | StoreU (r, c1, c2) -> Ast.StoreU (regname r,
                                        sum c1,
                                        sum c2)
    | PromoteU r -> Ast.PromoteU (regname r)
    | Fail -> Ast.Fail
    | Halt -> Ast.Halt

  let nbar (nb : Extract.nbar) =
    match nb with | Finite z -> Infinite_z.Int z | P_infty -> Infinite_z.Inf

  let sealable (sb : Extract.sealable) =
    match sb with
    | SCap ((p,l), b, e, a) ->
      Ast.Cap (perm p, locality l, b, nbar e, a)
    | SSealRange ((p,l), b, e, a) -> Ast.SealRange (p, locality l, b, e, a)

  let word (w : Extract.word) =
    match w with
    | WInt z -> Ast.I z
    | WSealable sb -> Ast.Sealable (sealable sb)
    | WSealed (ot, sb) -> Ast.Sealed (ot, sealable sb)

  let convert_error_msg (m : Extract.errorMsg) = Misc.Utils.implode_string m
end

(** Convert Ast programs into Extract programs *)
module ConvertAst = struct
  open Extract

  let reg (r : Ast.regname) : regName =
    match r with
    | Ast.PC -> PC
    | Ast.STK -> STK
    | Ast.Reg n -> R (Big_int_Z.big_int_of_int n)

  let sum (c : Ast.reg_or_const) : (Big_int_Z.big_int, regName) sum =
    match c with
    | Register r -> Inr (reg r)
    | Const z -> Inl z

  let machine_op (s : Ast.machine_op) : cerise_instruction =
    match s with
    | Ast.Jmp r -> Jmp (reg r)
    | Ast.Jnz (r1, r2) -> Jnz (reg r1, reg r2)
    | Ast.Move (r, c) -> Mov (reg r, sum c)
    | Ast.Load (r1, r2) -> Load (reg r1, reg r2)
    | Ast.Store (r, c) -> Store (reg r, sum c)
    | Ast.Add (r, c1, c2) -> Add (reg r, sum c1, sum c2)
    | Ast.Sub (r, c1, c2) -> Sub (reg r, sum c1, sum c2)
    | Ast.Mul (r, c1, c2) -> Mul (reg r, sum c1, sum c2)
    | Ast.Rem (r, c1, c2) -> Rem (reg r, sum c1, sum c2)
    | Ast.Div (r, c1, c2) -> Div (reg r, sum c1, sum c2)
    | Ast.Lt (r, c1, c2) -> Lt0 (reg r, sum c1, sum c2)
    | Ast.Lea (r, c) -> Lea (reg r, sum c)
    | Ast.Restrict (r, c) -> Restrict (reg r, sum c)
    | Ast.SubSeg (r, c1, c2) -> Subseg (reg r, sum c1, sum c2)
    | Ast.GetL (r1, r2) -> GetL (reg r1, reg r2)
    | Ast.GetB (r1, r2) -> GetB (reg r1, reg r2)
    | Ast.GetE (r1, r2) -> GetE (reg r1, reg r2)
    | Ast.GetA (r1, r2) -> GetA (reg r1, reg r2)
    | Ast.GetP (r1, r2) -> GetP (reg r1, reg r2)
    | Ast.GetOType (r1, r2) -> GetOType (reg r1, reg r2)
    | Ast.GetWType (r1, r2) -> GetWType (reg r1, reg r2)
    | Ast.Seal (r1, r2, r3) -> Seal (reg r1, reg r2, reg r3)
    | Ast.UnSeal (r1, r2, r3) -> UnSeal (reg r1, reg r2, reg r3)
    | Ast.LoadU (r1, r2, c) -> LoadU (reg r1, reg r2, sum c)
    | Ast.StoreU (r, c1, c2) -> StoreU (reg r, sum c1, sum c2)
    | Ast.PromoteU r -> PromoteU (reg r)
    | Ast.Fail -> Fail
    | Ast.Halt -> Halt

  let perm (p : Ast.perm) : perm =
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

  let loc (g : Ast.locality) : locality =
    match g with
    | Ast.Local -> Local
    | Ast.Global -> Global
    | Ast.Directed -> Directed

  let wtype (wt : Ast.wtype) : word =
    match wt with
    | Ast.W_I -> wt_int
    | Ast.W_Cap -> wt_cap
    | Ast.W_SealRange -> wt_sealrange
    | Ast.W_Sealed -> wt_sealed
end

(** Ir_wasm *)

module ConvertWasm = struct
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
end

type extract_compiled_program = (((Extract.regName * Extract.word) list) * ((Extract.addr * Extract.word) list)) Extract.error
type compiled_prog =  (Ast.regname * Ast.word) list * (Z.t * Ast.word) list

(** Interface for utilies function *)
module ConvertInterface
  : sig
    val compile : Z.t -> Z.t -> Z.t -> Z.t -> Z.t -> Z.t -> Z.t -> Ir_wasm.ws_module list -> compiled_prog
  end
= struct
  (** Machine parameters *)
  exception CompilerException of string

  let machine_param =
    let open Encode in
    {
      Extract.decodeInstr = (function z -> ConvertAst.machine_op (decode_machine_op z));
      encodeInstr = (function i -> encode_machine_op (ConvertExtract.instr i));
      encodePerm = (function p -> encode_perm (ConvertExtract.perm p));
      encodeLoc = (function g -> encode_locality (ConvertExtract.locality g));

      decodePermPair = (function z -> let (p,g) = decode_perm_loc_pair z in (ConvertAst.perm p, ConvertAst.loc g));
      encodePermPair = (function p -> let (p,g) = p in encode_perm_loc_pair (ConvertExtract.perm p) (ConvertExtract.locality g));

      decodeSealPermPair = (function z -> let (p,g) = decode_seal_perm_loc_pair z in (p, ConvertAst.loc g));
      encodeSealPermPair = (function p -> let (p,g) = p in encode_seal_perm_loc_pair p (ConvertExtract.locality g));

      encodeSealPerms = (function p -> encode_seal_perm p);
      decodeSealPerms = (function p -> decode_seal_perm p);
      encodeWordType = (function wt -> encode_wtype (ConvertExtract.wtype wt));
      decodeWordType = (function wt -> ConvertAst.wtype (decode_wtype wt));
    }

  let compile
      (start_stack : Z.t)
      (ot_lm : Z.t)
      (ot_g : Z.t)
      (ot_sm : Z.t)
      (max_lin_mem : Z.t)
      (max_indirect_table : Z.t)
      (size_safe_mem : Z.t)
      (l : Ir_wasm.ws_module list)
    : compiled_prog
    =
    let open Ir_wasm in
    (* Convert the Ir_wasm.modules into Extract.modules *)
    let mods =
      List.map
        (fun m ->
           (ConvertWasm.extract_module m, Utils.explode_string m.mod_name))
        l
    in

    (* Compile, link and load the *)
    let compiled : extract_compiled_program  =
      Extract.load_test
        machine_param start_stack mods [] ot_lm ot_g ot_sm max_lin_mem max_indirect_table size_safe_mem
    in

    match compiled with
    | Extract.Error m -> raise @@ CompilerException (ConvertExtract.convert_error_msg m)
    | Extract.Ok (regs, compiled_prog) ->
      let regs =
        List.map
          (fun w -> (ConvertExtract.regname (fst w), ConvertExtract.word (snd w)))
          regs
      in
      let prog =
        List.map
          (fun w -> ((fst w), ConvertExtract.word (snd w)))
          compiled_prog
      in
      (regs, prog)
end
