open Ast
(* open Parameters *)

let ( ~$ ) = Z.( ~$ )

exception NotYetImplemented

module MemMap = Map.Make (Z)

module RegMap = Map.Make (struct
  type t = regname

  let compare = compare_regname
end)

type exec_state = Running | Halted | Failed
type reg_state = word RegMap.t
type mem_state = word MemMap.t
type exec_conf = { reg : reg_state; mem : mem_state }

(* Architectural roots *)
(* - memory root, LD_LG_LM_MC_SD_SL     --> R_W_WL *)
let arch_root_memory_perm = PermSet.of_list [ R; W; WL ]

(* - executable root, EX_LD_LG_LM_MC_SR --> R_X_SR *)
let arch_root_executable_perm = PermSet.of_list [ R; X; SR ]

let is_derived_from (p_dst : PermSet.t) (p_src : PermSet.t) =
  if PermSet.equal p_dst p_src (* p <= p *) then true
  else if PermSet.mem E p_dst (* E <= RX *) then
    PermSet.equal (PermSet.of_list [ E ]) p_dst
    && PermSet.mem R p_src && PermSet.mem X p_src
    && (not (PermSet.mem DL p_src))
    && not (PermSet.mem DI p_src)
  else
    (* p_dst ⊆ p_src, except that we can add DI or DL *)
    PermSet.subset (PermSet.remove DL (PermSet.remove DI p_dst)) p_src

(* using a record to have notation similar to the paper *)
type mchn = exec_state * exec_conf

let init_reg_state : reg_state =
  let start_heap_addr = ~$0 in
  let max_heap_addr = Parameters.get_max_addr () in
  let start_otype = ~$0 in
  let max_otype = Parameters.get_max_otype () in

  let l =
    let n = 30 in
    List.init n (fun i -> (Reg (i + 1), I Z.zero))
  in

  let root_exec =
    Sealable
      (Cap (arch_root_executable_perm, Global, start_heap_addr, max_heap_addr, start_heap_addr))
  in
  let root_mem =
    Sealable (Cap (arch_root_memory_perm, Global, start_heap_addr, max_heap_addr, start_heap_addr))
  in
  let root_sealing =
    Sealable (SealRange ((true, true), Global, start_otype, max_otype, start_otype))
  in

  (* The PC register starts with full exec permission over the entire "heap" segment *)
  let pc_init = [ (PC, root_exec) ] in
  (* The CGP register starts with full mem permission over the entire "heap" segment *)
  let cgp_init = [ (cgp, root_mem) ] in
  (* The R0 register starts with full sealing permission over the entire otypes region *)
  let sealing_init =
    let sealing_reg = Reg 0 in
    [ (sealing_reg, root_sealing) ]
  in

  (* The stk register starts with full permission over the entire "stack" segment *)
  let seq = List.to_seq (pc_init @ l @ sealing_init @ cgp_init) in
  RegMap.of_seq seq

let get_reg (r : regname) ({ reg; _ } : exec_conf) : word = RegMap.find r reg
let ( @! ) x y = get_reg x y

let upd_reg (r : regname) (w : word) ({ reg; mem } : exec_conf) : exec_conf =
  { reg = RegMap.add r w reg; mem }

let init_mem_state (addr_start : Z.t) (prog : t) : mem_state =
  let zeroed_mem =
    let rec loop (i : Z.t) m =
      let addr_max = Parameters.get_max_addr () in
      (* NB: addr_max is not addressable *)
      if i >= addr_max then m else loop Z.(i + ~$1) (MemMap.add i (I ~$0) m)
    in
    loop Z.zero MemMap.empty
  in
  let enc_prog =
    List.to_seq
    @@ List.mapi
         (fun i x ->
           let i = ~$i in
           ( Z.(i + addr_start),
             match x with
             | Op op -> I (Encode.encode_machine_op op)
             | Word (Ast.I z) -> I z
             | Word (Ast.Sealable (Ast.Cap (p, l, b, e, a))) -> Sealable (Cap (p, l, b, e, a))
             | Word (Ast.Sealable (Ast.SealRange (p, l, b, e, a))) ->
                 Sealable (SealRange (p, l, b, e, a))
             | Word (Ast.Sealed (o, sb)) -> Sealed (o, sb) ))
         prog
  in
  MemMap.add_seq enc_prog zeroed_mem

let get_mem (addr : Z.t) (conf : exec_conf) : word option = MemMap.find_opt addr conf.mem
let ( @? ) x y = get_mem x y

let upd_mem (addr : Z.t) (w : word) ({ reg; mem } : exec_conf) : exec_conf =
  { reg; mem = MemMap.add addr w mem }

let init (initial_regs : word RegMap.t) (initial_mems : word MemMap.t) =
  (Running, { reg = initial_regs; mem = initial_mems })

let get_word (conf : exec_conf) (roc : reg_or_const) : word =
  match roc with Register r -> get_reg r conf | Const i -> I i

let upd_pc (conf : exec_conf) : mchn =
  match PC @! conf with
  | Sealable (Cap (p, g, b, e, a)) ->
      (Running, upd_reg PC (Sealable (Cap (p, g, b, e, Z.(a + ~$1)))) conf)
  | _ -> (Failed, conf)

let ( !> ) conf = upd_pc conf

let upd_pc_perm (w : word) =
  match w with
  | Sealable (Cap (p, g, b, e, a)) when PermSet.equal p (PermSet.singleton E) ->
      Sealable (Cap (PermSet.of_list [ R; X ], g, b, e, a))
  | _ -> w

let fetch_decode (conf : exec_conf) : machine_op option =
  match PC @! conf with
  | Sealable (Cap (_, _, _, _, addr)) -> (
      match get_mem addr conf with
      | Some (I enc) -> (
          try Some (Encode.decode_machine_op enc) with Encode.DecodeException _ -> None)
      | _ -> None)
  | _ -> None

let is_WLperm (p : PermSet.t) : bool = PermSet.mem WL p && PermSet.mem W p
let is_exec (p : PermSet.t) : bool = PermSet.mem R p && PermSet.mem X p
let can_write (p : PermSet.t) : bool = PermSet.mem W p
let can_read (p : PermSet.t) : bool = PermSet.mem R p

let is_pc_valid (conf : exec_conf) : bool =
  match PC @! conf with
  | Sealable (Cap (p, _, b, e, a)) when is_exec p ->
      if b <= a && a < e then Option.is_some @@ a @? conf else false
  | _ -> false

let perm_flowsto (p1 : PermSet.t) (p2 : PermSet.t) : bool = is_derived_from p1 p2

let locality_flowsto (g1 : locality) (g2 : locality) : bool =
  match g1 with Local -> true | Global -> ( match g2 with Global -> true | _ -> false)

let sealperm_flowsto (p1 : seal_perm) (p2 : seal_perm) : bool =
  let p_flows p p' = match (p, p') with false, _ -> true | true, true -> true | _, _ -> false in
  let s1, u1 = p1 in
  let s2, u2 = p2 in
  p_flows s1 s2 && p_flows u1 u2

let get_wtype (w : word) : wtype =
  match w with
  | I _ -> W_I
  | Sealable (Cap _) -> W_Cap
  | Sealable (SealRange _) -> W_SealRange
  | Sealed (_, _) -> W_Sealed

let get_locality_sealable (s : sealable) =
  match s with Cap (_, l, _, _, _) | SealRange (_, l, _, _, _) -> l

let is_sealrange (sb : sealable) = match sb with SealRange _ -> true | _ -> false
let is_cap (sb : sealable) = match sb with Cap _ -> true | _ -> false

let load_deep_local_sealable (w : sealable) : sealable =
  let deep_local_perm p = PermSet.add DL p in
  match w with
  | Cap (p, _, b, e, a) -> Cap (deep_local_perm p, Local, b, e, a)
  | SealRange (p, _, b, e, a) -> SealRange (p, Local, b, e, a)

let load_deep_local (w : word) : word =
  match w with
  | Sealable s -> Sealable (load_deep_local_sealable s)
  | Sealed (ot, s) -> Sealed (ot, load_deep_local_sealable s)
  | _ -> w

let load_deep_immutable_sealable (w : sealable) : sealable =
  let deep_immutable_perm p = PermSet.add DI (PermSet.remove WL (PermSet.remove W p)) in
  match w with Cap (p, g, b, e, a) -> Cap (deep_immutable_perm p, g, b, e, a) | _ -> w

let load_deep_immutable (w : word) : word =
  match w with
  | Sealable s -> Sealable (load_deep_immutable_sealable s)
  | Sealed (ot, s) -> Sealed (ot, load_deep_immutable_sealable s)
  | _ -> w

(* NOTE Although we've already check that not supported instructions / capabilities *)
(*  are not in the initial machine, we still need to make sure that *)
(*  the user does not encode not supported instructions *)
let exec_single (conf : exec_conf) : mchn =
  let fail_state = (Failed, conf) in
  if is_pc_valid conf then
    match fetch_decode conf with
    | None -> fail_state
    | Some instr -> (
        match instr with
        | Fail -> (Failed, conf)
        | Halt -> (Halted, conf)
        | Jalr (r_dst, r_src) when (not (r_dst = mtcc)) && not (r_src = mtcc) -> (
            match PC @! conf with
            | Sealable (Cap (_, g, b, e, a)) ->
                let new_pc = upd_pc_perm (r_src @! conf) in
                let link_cap = Sealable (Cap (PermSet.singleton E, g, b, e, Z.(a + Z.one))) in
                (Running, upd_reg PC new_pc (upd_reg r_dst link_cap conf))
            | _ -> fail_state)
        | Jmp c when not (c = Register mtcc) -> (
            match (get_word conf c, PC @! conf) with
            | I imm, Sealable (Cap (p, g, b, e, a)) ->
                let new_pc = Sealable (Cap (p, g, b, e, Z.(a + imm))) in
                (Running, upd_reg PC new_pc conf)
            | _ -> fail_state)
        | Jnz (r_src, c) when (not (r_src = mtcc)) && not (c = Register mtcc) -> (
            match r_src @! conf with
            | I i when Z.(equal i zero) -> !>conf
            | _ -> (
                match (get_word conf c, PC @! conf) with
                | I imm, Sealable (Cap (p, g, b, e, a)) ->
                    let new_pc = Sealable (Cap (p, g, b, e, Z.(a + imm))) in
                    (Running, upd_reg PC new_pc conf)
                | _, _ -> fail_state))
        | MoveSR (r, c) when r = mtcc && not (c = Register mtcc) ->
            let w = get_word conf c in
            !>(upd_reg r w conf)
        | Move (r, c) when (not (r = mtcc)) && not (c = Register mtcc) ->
            let w = get_word conf c in
            !>(upd_reg r w conf)
        | Load (r1, r2) when (not (r1 = mtcc)) && not (r2 = mtcc) -> (
            match r2 @! conf with
            | Sealable (Cap (p, _, b, e, a)) ->
                if can_read p then
                  match a @? conf with
                  | Some w when b <= a && a < e ->
                      let w = if PermSet.mem DL p then load_deep_local w else w in
                      let w = if PermSet.mem DI p then load_deep_immutable w else w in
                      !>(upd_reg r1 w conf)
                  | _ -> fail_state
                else fail_state
            | _ -> fail_state)
        | Store (r, c) when (not (r = mtcc)) && not (c = Register mtcc) -> (
            let w = get_word conf c in
            match r @! conf with
            | Sealable (Cap (p, _, b, e, a)) when b <= a && a < e ->
                if can_write p then
                  match w with
                  (* We consider that a Directed sealing capability is similar to Local *)
                  | (Sealed (_, sb) | Sealable sb) when get_locality_sealable sb = Local ->
                      if is_WLperm p then !>(upd_mem a w conf) else fail_state
                  | _ -> !>(upd_mem a w conf)
                else fail_state
            | _ -> fail_state)
        | Restrict (r, c) when (not (r = mtcc)) && not (c = Register mtcc) -> (
            match r @! conf with
            | Sealable (Cap (p, g, b, e, a)) -> (
                match get_word conf c with
                | I i -> (
                    let decode_p' =
                      try Some (Encode.decode_perm_loc_pair i)
                      with Encode.DecodeException _ -> None
                    in
                    match decode_p' with
                    | None -> fail_state
                    | Some (p', g') ->
                        if perm_flowsto p' p && locality_flowsto g' g then
                          !>(upd_reg r (Sealable (Cap (p', g', b, e, a))) conf)
                        else fail_state)
                | _ -> fail_state)
            | Sealable (SealRange (sp, g, b, e, a)) -> (
                match get_word conf c with
                | I i -> (
                    let decode_sp' =
                      try Some (Encode.decode_seal_perm_loc_pair i)
                      with Encode.DecodeException _ -> None
                    in
                    match decode_sp' with
                    | None -> fail_state
                    | Some (sp', g') ->
                        if sealperm_flowsto sp' sp && locality_flowsto g' g then
                          !>(upd_reg r (Sealable (SealRange (sp', g', b, e, a))) conf)
                        else fail_state)
                | _ -> fail_state)
            | _ -> fail_state)
        | SubSeg (r, c1, c2)
          when (not (r = mtcc)) && (not (c1 = Register mtcc)) && not (c2 = Register mtcc) -> (
            match r @! conf with
            | Sealable (Cap (p, g, b, e, a)) -> (
                let w1 = get_word conf c1 in
                let w2 = get_word conf c2 in
                match (w1, w2) with
                | I z1, I z2 ->
                    if b <= z1 && Z.(~$0 <= z2) && Z.zero <= e && not (PermSet.mem E p) then
                      let w = Sealable (Cap (p, g, z1, z2, a)) in
                      !>(upd_reg r w conf)
                    else fail_state
                | _ -> fail_state)
            | Sealable (SealRange (p, g, b, e, a)) -> (
                let w1 = get_word conf c1 in
                let w2 = get_word conf c2 in
                match (w1, w2) with
                | I z1, I z2 ->
                    if b <= z1 && Z.(~$0 <= z2) && Z.(~$0 <= e) then
                      let w = Sealable (SealRange (p, g, z1, z2, a)) in
                      !>(upd_reg r w conf)
                    else fail_state
                | _ -> fail_state)
            | _ -> fail_state)
        | Lea (r, c) when (not (r = mtcc)) && not (c = Register mtcc) -> (
            match r @! conf with
            | Sealable (Cap (p, g, b, e, a)) -> (
                let w = get_word conf c in
                match w with
                | I z when not (PermSet.mem E p) ->
                    !>(upd_reg r (Sealable (Cap (p, g, b, e, Z.(a + z)))) conf)
                | _ -> fail_state)
            | Sealable (SealRange (p, g, b, e, a)) -> (
                let w = get_word conf c in
                match w with
                | I z -> !>(upd_reg r (Sealable (SealRange (p, g, b, e, Z.(a + z)))) conf)
                | _ -> fail_state)
            | _ -> fail_state)
        | Add (r, c1, c2)
          when (not (r = mtcc)) && (not (c1 = Register mtcc)) && not (c2 = Register mtcc) -> (
            let w1 = get_word conf c1 in
            let w2 = get_word conf c2 in
            match (w1, w2) with I z1, I z2 -> !>(upd_reg r (I Z.(z1 + z2)) conf) | _ -> fail_state)
        | Sub (r, c1, c2)
          when (not (r = mtcc)) && (not (c1 = Register mtcc)) && not (c2 = Register mtcc) -> (
            let w1 = get_word conf c1 in
            let w2 = get_word conf c2 in
            match (w1, w2) with I z1, I z2 -> !>(upd_reg r (I Z.(z1 - z2)) conf) | _ -> fail_state)
        | Mul (r, c1, c2)
          when (not (r = mtcc)) && (not (c1 = Register mtcc)) && not (c2 = Register mtcc) -> (
            let w1 = get_word conf c1 in
            let w2 = get_word conf c2 in
            match (w1, w2) with I z1, I z2 -> !>(upd_reg r (I Z.(z1 * z2)) conf) | _ -> fail_state)
        | Rem (r, c1, c2)
          when (not (r = mtcc)) && (not (c1 = Register mtcc)) && not (c2 = Register mtcc) -> (
            let w1 = get_word conf c1 in
            let w2 = get_word conf c2 in
            match (w1, w2) with
            | I z1, I z2 when z2 != Z.zero -> !>(upd_reg r (I Z.(z1 mod z2)) conf)
            | _ -> fail_state)
        | Div (r, c1, c2)
          when (not (r = mtcc)) && (not (c1 = Register mtcc)) && not (c2 = Register mtcc) -> (
            let w1 = get_word conf c1 in
            let w2 = get_word conf c2 in
            match (w1, w2) with
            | I z1, I z2 when z2 != Z.zero -> !>(upd_reg r (I Z.(z1 / z2)) conf)
            | _ -> fail_state)
        | Lt (r, c1, c2)
          when (not (r = mtcc)) && (not (c1 = Register mtcc)) && not (c2 = Register mtcc) -> (
            let w1 = get_word conf c1 in
            let w2 = get_word conf c2 in
            match (w1, w2) with
            | I z1, I z2 when Z.(lt z1 z2) -> !>(upd_reg r (I Z.one) conf)
            | I _, I _ -> !>(upd_reg r (I Z.zero) conf)
            | _ -> fail_state)
        | GetL (r1, r2) when (not (r1 = mtcc)) && not (r2 = mtcc) -> (
            match r2 @! conf with
            | Sealable sb | Sealed (_, sb) ->
                let g = get_locality_sealable sb in
                !>(upd_reg r1 (I (Encode.encode_locality g)) conf)
            | _ -> fail_state)
        | GetB (r1, r2) when (not (r1 = mtcc)) && not (r2 = mtcc) -> (
            match r2 @! conf with
            | Sealable (SealRange (_, _, b, _, _)) | Sealable (Cap (_, _, b, _, _)) ->
                !>(upd_reg r1 (I b) conf)
            | _ -> fail_state)
        | GetE (r1, r2) when (not (r1 = mtcc)) && not (r2 = mtcc) -> (
            match r2 @! conf with
            | Sealable (SealRange (_, _, _, e, _)) -> !>(upd_reg r1 (I e) conf)
            | Sealable (Cap (_, _, _, e, _)) -> !>(upd_reg r1 (I e) conf)
            | _ -> fail_state)
        | GetA (r1, r2) when (not (r1 = mtcc)) && not (r2 = mtcc) -> (
            match r2 @! conf with
            | Sealable (SealRange (_, _, _, _, a)) | Sealable (Cap (_, _, _, _, a)) ->
                !>(upd_reg r1 (I a) conf)
            | _ -> fail_state)
        | GetP (r1, r2) when (not (r1 = mtcc)) && not (r2 = mtcc) -> (
            match r2 @! conf with
            | Sealable (Cap (p, _, _, _, _)) -> !>(upd_reg r1 (I (Encode.encode_perm p)) conf)
            | Sealable (SealRange (p, _, _, _, _)) ->
                !>(upd_reg r1 (I (Encode.encode_seal_perm p)) conf)
            | _ -> fail_state)
        | GetOType (r1, r2) when (not (r1 = mtcc)) && not (r2 = mtcc) -> (
            match r2 @! conf with
            | Sealed (o, _) -> !>(upd_reg r1 (I o) conf)
            | _ -> !>(upd_reg r1 (I ~$(-1)) conf))
        | GetWType (r1, r2) when (not (r1 = mtcc)) && not (r2 = mtcc) ->
            let wtype_enc = Encode.encode_wtype (get_wtype (r2 @! conf)) in
            !>(upd_reg r1 (I wtype_enc) conf)
        | Seal (dst, r1, r2) when (not (dst = mtcc)) && (not (r1 = mtcc)) && not (r2 = mtcc) -> (
            match (r1 @! conf, r2 @! conf) with
            | Sealable (SealRange ((true, _), _, b, e, a)), Sealable sb when b <= a && a < e ->
                !>(upd_reg dst (Sealed (a, sb)) conf)
            | _ -> fail_state)
        | UnSeal (dst, r1, r2) when (not (dst = mtcc)) && (not (r1 = mtcc)) && not (r2 = mtcc) -> (
            match (r1 @! conf, r2 @! conf) with
            | Sealable (SealRange ((_, true), _, b, e, a)), Sealed (a', sb) ->
                if b <= a && a < e && a = a' then !>(upd_reg dst (Sealable sb) conf) else fail_state
            | _ -> fail_state)
        | _ -> fail_state)
  else fail_state

let step (m : mchn) : mchn option =
  match m with Running, conf -> Some (exec_single conf) | (Failed | Halted), _ -> None

let rec step_n (m : mchn) n : mchn option =
  if n > 0 then match step m with Some m' -> step_n m' (n - 1) | None -> Some m else Some m

let rec run (m : mchn) : mchn = match step m with Some m' -> run m' | None -> m
