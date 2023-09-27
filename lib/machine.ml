open Ast
open Parameters
let (~$) = Z.(~$)

exception NotYetImplemented
module MemMap = Map.Make(Z)
module RegMap =
  Map.Make(struct
    type t = regname
    let compare = compare_regname
  end)
    
type exec_state = Running | Halted | Failed
type reg_state = word RegMap.t
type mem_state = word MemMap.t
type exec_conf = { reg : reg_state; mem : mem_state } (* using a record to have notation similar to the paper *)
type mchn = exec_state * exec_conf

let init_reg_state (stk_addr : Z.t) : reg_state =
  let start_heap_addr = ~$0 in
  let max_heap_addr =
    if !flags.stack
    then Infinite_z.Int stk_addr
    else !Parameters.flags.max_addr
  in
  let max_stk_addr = !Parameters.flags.max_addr in

  let l =
    let seal_reg = (if !flags.sealing then 1 else 0) in
    let n =
      32 - seal_reg
    in
    List.init n (fun i -> Reg (i+seal_reg), I Z.zero)
  in

  (* The PC register starts with full permission over the entire "heap" segment *)
  let pc_init = [(PC, Sealable (Cap (RWX, Global, start_heap_addr, max_heap_addr, start_heap_addr)))] in

  let stk_init =
    if !flags.stack
    then
      let stk_locality = !flags.locality in
      let stk_perm = if !flags.unitialized then URWLX else RWLX in
      [(stk, Sealable (Cap (stk_perm, stk_locality, stk_addr, max_stk_addr, stk_addr)))]
    else []
  in

  let pc_sealing =
    if !flags.sealing
    then [(Reg 0, Sealable (SealRange ((true,true), Global, start_heap_addr, stk_addr, start_heap_addr)))]
    else []
  in
  (* The stk register starts with full permission over the entire "stack" segment *)
  let seq = List.to_seq (pc_init @ stk_init @ pc_sealing @ l) in
  RegMap.of_seq seq

let get_reg (r : regname) ({reg ; _} : exec_conf) : word = RegMap.find r reg
let (@!) x y = get_reg x y
  
let upd_reg (r : regname) (w : word) ({reg ; mem} : exec_conf) : exec_conf =
  {reg = RegMap.add r w reg ; mem}

let init_mem_state (addr_start: Z.t) (prog : t) : mem_state =
  let zeroed_mem =
    let rec loop (i : Z.t) m =
      let addr_max = Parameters.get_max_addr () in
      (* NB: addr_max is not addressable *)
      if i >= addr_max then m else loop Z.(i+ ~$1) (MemMap.add i (I ~$0) m) in
    loop Z.zero MemMap.empty
  in
  let enc_prog =
    List.to_seq @@ List.mapi
      (fun i x ->
         check_statement x;
         let i = ~$i in
         Z.(i+addr_start),
         match x with
         | Op op -> I (Encode.encode_machine_op op)
         | Word (Ast.I z) -> I z
         | Word (Ast.Sealable (Ast.Cap (p, l, b, e, a))) ->
           Sealable (Cap (p,l,b,e,a))
         | Word (Ast.Sealable (Ast.SealRange (p, l, b, e, a))) ->
           Sealable (SealRange (p,l,b,e,a))
         | Word (Ast.Sealed (o, sb)) ->
           Sealed (o, sb)) prog
  in
  MemMap.add_seq enc_prog zeroed_mem

let get_mem (addr : Z.t) (conf : exec_conf) : word option = MemMap.find_opt addr conf.mem
let (@?) x y = get_mem x y

let upd_mem (addr : Z.t) (w : word) ({reg ; mem} : exec_conf) : exec_conf = {reg ; mem = MemMap.add addr w mem}

let init
    (initial_regs : word RegMap.t)
    (initial_mems : word MemMap.t) =
  (Running, {reg = initial_regs; mem = initial_mems})

let get_word (conf : exec_conf) (roc : reg_or_const) : word =
  match roc with
  | Register r -> get_reg r conf
  | Const i -> I i

let upd_pc (conf : exec_conf) : mchn =
  match PC @! conf with
  | Sealable (Cap (p, g, b, e, a)) ->
    (Running, upd_reg PC (Sealable (Cap (p, g, b, e, Z.(a + ~$1)))) conf)
  | _ -> (Failed, conf)
let (!>) conf = upd_pc conf

let upd_pc_perm (w : word) =
  match w with
  | Sealable (Cap (E,g, b, e, a)) -> Sealable (Cap (RX, g, b, e, a))
  | _ -> w

let fetch_decode (conf : exec_conf) : machine_op option =
  match PC @! conf with
  | Sealable (Cap (_, _, _, _, addr)) ->
    (match get_mem addr conf with
     | Some (I enc) ->
       (try Some (Encode.decode_machine_op enc)
        with Encode.DecodeException _ -> None)
     | _ -> None)
  | _ -> None

let is_pc_valid (conf : exec_conf) : bool =
  match PC @! conf with
  | Sealable (Cap ((RX|RWX|RWLX), _, b, e, a)) -> begin
      if b <= a && (Infinite_z.z_lt a e)
      then Option.is_some @@ a @? conf
      else false
    end
  | _ -> false

let perm_flowsto (p1 : perm) (p2 : perm) : bool =
  match p1 with
  | O -> true
  | E ->
    (match p2 with
    | E | RX | RWX | RWLX -> true
    | _ -> false)
  | RX ->
    (match p2 with
    | RX | RWX | RWLX -> true
    | _ -> false)
  | RWX ->
    (match p2 with
    | RWX | RWLX -> true
    | _ -> false)
  | RWLX ->
    (match p2 with
    | RWLX -> true
    | _ -> false)
  | RO ->
    (match p2 with
    | E | O | URW | URWL | URWX | URWLX -> false
    | _ -> true)
  | RW ->
    (match p2 with
    | RW | RWX | RWL | RWLX -> true
    | _ -> false)
  | RWL ->
    (match p2 with
     | RWL | RWLX -> true
     | _ -> false)
  | URW ->
    (match p2 with
     | URW | URWL | URWX | URWLX | RW | RWX | RWL | RWLX -> true
     | _ -> false)
  | URWL ->
    (match p2 with
     | URWL | RWL | RWLX | URWLX -> true
     | _ -> false)
  | URWX ->
    (match p2 with
     | URWX | RWX | RWLX | URWLX -> true
     | _ -> false)
  | URWLX ->
    (match p2 with
     | URWLX | RWLX -> true
     | _ -> false)

let locality_flowsto (g1 : locality) (g2 : locality) : bool =
  match g1 with
  | Directed -> true
  | Local ->
    (match g2 with
     | Directed -> false
     | _ -> true)
  | Global ->
    (match g2 with
     | Global -> true
     | _ -> false)

let promote_uperm (p : perm) : perm =
  match p with
  | URW -> RW
  | URWL -> RWL
  | URWX -> RWX
  | URWLX -> RWLX
  | _ -> p

let is_uperm (p : perm) : bool =
  match p with
  | URW | URWL | URWX | URWLX -> true
  | _ -> false

let is_WLperm (p : perm) : bool =
  match p with
  | RWL | RWLX | URWL | URWLX -> true
  | _ -> false

let is_exec (p : perm) : bool =
  match p with
  | RX | RWX | RWLX | URWX | URWLX -> true
  | _ -> false

let sealperm_flowsto (p1 : seal_perm) (p2 : seal_perm) : bool =
  let p_flows p p' =
    match p,p' with
    | false, _ -> true
    | true, true -> true
    | _,_ -> false
  in
  let (s1, u1) = p1 in
  let (s2, u2) = p2 in
  (p_flows s1 s2) && (p_flows u1 u2)

let can_write (p : perm) : bool =
  match p with
  | RW | RWX | RWL | RWLX -> true
  | _ -> false

let can_read (p : perm) : bool =
  match p with
  | RO|RX|RW|RWX|RWL|RWLX-> true
  | _ -> false

let can_read_upto (w : word) : Infinite_z.t =
  match w with
  | Sealable (Cap (p,_,_,e,a)) -> if is_uperm p then Infinite_z.z_min a e else e
  | _ -> Int Z.zero

let get_wtype (w : word) : wtype =
  (match w with
   | I _ -> W_I
   | Sealable (Cap _) -> W_Cap
   | Sealable (SealRange _) -> W_SealRange
   | Sealed (_, _) -> W_Sealed)

let get_locality_sealable (s : sealable) =
  match s with | Cap (_, l, _, _, _) | SealRange (_, l, _, _, _) -> l

let is_sealrange (sb : sealable) =
 match sb with | SealRange _ -> true | _ -> false
let is_cap (sb : sealable) =
 match sb with | Cap _ -> true | _ -> false

(* NOTE Although we've already check that not supported instructions / capabilities *)
(*  are not in the initial machine, we still need to make sure that *)
(*  the user does not encode not supported instructions *)
let exec_single (conf : exec_conf) : mchn =
  let fail_state = (Failed, conf) in
  if is_pc_valid conf
  then match fetch_decode conf with
    | None -> fail_state
    | Some instr -> begin
        check_machine_op instr ;
        match instr with
        | Fail -> (Failed, conf)
        | Halt -> (Halted, conf)
        | Move (r, c) -> begin
            let w = get_word conf c in
            !> (upd_reg r w conf)
          end
        | Load (r1, r2) -> begin
            match r2 @! conf with
            | Sealable (Cap (p, _, b, e, a)) ->
              if can_read p then
                match a @? conf with
                | Some w when (b <= a && Infinite_z.z_lt a e) -> !> (upd_reg r1 w conf)
                 (* case where we actually load a word outside of the already *)
                 (* mapped memory, in case of infinite memory*)
                | None   when (b <= a
                               && Infinite_z.z_lt a e
                               && !Parameters.flags.max_addr = Inf)
                  -> !> (upd_reg r1 (I Z.zero) conf)
                | _ -> fail_state
              else fail_state
            | _ -> fail_state
          end
        | Store (r, c) -> begin
            let w = get_word conf c in
            match r @! conf with
            | Sealable (Cap (p, _, b, e, a)) when (b <= a && Infinite_z.z_lt a e) ->
              if can_write p then
                (match w with
                 (* We consider that a Directed sealing capability is similar to Local *)
                 | Sealed ( _, sb) | Sealable sb
                   when (get_locality_sealable sb = Local) || (get_locality_sealable sb = Directed && is_sealrange sb) ->
                   if is_WLperm p
                   then !> (upd_mem a w conf)
                   else fail_state
                 | Sealed ( _, sb) | Sealable sb
                   when (get_locality_sealable sb = Directed && is_cap sb) ->
                   if (is_WLperm p && Infinite_z.leq_z (can_read_upto w) a)
                   then !> (upd_mem a w conf)
                   else fail_state
                 | _ -> !> (upd_mem a w conf))
              else fail_state
            | _ -> fail_state
          end
        | Jmp r -> begin
            let new_pc = upd_pc_perm (r @! conf) in
            (Running, upd_reg PC new_pc conf)
          end
        | Jnz (r1, r2) -> begin
            match r2 @! conf with
            | I i when Z.(equal i zero) -> !> conf
            | _ -> begin
                let new_pc = upd_pc_perm (r1 @! conf) in
                (Running, upd_reg PC new_pc conf)
              end
          end
        | Restrict (r, c) -> begin
            match r @! conf with
            | Sealable (Cap (p, g, b, e, a)) -> begin
                match get_word conf c with
                | I i -> begin
                    if (!flags.locality = Global)
                       then
                         (* Locality is Global, so we only check the permission *)
                         let decode_p' =
                           try Some (Encode.decode_perm i) with | Encode.DecodeException _ -> None
                         in
                         match decode_p' with
                         | None -> fail_state
                         | Some p' ->
                           check_perm p; check_perm p';
                           if (perm_flowsto p' p)
                           then !> (upd_reg r (Sealable (Cap (p', Global, b, e, a))) conf)
                           else fail_state
                       else
                         let decode_p' =
                           try Some (Encode.decode_perm_loc_pair i) with | Encode.DecodeException _ -> None
                         in
                         match decode_p' with
                         | None -> fail_state
                         | Some (p', g') ->
                           check_perm p; check_perm p'; check_locality g; check_locality g';
                           if (perm_flowsto p' p) && (locality_flowsto g' g)
                           then !> (upd_reg r (Sealable (Cap (p', g', b, e, a))) conf)
                           else fail_state
                     end
                | _ -> fail_state
              end
            | Sealable (SealRange (sp, g, b, e, a)) -> begin
                match get_word conf c with
                | I i -> begin
                    (* Locality is Global, so we only check the permission *)
                    if (!flags.locality = Global)
                    then
                      let decode_sp' =
                        try Some (Encode.decode_seal_perm i) with | Encode.DecodeException _ -> None
                      in
                      match decode_sp' with
                      | None -> fail_state
                      | Some sp' ->
                        check_seal_perm sp ; check_seal_perm sp' ;
                        if (sealperm_flowsto sp' sp)
                        then !> (upd_reg r (Sealable (SealRange (sp', Global, b, e, a))) conf)
                        else fail_state
                    else
                      let decode_sp' =
                        try Some (Encode.decode_seal_perm_loc_pair i) with | Encode.DecodeException _ -> None
                      in
                      match decode_sp' with
                      | None -> fail_state
                      | Some (sp', g') ->
                        check_seal_perm sp ; check_seal_perm sp' ; check_locality g ; check_locality g' ;
                        if (sealperm_flowsto sp' sp) && (locality_flowsto g' g)
                        then !> (upd_reg r (Sealable (SealRange (sp', g', b, e, a))) conf)
                        else fail_state
                  end
                | _ -> fail_state
              end
            | _ -> fail_state
          end
        | SubSeg (r, c1, c2) -> begin
            match r @! conf with
            | Sealable (Cap (p, g, b, e, a)) -> begin
                let w1 = get_word conf c1 in
                let w2 = get_word conf c2 in
                match w1, w2 with
                | I z1, I z2 ->
                  if b <= z1 && Z.(~$0 <= z2) && (Infinite_z.z_leq Z.zero e) && p <> E
                  then
                    let w = Sealable (Cap (p, g, z1, Int z2, a)) in
                    !> (upd_reg r w conf)
                    (* Special case: SubSeg (p,g,b,+∞,a) z1 (-1) , ie. the upper-bound is still infinity *)
                  else if (b <= z1 && Z.(z2 == ~$(-1)) && (Infinite_z.eq Inf e) && p <> E)
                  then let w = Sealable (Cap (p, g, z1, Inf, a)) in
                    !> (upd_reg r w conf)
                  else fail_state
                | _ -> fail_state
              end
            | Sealable (SealRange (p, g, b, e, a)) -> begin
                let w1 = get_word conf c1 in
                let w2 = get_word conf c2 in
                match w1, w2 with
                | I z1, I z2 ->
                  if b <= z1 && Z.(~$0 <= z2) && Z.(~$0 <= e)
                  then
                    let w = Sealable (SealRange (p, g, z1, z2, a)) in
                    !> (upd_reg r w conf)
                  else fail_state
                | _ -> fail_state
              end
            | _ -> fail_state
          end
        | Lea (r, c) -> begin
            match r @! conf with
            | Sealable (Cap (p, g, b, e, a)) -> begin
                let w = get_word conf c in
                match w with
                | I z when p <> E -> !> (upd_reg r (Sealable (Cap (p, g, b, e, Z.(a + z)))) conf)
                | _ -> fail_state
              end
            | Sealable (SealRange (p, g, b, e, a)) -> begin
                let w = get_word conf c in
                match w with
                | I z -> !> (upd_reg r (Sealable (SealRange (p, g, b, e, Z.(a + z)))) conf)
                | _ -> fail_state
              end
            | _ -> fail_state
          end
        | Add (r, c1, c2) -> begin
            let w1 = get_word conf c1 in
            let w2 = get_word conf c2 in
            match w1, w2 with
            | I z1, I z2 -> !> (upd_reg r (I Z.(z1 + z2)) conf)
            | _ -> fail_state
          end
        | Sub (r, c1, c2) -> begin
            let w1 = get_word conf c1 in
            let w2 = get_word conf c2 in
            match w1, w2 with
            | I z1, I z2 -> !> (upd_reg r (I Z.(z1 - z2)) conf)
            | _ -> fail_state
          end
        | Mul (r, c1, c2) -> begin
            let w1 = get_word conf c1 in
            let w2 = get_word conf c2 in
            match w1, w2 with
            | I z1, I z2 -> !> (upd_reg r (I Z.(z1 * z2)) conf)
            | _ -> fail_state
          end
        | Rem (r, c1, c2) -> begin
            let w1 = get_word conf c1 in
            let w2 = get_word conf c2 in
            match w1, w2 with
            | I z1, I z2 when z2 != Z.zero -> !> (upd_reg r (I Z.(z1 mod z2)) conf)
            | _ -> fail_state
          end
        | Div (r, c1, c2) -> begin
            let w1 = get_word conf c1 in
            let w2 = get_word conf c2 in
            match w1, w2 with
            | I z1, I z2 when z2 != Z.zero -> !> (upd_reg r (I Z.(z1 / z2)) conf)
            | _ -> fail_state
          end

        | Lt (r, c1, c2) -> begin
            let w1 = get_word conf c1 in
            let w2 = get_word conf c2 in
            match w1, w2 with
            | I z1, I z2 when Z.(lt z1 z2) -> !> (upd_reg r (I Z.one) conf)
            | I _, I _ -> !> (upd_reg r (I Z.zero) conf)
            | _ -> fail_state
          end
        | GetL (r1, r2) ->
          begin
            match r2 @! conf with
            | Sealable sb
            | Sealed (_, sb) ->
              let g = get_locality_sealable sb in
              !> (upd_reg r1 (I (Encode.encode_locality g)) conf)
            | _ -> fail_state
          end
        | GetB (r1, r2) -> begin
            match r2 @! conf with
            | Sealable (SealRange (_, _, b, _, _))
            | Sealable (Cap (_, _, b, _, _)) -> !> (upd_reg r1 (I b) conf)
            | _ -> fail_state
          end
        | GetE (r1, r2) -> begin
            match r2 @! conf with
            | Sealable (SealRange (_, _, _, e, _)) -> !> (upd_reg r1 (I e) conf)
            | Sealable (Cap (_, _, _, e, _)) ->
              let e' : Z.t = match e with | Inf -> Z.minus_one | Int z -> z in
              !> (upd_reg r1 (I e') conf)
            | _ -> fail_state
          end
        | GetA (r1, r2) -> begin
            match r2 @! conf with
            | Sealable (SealRange (_, _, _, _, a))
            | Sealable (Cap (_, _, _, _, a)) -> !> (upd_reg r1 (I a) conf)
            | _ -> fail_state
          end
        | GetP (r1, r2) -> begin
            match r2 @! conf with
            | Sealable (Cap (p, _, _, _, _)) -> !> (upd_reg r1 (I (Encode.encode_perm p)) conf)
            | Sealable (SealRange (p, _, _, _, _)) -> !> (upd_reg r1 (I (Encode.encode_seal_perm p)) conf)
            | _ -> fail_state
          end
        | GetOType (r1, r2) ->
          begin
            match r2 @! conf with
            | Sealed (o,_) -> !> (upd_reg r1 (I o) conf)
            | _ -> !> (upd_reg r1 (I ~$(-1)) conf)
          end
        | GetWType (r1, r2) -> begin
            let wtype_enc = Encode.encode_wtype (get_wtype (r2 @! conf)) in
            !> (upd_reg r1 (I wtype_enc) conf)
          end

        | Seal (dst, r1, r2) -> begin
            match r1 @! conf, r2 @! conf with
            | Sealable (SealRange ((true,_),_,b,e,a)), Sealable sb when (b <= a && a < e) ->
              !> (upd_reg dst (Sealed (a, sb)) conf)
            | _ -> fail_state
          end
        | UnSeal (dst, r1, r2) -> begin
            match r1 @! conf, r2 @! conf with
            | Sealable (SealRange ((_,true),_,b,e,a)), (Sealed (a', sb))->
              if (b <= a && a < e && a = a')
              then !> (upd_reg dst (Sealable sb) conf)
              else fail_state
            | _ -> fail_state
          end
        | Invoke (r1, r2) -> begin (* r1 = code, r2 = data *)
            match r1 @! conf, r2 @! conf with
            | Sealed (o1, w1), Sealed (o2, w2) when o1 = o2 ->
              begin
                match w1, w2 with
                | Cap (p1, _, _ ,_ ,_ ), Cap (p2, _, _ ,_ ,_ )
                  when (is_exec p1) && (not (is_exec p2)) ->
                  let new_pc = upd_pc_perm (Sealable w1) in
                  (Running, upd_reg r2 (Sealable w2) (upd_reg PC new_pc conf))
                | _ ,_  -> fail_state
              end
            | _ -> fail_state
          end


        | LoadU (r1, r2, c) -> begin
            match r2 @! conf with
            | Sealable (Cap (p, _, b, e, a)) ->
              Z.(
                if is_uperm p then
                  (match (get_word conf c) with
                   | I off when
                       (b <= a + off) &&
                       (a + off < a) &&
                       (Infinite_z.z_leq a e)
                     -> (match (a+ off) @? conf with
                         | Some w -> !> (upd_reg r1 w conf)
                         | _ -> fail_state)
                   | _ -> fail_state)
                else fail_state)
            | _ -> fail_state
          end

        | StoreU (r, c1, c2) -> begin
            let woff = get_word conf c1 in
            let w = get_word conf c2 in
            match woff with
            | I off ->
              Z.(
                (match r @! conf with
                 | Sealable (Cap (p, g, b, e, a)) when
                     (b <= a + off) &&
                     (a + off <= a) &&
                     (Infinite_z.z_leq a e)
                   ->
                   if is_uperm p then
                     (match w with
                      | Sealable (Cap (_, g,_,_,_)) when g != Global && (not (is_WLperm p)) ->
                        fail_state
                      | Sealable (Cap (_, Directed,_,_,_)) when
                          (not (Infinite_z.leq_z (can_read_upto w) (a + off))) ->
                        fail_state
                      | _ ->
                        let conf' =
                          if off = ~$0
                          then (upd_reg r (Sealable (Cap (p, g, b, e, a + ~$1))) conf)
                          else conf (* if non zero, no increment *)
                        in !> (upd_mem (a+off) w conf'))
                   else fail_state
                 | _ -> fail_state))
            | _ -> fail_state
          end

        | PromoteU r ->
          match r @! conf with
          | Sealable (Cap (p,g,b,e,a)) ->
            (match p with
             | URW | URWL | URWX | URWLX ->
               let p' = promote_uperm p in
               let e' = Infinite_z.min_z e a in
               !> (upd_reg r (Sealable (Cap (p',g,b,e',a))) conf)
             | _ -> fail_state)
          | _ -> fail_state
      end
  else fail_state

let step (m: mchn): mchn option =
  match m with
  | Running, conf -> Some (exec_single conf )
  | (Failed | Halted), _ -> None

let rec step_n (m: mchn) n : mchn option =
  if n > 0
  then
    (match (step m) with
     | Some m' -> step_n m' (n-1)
     | None -> Some m)
  else Some m

let rec run (m : mchn) : mchn =
  match step m with
  | Some m' -> run m'
  | None -> m
