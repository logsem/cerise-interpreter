open Ast
let (~$) = Z.(~$)


exception NotYetImplemented

module MemMap = Map.Make(Z)
module RegMap =
  Map.Make(struct
    type t = regname
    let compare = compare_regname
  end)
    
type exec_state = Running | Halted | Failed
(* type word = I of Z.t | Cap of perm * Z.t * Z.t * Z.t *)
type word = I of Z.t | Sealable of sealable | Sealed of Z.t * sealable
type reg_state = word RegMap.t
type mem_state = word MemMap.t
type exec_conf = { reg : reg_state; mem : mem_state } (* using a record to have notation similar to the paper *)
type mchn = exec_state * exec_conf

let init_reg_state (addr_max : Z.t) : reg_state =
  let start_heap_addr = ~$0 in
  let max_heap_addr = Z.(addr_max / ~$2) in

  let l = List.init 31 (fun i -> Reg (i+1), I Z.zero) in
  (* let l = List.init 32 (fun i -> Reg i, I Z.zero) in *)

  (* The PC register starts with full permission over the entire "heap" segment *)
  let pc_init = (PC, Sealable (Cap (RWX, start_heap_addr, max_heap_addr, start_heap_addr))) in
  let pc_sealing = (Reg 0, Sealable (SealRange ((true,true), start_heap_addr, max_heap_addr, start_heap_addr))) in
  let seq = List.to_seq (pc_init :: pc_sealing :: l) in
  RegMap.of_seq seq

let get_reg (r : regname) ({reg ; _} : exec_conf) : word = RegMap.find r reg
let (@!) x y = get_reg x y
  
let upd_reg (r : regname) (w : word) ({reg ; mem} : exec_conf) : exec_conf =
  {reg = RegMap.add r w reg ; mem}

let init_mem_state (addr_start: Z.t) (addr_max : Z.t) (prog : t) : mem_state =
  let zeroed_mem =
    (* NB: addr_max is not addressable *)
    let rec loop (i : Z.t) m =
      if i >= addr_max then m else loop Z.(i+ ~$1) (MemMap.add i (I ~$0) m) in
    loop Z.zero MemMap.empty
  in
  let enc_prog =
    List.to_seq @@ List.mapi
      (fun i x ->
        let i = ~$i in
        Z.(i+addr_start),
                  match x with
                  | Op op -> I (Encode.encode_machine_op op)
                  | Word (Ast.I z) -> I z
                  | Word (Ast.Sealable sb) -> Sealable sb
                    (* Cap (p, b, e, a)) *)
                  | Word (Ast.Sealed (o, sb)) -> Sealed (o, sb))
      prog in
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
  | CP (Const i) -> I i
  | CP (Perm p) -> I (Encode.encode_perm p) (* A permission is just an integer in the model *)

let upd_pc (conf : exec_conf) : mchn =
  match PC @! conf with
  | Sealable (Cap (p, b, e, a)) -> (Running, upd_reg PC (Sealable (Cap (p, b, e, Z.(a + ~$1)))) conf)
  | _ -> (Failed, conf)
let (!>) conf = upd_pc conf

let upd_pc_perm (w : word) =
  match w with
  | Sealable (Cap (E, b, e, a)) -> Sealable (Cap (RX, b, e, a))
  | _ -> w

let fetch_decode (conf : exec_conf) : machine_op option =
  match PC @! conf with
  | Sealable (Cap (_, _, _, addr)) ->
    (match get_mem addr conf with
    | Some (I enc) ->
      (try Some (Encode.decode_machine_op enc)
        with Encode.DecodeException _ -> None)
    | _ -> None)
  | _ -> None

let is_pc_valid (conf : exec_conf) : bool =
  match PC @! conf with
  | Sealable (Cap ((RX|RWX), b, e, a)) -> begin
      if b <= a && a < e
      then Option.is_some @@ a @? conf
      else false
    end
  | _ -> false

let perm_flowsto (p1 : perm) (p2 : perm) : bool =
  match p1 with
  | O -> true
  | E ->
    (match p2 with
    | E | RX | RWX -> true
    | _ -> false)
  | RX ->
    (match p2 with
    | RX | RWX -> true
    | _ -> false)
  | RWX ->
    (match p2 with
    | RWX -> true
    | _ -> false)
  | RO ->
    (match p2 with
    | E | O -> false
    | _ -> true)
  | RW ->
    (match p2 with
    | RW | RWX -> true
    | _ -> false)

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
  | RW | RWX -> true
  | _ -> false

let can_read (p : perm) : bool =
  match p with
  | RO| RX| RW| RWX -> true
  | _ -> false

let exec_single (conf : exec_conf) : mchn =
  let fail_state = (Failed, conf) in
  if is_pc_valid conf 
  then match fetch_decode conf with
    | None -> fail_state
    | Some instr -> begin
        match instr with
        | Fail -> (Failed, conf)
        | Halt -> (Halted, conf)
        | Move (r, c) -> begin
            let w = get_word conf c in
            !> (upd_reg r w conf)
          end
        | Load (r1, r2) -> begin
            match r2 @! conf with
            | Sealable (Cap (p, b, e, a)) ->
              if can_read p then
                match a @? conf with
                | Some w when (b <= a && a < e) -> !> (upd_reg r1 w conf)
                | _ -> fail_state
              else fail_state
            | _ -> fail_state
          end
        | Store (r, c) -> begin
            let w = get_word conf c in
            match r @! conf with
            | Sealable (Cap (p, b, e, a)) when (b <= a && a < e) ->
              if can_write p
              then !> (upd_mem a w conf)
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
            | Sealable (Cap (p, b, e, a)) -> begin
                match get_word conf c with
                | I i -> begin
                    let p' = Encode.decode_perm i in
                    if perm_flowsto p' p
                    then !> (upd_reg r (Sealable (Cap (p', b, e, a))) conf)
                    else fail_state
                  end
                | _ -> fail_state
              end
            | Sealable (SealRange (sp, b, e, a)) -> begin
                match get_word conf c with
                | I i -> begin
                    let sp' = Encode.decode_sealperm i in
                    if sealperm_flowsto sp' sp
                    then !> (upd_reg r (Sealable (SealRange (sp', b, e, a))) conf)
                    else fail_state
                  end
                | _ -> fail_state
              end


            | _ -> fail_state
          end
        | SubSeg (r, c1, c2) -> begin
            match r @! conf with
            | Sealable (Cap (p, b, e, a)) -> begin
                let w1 = get_word conf c1 in
                let w2 = get_word conf c2 in
                match w1, w2 with
                | I z1, I z2 ->
                  if b <= z1 && Z.(~$0 <= z2) && Z.(~$0 <= e) && p <> E
                  then
                    let w = Sealable (Cap (p, z1, z2, a)) in
                    !> (upd_reg r w conf)
                  else fail_state
                | _ -> fail_state
              end
            | Sealable (SealRange (p, b, e, a)) -> begin
                let w1 = get_word conf c1 in
                let w2 = get_word conf c2 in
                match w1, w2 with
                | I z1, I z2 ->
                  if b <= z1 && Z.(~$0 <= z2) && Z.(~$0 <= e)
                  then
                    let w = Sealable (SealRange (p, z1, z2, a)) in
                    !> (upd_reg r w conf)
                  else fail_state
                | _ -> fail_state
              end
            | _ -> fail_state
          end
        | Lea (r, c) -> begin
            match r @! conf with
            | Sealable (Cap (p, b, e, a)) -> begin
                let w = get_word conf c in
                match w with
                | I z when p <> E -> !> (upd_reg r (Sealable (Cap (p, b, e, Z.(a + z)))) conf)
                | _ -> fail_state
              end
            | Sealable (SealRange (p, b, e, a)) -> begin
                let w = get_word conf c in
                match w with
                | I z -> !> (upd_reg r (Sealable (SealRange (p, b, e, Z.(a + z)))) conf)
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
        | GetB (r1, r2) -> begin
            match r2 @! conf with
            | Sealable (SealRange (_, b, _, _))
            | Sealable (Cap (_, b, _, _)) -> !> (upd_reg r1 (I b) conf)
            | _ -> fail_state
          end
        | GetE (r1, r2) -> begin
            match r2 @! conf with
            | Sealable (SealRange (_, _, e, _))
            | Sealable (Cap (_, _, e, _)) -> !> (upd_reg r1 (I e) conf)
            | _ -> fail_state
          end
        | GetA (r1, r2) -> begin
            match r2 @! conf with
            | Sealable (SealRange (_, _, _, a))
            | Sealable (Cap (_, _, _, a)) -> !> (upd_reg r1 (I a) conf)
            | _ -> fail_state
          end
        | GetP (r1, r2) -> begin
            match r2 @! conf with
            | Sealable (Cap (p, _, _, _)) -> !> (upd_reg r1 (I (Encode.encode_perm p)) conf)
            | Sealable (SealRange (p, _, _, _)) -> !> (upd_reg r1 (I (Encode.encode_sealperm p)) conf)
            | _ -> fail_state
          end
        | GetOType (r1, r2) -> begin
            match r2 @! conf with
            | Sealed (o,_) -> !> (upd_reg r1 (I o) conf)
            | _ -> !> (upd_reg r1 (I ~$(-1)) conf)
          end
        | GetWType (r1, r2) -> begin
            let wtype_enc =
              Encode.encode_wtype
                (match r2 @! conf with
                 | I _ -> W_I
                 | Sealable (Cap _) -> W_Cap
                 | Sealable (SealRange _) -> W_SealRange
                 | Sealed _ -> W_Sealed)
            in !> (upd_reg r1 (I wtype_enc) conf)
          end

        | Seal (dst, r1, r2) -> begin
            match r1 @! conf, r2 @! conf with
            | Sealable (SealRange ((true,_),b,e,a)), Sealable sb when (b <= a && a < e) ->
              !> (upd_reg dst (Sealed (a, sb)) conf)
            | _ -> fail_state
          end
        | UnSeal (dst, r1, r2) -> begin
            match r1 @! conf, r2 @! conf with
            | Sealable (SealRange ((_,true),b,e,a)), (Sealed (a', sb))->
              if (b <= a && a < e && a = a')
              then !> (upd_reg dst (Sealable sb) conf)
              else fail_state
            | _ -> fail_state
          end
      end
  else fail_state

let step (m: mchn): mchn option =
  match m with
  | Running, conf -> Some (exec_single conf)
  | (Failed | Halted), _ -> None

let rec step_n (m: mchn) n : mchn option =
  if n > 0 then
  (match (step m) with
  | Some m' -> step_n m' (n-1)
  | None -> None)
  else Some m

let rec run (m : mchn) : mchn =
  match step m with
  | Some m' -> run m'
  | None -> m
