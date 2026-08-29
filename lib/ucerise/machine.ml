open Ast

module RegMap = Map.Make(struct type t = register let compare = compare end)
module MemMap = Map.Make(Z)
type status = Running | Halted | Failed
type t = { config: Runtime_config.t; status: status; registers: word RegMap.t; memory: word MemMap.t }
let diagnostic s = Error [Diagnostic.error s]
let ( let* ) = Result.bind

let eval config expression =
  match Assembly_frontend.Expression.evaluate_runtime config expression with
  | Ok z -> Ok z | Error e -> diagnostic e
let lower_permission = function
  | Permission_literal p -> Ok p
  | Permission_parameter n -> diagnostic (Printf.sprintf "Unexpanded permission parameter $%s." n)
let lower_locality = function
  | Locality l -> Ok l
  | Locality_parameter n -> diagnostic (Printf.sprintf "Unexpanded locality parameter $%s." n)
let lower_word config = function
  | I_term e -> Result.map (fun z -> I z) (eval config e)
  | Cap_term (p,l,b,e,a) ->
      let* p = lower_permission p in let* l = lower_locality l in
      let* b = eval config b in let* e = eval config e in
      Result.map (fun a -> Cap (Cap (p,l,b,e,a))) (eval config a)
let lower_register = function
  | Named r -> Ok r
  | Register_parameter n -> diagnostic (Printf.sprintf "Unexpanded register parameter $%s." n)
let lower_constant config = function
  | Expression e -> eval config e
  | Permission p -> Ok (Codec.encode_permission p)
  | Permission_locality (p,l) ->
      let* p = lower_permission p in Result.map (Codec.encode_permission_locality p) (lower_locality l)
  | Parameterized_permission_locality (n,_) ->
      diagnostic (Printf.sprintf "Unexpanded permission parameter $%s." n)
  | Locality_constant l -> Ok (Codec.encode_locality l)
  | Value_parameter n -> diagnostic (Printf.sprintf "Unexpanded value parameter $%s." n)
let lower_operand config = function
  | Register_term r -> Result.map (fun r -> Register r) (lower_register r)
  | Constant_term c -> Result.map (fun z -> Constant z) (lower_constant config c)
let lower_instruction config op =
  let r = lower_register and o = lower_operand config in
  let rr c a b = let* a = r a in Result.map (fun b -> c (a,b)) (r b) in
  let ro c a b = let* a = r a in Result.map (fun b -> c (a,b)) (o b) in
  let roo c a b d = let* a = r a in let* b = o b in Result.map (fun d -> c (a,b,d)) (o d) in
  let rro c a b d = let* a = r a in let* b = r b in Result.map (fun d -> c (a,b,d)) (o d) in
  match op with
  | Jmp_term a -> Result.map (fun a -> Jmp a) (r a)
  | Jnz_term (a,b) -> rr (fun (a,b) -> Jnz (a,b)) a b
  | Move_term (a,b) -> ro (fun (a,b) -> Move (a,b)) a b
  | Load_term (a,b) -> rr (fun (a,b) -> Load (a,b)) a b
  | Store_term (a,b) -> ro (fun (a,b) -> Store (a,b)) a b
  | Add_term (a,b,c) -> roo (fun (a,b,c) -> Add (a,b,c)) a b c
  | Sub_term (a,b,c) -> roo (fun (a,b,c) -> Sub (a,b,c)) a b c
  | Lt_term (a,b,c) -> roo (fun (a,b,c) -> Lt (a,b,c)) a b c
  | Lea_term (a,b) -> ro (fun (a,b) -> Lea (a,b)) a b
  | Restrict_term (a,b) -> ro (fun (a,b) -> Restrict (a,b)) a b
  | SubSeg_term (a,b,c) -> roo (fun (a,b,c) -> SubSeg (a,b,c)) a b c
  | IsPtr_term (a,b) -> rr (fun (a,b) -> IsPtr (a,b)) a b
  | GetP_term (a,b) -> rr (fun (a,b) -> GetP (a,b)) a b
  | GetL_term (a,b) -> rr (fun (a,b) -> GetL (a,b)) a b
  | GetB_term (a,b) -> rr (fun (a,b) -> GetB (a,b)) a b
  | GetE_term (a,b) -> rr (fun (a,b) -> GetE (a,b)) a b
  | GetA_term (a,b) -> rr (fun (a,b) -> GetA (a,b)) a b
  | Fail_term -> Ok Fail | Halt_term -> Ok Halt
  | LoadU_term (a,b,c) -> rro (fun (a,b,c) -> LoadU (a,b,c)) a b c
  | StoreU_term (a,b,c) -> roo (fun (a,b,c) -> StoreU (a,b,c)) a b c
  | PromoteU_term a -> Result.map (fun a -> PromoteU a) (r a)

let lower_program config program =
  let rec loop addr memory = function
    | [] -> Ok memory
    | statement::rest -> (
        match statement with
        | Word term -> let* w = lower_word config term in loop Z.(succ addr) (MemMap.add addr w memory) rest
        | Op term ->
            let* op = lower_instruction config term in
            match Codec.encode op with
            | Ok z -> loop Z.(succ addr) (MemMap.add addr (I z) memory) rest
            | Error e -> diagnostic (Instruction_codec.error_message e))
  in loop Z.zero MemMap.empty program

let init config program regfile =
  let stack = Runtime_config.stack_addr config and limit = Runtime_config.max_addr config in
  let registers = List.init 32 (fun n -> Reg n,I Z.zero) |> List.to_seq |> RegMap.of_seq in
  let registers = registers
    |> RegMap.add PC (Cap (Cap (RWX,Global,Z.zero,stack,Z.zero)))
    |> RegMap.add (Reg 31) (Cap (Cap (URWLX,Local,stack,limit,stack))) in
  let* registers =
    match regfile with None -> Ok registers | Some entries ->
      List.fold_left (fun acc (r,w) ->
        let* regs = acc in Result.map (fun w -> RegMap.add r w regs) (lower_word config w))
        (Ok registers) entries in
  Result.map (fun memory -> {config; status=Running; registers; memory}) (lower_program config program)

let read_register r s = RegMap.find r s.registers
let read_memory a s =
  match MemMap.find_opt a s.memory with
  | Some w -> Some w
  | None when Z.sign a >= 0 && Z.compare a (Runtime_config.max_addr s.config) < 0 -> Some (I Z.zero)
  | None -> None
let set_register r w s = {s with registers=RegMap.add r w s.registers}
let set_memory_raw a w s = {s with memory=MemMap.add a w s.memory}
let fail s = {s with status=Failed}
let pc_next s =
  match read_register PC s with
  | Cap (Cap (p,l,b,e,a)) -> set_register PC (Cap (Cap (p,l,b,e,Z.succ a))) s
  | _ -> fail s
let write_next r w s = pc_next (set_register r w s)
let value s = function Register r -> read_register r s | Constant z -> I z
let is_u = function URW|URWX|URWL|URWLX -> true | _ -> false
let is_wl = function RWL|RWLX|URWL|URWLX -> true | _ -> false
let can_read = function RO|RX|RW|RWX|RWL|RWLX -> true | _ -> false
let can_write = function RW|RWX|RWL|RWLX -> true | _ -> false
let executable = function RX|RWX|RWLX|URWX|URWLX -> true | _ -> false
let promote = function URW->RW | URWX->RWX | URWL->RWL | URWLX->RWLX | p->p
let permission_flows requested current =
  match requested with
  | O -> true
  | E -> (match current with E|RX|RWX|RWLX -> true | _ -> false)
  | RO -> (match current with RO|RX|RW|RWX|RWL|RWLX -> true | _ -> false)
  | RX -> (match current with RX|RWX|RWLX -> true | _ -> false)
  | RW -> (match current with RW|RWX|RWL|RWLX -> true | _ -> false)
  | RWX -> (match current with RWX|RWLX -> true | _ -> false)
  | RWL -> (match current with RWL|RWLX -> true | _ -> false)
  | RWLX -> current=RWLX
  | URW -> (match current with URW|URWL|URWX|URWLX|RW|RWX|RWL|RWLX -> true | _ -> false)
  | URWL -> (match current with URWL|URWLX|RWL|RWLX -> true | _ -> false)
  | URWX -> (match current with URWX|URWLX|RWX|RWLX -> true | _ -> false)
  | URWLX -> (match current with URWLX|RWLX -> true | _ -> false)
let locality_flows requested current =
  match requested,current with Local,Local | Local,Global | Global,Global -> true | _ -> false
let valid_pc s =
  match read_register PC s with
  | Cap (Cap (p,_,b,e,a)) when executable p -> b<=a && a<e && Option.is_some (read_memory a s)
  | _ -> false

let rec execute op s =
  let get r = read_register r s and v o = value s o in
  match op with
  | Fail -> fail s | Halt -> {s with status=Halted}
  | Move (r,o) -> write_next r (v o) s
  | Load (d,r) -> (
      match get r with Cap (Cap (p,_,b,e,a)) when can_read p && b<=a && a<e ->
        (match read_memory a s with Some w -> write_next d w s | None -> fail s)
      | _ -> fail s)
  | Store (r,o) -> (
      match get r with Cap (Cap (p,_,b,e,a)) when can_write p && b<=a && a<e ->
        let w = v o in
        (match w with Cap (Cap (_,Local,_,_,_)) when not (is_wl p) -> fail s
        | _ -> pc_next (set_memory_raw a w s))
      | _ -> fail s)
  | Jmp r -> (
      match get r with Cap (Cap (E,l,b,e,a)) -> set_register PC (Cap (Cap (RX,l,b,e,a))) s
      | w -> set_register PC w s)
  | Jnz (r,t) -> (match get t with I z when Z.equal z Z.zero -> pc_next s | _ -> execute (Jmp r) s)
  | Add (r,a,b) | Sub (r,a,b) | Lt (r,a,b) -> (
      match v a,v b with I x,I y ->
        let z = match op with Add _ -> Z.add x y | Sub _ -> Z.sub x y
          | _ -> if Z.lt x y then Z.one else Z.zero in
        write_next r (I z) s
      | _ -> fail s)
  | Lea (r,o) -> (
      match get r,v o with Cap (Cap (E,_,_,_,_)),_ -> fail s
      | Cap (Cap (p,l,b,e,a)),I z -> write_next r (Cap (Cap (p,l,b,e,Z.add a z))) s
      | _ -> fail s)
  | Restrict (r,o) -> (
      match get r,v o with
      | Cap (Cap (p,l,b,e,a)),I z -> (
          match Codec.decode_permission_locality z with
          | Ok (p',l') when permission_flows p' p && locality_flows l' l ->
              write_next r (Cap (Cap (p',l',b,e,a))) s
          | _ -> fail s)
      | _ -> fail s)
  | SubSeg (r,o1,o2) -> (
      match get r,v o1,v o2 with
      | Cap (Cap (E,_,_,_,_)),_,_ -> fail s
      | Cap (Cap (p,l,b,e,a)),I b',I e'
        when b<=b' && Z.sign e'>=0 && Z.sign e>=0 ->
          write_next r (Cap (Cap (p,l,b',e',a))) s
      | _ -> fail s)
  | IsPtr (r,x) -> write_next r (I (match get x with Cap _ -> Z.one | I _ -> Z.zero)) s
  | GetP (r,x) -> (match get x with Cap (Cap (p,_,_,_,_)) ->
      write_next r (I (Codec.encode_permission p)) s | _ -> fail s)
  | GetL (r,x) -> (match get x with Cap (Cap (_,l,_,_,_)) ->
      write_next r (I (Codec.encode_locality l)) s | _ -> fail s)
  | GetB (r,x) -> (match get x with Cap (Cap (_,_,b,_,_)) -> write_next r (I b) s | _ -> fail s)
  | GetE (r,x) -> (match get x with Cap (Cap (_,_,_,e,_)) -> write_next r (I e) s | _ -> fail s)
  | GetA (r,x) -> (match get x with Cap (Cap (_,_,_,_,a)) -> write_next r (I a) s | _ -> fail s)
  | LoadU (d,r,o) -> (
      match get r,v o with
      | Cap (Cap (p,_,b,e,a)),I off when is_u p && b<=Z.add a off && Z.add a off<a && a<=e ->
          (match read_memory (Z.add a off) s with Some w -> write_next d w s | None -> fail s)
      | _ -> fail s)
  | StoreU (r,o,w) -> (
      match get r,v o with
      | Cap (Cap (p,l,b,e,a)),I off when is_u p && b<=Z.add a off && Z.add a off<=a && a<=e ->
          let w = v w in
          (match w with Cap (Cap (_,Local,_,_,_)) when not (is_wl p) -> fail s
          | _ ->
              let s = if Z.equal off Z.zero then
                set_register r (Cap (Cap (p,l,b,e,Z.succ a))) s else s in
              pc_next (set_memory_raw (Z.add a off) w s))
      | _ -> fail s)
  | PromoteU r -> (
      match get r with Cap (Cap (p,l,b,e,a)) when is_u p ->
        write_next r (Cap (Cap (promote p,l,b,Z.min e a,a))) s
      | _ -> fail s)

let step s =
  if s.status <> Running then Error (Machine_backend.Stopped
    (if s.status=Halted then Machine_view.Halted else Machine_view.Failed))
  else if not (valid_pc s) then Ok (fail s)
  else match read_register PC s with
    | Cap (Cap (_,_,_,_,a)) -> (
        match read_memory a s with
        | Some (I z) -> (match Codec.decode z with Ok op -> Ok (execute op s) | Error _ -> Ok (fail s))
        | _ -> Ok (fail s))
    | _ -> Ok (fail s)
let rec step_n n s =
  if n<0 then Error (Machine_backend.Backend_error "step count must be non-negative")
  else if n=0 then Ok s
  else match step s with
    | Ok next -> step_n (n-1) next
    | Error (Machine_backend.Stopped _) -> Ok s
    | Error _ as e -> e
