open Ast
type machineFlags =
  {
    version : string; (* Name of the version of Cerise *)
    sealing : bool; (* Are sealing/sealed capabilities supported ? *)
    stack : bool; (* Is there a stack register ? *)
    locality : Ast.locality; (* Minimum locality supported *)
    unitialized : bool; (* Are uninitialized capabilities supported ? *)
  }

let vanilla_cerise : machineFlags =
  {
    sealing = false;
    stack  = false;
    locality  = Global;
    unitialized  = false;
    version = "vanilla-cerise";
  }

let stack_cerise : machineFlags =
  {
    sealing = false;
    stack  = true;
    locality  = Local;
    unitialized  = true;
    version = "stack-cerise";
  }

let mcerise : machineFlags =
  {
    sealing = false;
    stack  = true;
    locality  = Directed;
    unitialized  = true;
    version = "mcerise";
  }

let sealing_cerise : machineFlags =
  {
    sealing = true;
    stack  = false;
    locality  = Global;
    unitialized  = false;
    version = "sealing-cerise";
  }

let full_cerise : machineFlags =
  {
    sealing = true;
    stack  = true;
    locality  = Directed;
    unitialized  = true;
    version = "cerise";
  }

let custom_cerise : machineFlags =
  {
    sealing = false;
    stack  = false;
    locality  = Global;
    unitialized  = false;
    version = "custom";
  }


let default : machineFlags = full_cerise
let flags : machineFlags ref = ref default

exception MalformedConfiguration of string
let config_check (f : machineFlags) =
  if (f.stack && (f.locality = Global))
  then raise @@ MalformedConfiguration "A configuration with the stack requires at least the Local locality."
  else () (* TODO is there other bad configurations ? *)

let set_sealing s =
  flags :=
    {
    sealing = s;
    stack  = !flags.stack;
    locality  = !flags.locality;
    unitialized  = !flags.unitialized;
    version =
      match !flags.sealing, s with
      | (false, true) -> (Printf.sprintf "%s +sealing" !flags.version)
      | (true, false) -> (Printf.sprintf "%s -sealing" !flags.version)
      | _ -> !flags.version;
    }

let set_stack t =
  flags :=
    {
    sealing = !flags.sealing;
    stack  = t;
    locality  = !flags.locality;
    unitialized  = !flags.unitialized;
    version =
      match !flags.stack, t with
      | (false, true) -> (Printf.sprintf "%s +stack" !flags.version)
      | (true, false) -> (Printf.sprintf "%s -stack" !flags.version)
      | _ -> !flags.version;
    }

let set_locality l =
  flags :=
    {
    sealing = !flags.sealing;
    stack  = !flags.stack;
    locality  = l;
    unitialized  = !flags.unitialized;
    version =
      if !flags.locality = l
      then !flags.version
      else
        (Printf.sprintf "%s +%s" !flags.version
        (match l with
        | Global -> "Global"
        | Local -> "Local"
        | Directed -> "Directed"
        ))
    }

let set_uperms u =
  flags :=
    {
    sealing = !flags.sealing;
    stack  = !flags.stack;
    locality  = !flags.locality;
    unitialized  = u;
    version =
      match !flags.unitialized, u with
      | (false, true) -> (Printf.sprintf "%s +uperms" !flags.version)
      | (true, false) -> (Printf.sprintf "%s -uperms" !flags.version)
      | _ -> !flags.version;
    }

exception NotSupported of string
let not_supported s = raise @@ NotSupported (Printf.sprintf "%s (%s)" s !flags.version)
let instruction_not_supported i =
    not_supported (Printf.sprintf "Instruction %s is not supported." i)

(* NOTE it is basically (locality_flowsto (!flags.locality) g *)
let locality_allowed (g : locality) : bool =
  match !flags.locality with
  | Directed -> true
  | Local ->
    (match g with
     | Directed -> false
     | _ -> true)
  | Global ->
    (match g with
     | Global -> true
     | _ -> false)

let check_seal_perm _ =
  if not !flags.sealing
  then not_supported "Sealing permissions not supported"

let check_perm (p : Ast.perm) =
  if not !flags.unitialized
  then
    match p with
    | URW | URWL | URWLX | URWX -> not_supported "U permission not supported"
    | _ -> ()
  else if not (locality_allowed Local)
  then
    match p with
    | RWL | RWLX | URWLX | URWL -> not_supported "WL permission not supported"
    | _ -> ()

let check_locality (g : locality) =
  if not (locality_allowed g)
  then
    match g with
    | Directed -> not_supported "Directed locality not supported."
    | Local -> not_supported "Local locality not supported."
    | Global -> not_supported "Global locality not supported." (* should not happen *)

let check_register (r : regname) =
  match r with
  | STK ->
    if not !flags.stack
    then not_supported "STK register not supported."
  | _ -> ()

let check_machine_op (i : Ast.machine_op)=
  let instruction_not_supported s = instruction_not_supported s in
  let check_zreg z = match z with | Const _ -> () | Register r -> check_register r
  in
  (match i with
   | GetOType (_, _) ->
     if not !flags.sealing then instruction_not_supported "GetOType"
   | Seal (_, _, _) ->
     if not !flags.sealing then instruction_not_supported "Seal"
   | UnSeal (_, _, _) ->
     if not !flags.sealing then instruction_not_supported "UnSeal"
   | GetL (_, _) ->
     if not !flags.stack then instruction_not_supported "GetL"
   | LoadU (_, _, _) ->
     if not !flags.unitialized then instruction_not_supported "LoadU"
   | StoreU (_, _, _) ->
     if not !flags.unitialized then instruction_not_supported "StoreU"
   | PromoteU _ ->
     if not !flags.unitialized then instruction_not_supported "PromoteU"
   | _ -> ())
  ;
  (match i with
   | PromoteU r
   | Jmp r -> check_register r
   | GetL (r1, r2)
   | GetB (r1, r2)
   | GetE (r1, r2)
   | GetA (r1, r2)
   | GetP (r1, r2)
   | GetOType (r1, r2)
   | GetWType (r1, r2)
   | Load (r1, r2)
   | Jnz (r1, r2) -> check_register r1 ; check_register r2
   | Lea (r1, zr2)
   | Restrict (r1, zr2)
   | Store (r1, zr2)
   | Move (r1, zr2) -> check_register r1 ; check_zreg zr2
   | Add (r1, zr2, zr3)
   | Sub (r1, zr2, zr3)
   | Mul (r1, zr2, zr3)
   | Rem (r1, zr2, zr3)
   | Div (r1, zr2, zr3)
   | SubSeg (r1, zr2, zr3)
   | StoreU (r1, zr2, zr3)
   | Lt (r1, zr2, zr3) -> check_register r1 ; check_zreg zr2 ; check_zreg zr3
   | Seal (r1, r2, r3)
   | UnSeal (r1, r2, r3) -> check_register r1 ; check_register r2 ; check_register r3
   | LoadU (r1, r2, zr3) ->  check_register r1 ; check_register r2 ; check_zreg zr3
   | Fail
   | Halt -> ())

let check_word (w : Ast.word) =
  match w with
  | Sealed _
  | Sealable (SealRange _) ->
    if not !flags.sealing
    then not_supported "Sealing capabilities not supported."
  | Sealable (Cap (p, g, _, _, _)) ->
    check_perm p ; check_locality g
  | _ -> ()

let check_statement (i : Ast.statement)=
  match i with
  | Op op -> check_machine_op op
  | Word w -> check_word w

let rec check_program (prog : Ast.t) =
  match prog with
  | [] -> ()
  | i::l' -> check_statement i ; check_program l'
