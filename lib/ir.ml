(* Type definitions for the syntax AST *)

exception UnknownLabelException of string

type regname = PC | STK | Reg of int
type expr
  = IntLit of int
  | Label of string
  | AddOp of expr * expr
  | SubOp of expr * expr

type perm = O | E | RO | RX | RW | RWX | RWL | RWLX | URW | URWL | URWX | URWLX
type locality = Global | Local | Directed
type seal_perm = bool * bool
type wtype = W_I | W_Cap | W_SealRange | W_Sealed
type const_encoded = ConstExpr of expr
                   | Perm of perm * locality
                   | SealPerm of seal_perm * locality
                   | Locality of locality
                   | Wtype of wtype

type reg_or_const = Register of regname | Const of const_encoded
type sealable = Cap of perm * locality * expr * expr * expr | SealRange of seal_perm * locality * expr * expr * expr
type word = I of expr | Sealable of sealable | Sealed of expr * sealable
exception WordException of word

type machine_op
  =
  | Jmp of regname
  | Jnz of regname * regname
  | Move of regname * reg_or_const
  | Load of regname * regname
  | Store of regname * reg_or_const
  | Add of regname * reg_or_const * reg_or_const
  | Sub of regname * reg_or_const * reg_or_const
  | Mul of regname * reg_or_const * reg_or_const
  | Rem of regname * reg_or_const * reg_or_const
  | Div of regname * reg_or_const * reg_or_const
  | Lt of regname * reg_or_const * reg_or_const
  | Lea of regname * reg_or_const
  | Restrict of regname * reg_or_const
  | SubSeg of regname * reg_or_const * reg_or_const
  | GetL of regname * regname
  | GetB of regname * regname
  | GetE of regname * regname
  | GetA of regname * regname
  | GetP of regname * regname
  | GetOType of regname * regname
  | GetWType of regname * regname
  | Seal of regname * regname * regname
  | UnSeal of regname * regname * regname
  | LoadU of regname * regname * reg_or_const
  | StoreU of regname * reg_or_const * reg_or_const
  | PromoteU of regname
  | Fail
  | Halt
  | Lbl of string
  | Word of word
type statement = machine_op (* TODO: PseudoOp and LabelDefs *)

type t = statement list
type env = (string * int) list

let rec compute_env (i : int) (prog : t) (envr : env) : env =
  match prog with
  | [] -> envr
  | (Lbl s) :: p -> compute_env (i+1) p ((s, i - (List.length envr)) :: envr)
  | _ :: p -> compute_env (i+1) p envr

let rec eval_expr (envr : env) (e : expr) : Z.t =
  match e with
  | IntLit i -> (Z.of_int i)
  | Label s -> begin
      match List.find_opt (fun p -> (fst p) = s) envr with
      | Some (_,i) -> (Z.of_int i)
      | None -> raise (UnknownLabelException s)
    end
  | AddOp (e1, e2) -> Z.((eval_expr envr e1) + (eval_expr envr e2))
  | SubOp (e1, e2) -> Z.((eval_expr envr e1) - (eval_expr envr e2))

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

let translate_regname (r : regname) : Ast.regname =
  match r with
  | PC -> Ast.PC
  | STK -> Ast.STK
  | Reg i -> Ast.Reg i

(* Interleave two integers bitwise.
 * Example: x = 0b101 and y = 0b110
 * results in 0b111001. *)
let rec interleave_int (x : Z.t) (y : Z.t) : Z.t =
  let open Z in
  if x = zero && y = zero
  then zero
  else
    let x1 = x land one in
    let y1 = (y land one) lsl 1 in
    let x2 = x asr 1 in
    let y2 = y asr 1 in
    x1 + y1 + ((interleave_int x2 y2) lsl 2)

(* Encode two integers by interleaving their
 * absolute values bitwise, followed
 * by two bits representing signs.
 *)
let encode_int_int (x : Z.t) (y : Z.t) =
  let sign_bits = Z.of_int @@ begin
    match (Z.sign y, Z.sign x) with
      | (-1, -1) -> 0b11
      | (-1, (0|1))  -> 0b10
      | ((0|1), -1)  -> 0b01
      | ((0|1), (0|1)) -> 0b00
      | _ -> assert false
  end in
  let interleaved = interleave_int (Z.abs x) (Z.abs y) in
  Z.(sign_bits + (interleaved lsl 2))

let encode_perm (p : perm) : Z.t =
  Z.of_int @@
  match p with
  (*           ULRWX *)
  | O     -> 0b00000
  | E     -> 0b00001
  | RO    -> 0b00100
  | RX    -> 0b00101
  | RW    -> 0b00110
  | RWX   -> 0b00111
  | RWL   -> 0b01110
  | RWLX  -> 0b01111
  | URW   -> 0b10110
  | URWX  -> 0b10111
  | URWL  -> 0b11110
  | URWLX -> 0b11111

let encode_seal_perm (p : seal_perm) : Z.t =
  Z.of_int @@
  match p with
  (*                     SU *)
  | (false, false)  -> 0b00
  | (false, true)   -> 0b01
  | (true, false)   -> 0b10
  | (true, true)    -> 0b11

let encode_locality (g : locality) : Z.t =
  Z.of_int @@
  match g with
  | Directed -> 0b00
  | Local    -> 0b01
  | Global   -> 0b10

let encode_wtype (w : wtype) : Z.t =
  Z.of_int @@
  match w with
  | W_I -> 0b00
  | W_Cap -> 0b01
  | W_SealRange -> 0b10
  | W_Sealed -> 0b11

let encode_const (envr : env) (c : const_encoded) : Z.t =
  (* TODO should be some global parameters *)
  let _CONST_ENC      = 0b000 in
  let _PERM_ENC       = 0b001 in
  let _SEAL_PERM_ENC  = 0b010 in
  let _WTYPE_ENC      = 0b011 in
  let _LOCALITY_ENC   = 0b100 in
  let encode t z = encode_int_int (Z.of_int t) z in
  let encode2 t z1 z2 = encode_int_int (Z.of_int t) (encode_int_int z1 z2) in
  match c with
  | ConstExpr e -> encode _CONST_ENC (eval_expr envr e)
  | Perm (p, g) -> encode2 _PERM_ENC (encode_perm p) (encode_locality g)
  | SealPerm (sp, g) -> encode2 _SEAL_PERM_ENC (encode_seal_perm sp) (encode_locality g)
  | Wtype wt -> encode _WTYPE_ENC (encode_wtype wt)
  | Locality l -> encode _LOCALITY_ENC (encode_locality l)

let translate_reg_or_const (envr : env) (roc : reg_or_const) : Ast.reg_or_const =
  match roc with
  | Register r -> Ast.Register (translate_regname r)
  | Const c -> Ast.Const (encode_const envr c)

let translate_sealable (envr : env) (s : sealable) : Ast.sealable =
  match s with
  | Cap (p, l, b, e, a) ->
    Ast.Cap ((translate_perm p),
             (translate_locality l),
             (eval_expr envr b),
             (eval_expr envr e),
             (eval_expr envr a))
  | SealRange (p, l, b, e, a) ->
    Ast.SealRange (p,
                   (translate_locality l),
                   (eval_expr envr b),
                   (eval_expr envr e),
                   (eval_expr envr a))

let translate_word (envr : env) (w : word) : Ast.statement =
  match w with
  | I e -> Ast.Word (Ast.I (eval_expr envr e))
  | Sealable sb -> Ast.Word (Ast.Sealable (translate_sealable envr sb))
  | Sealed (o,sb) -> Ast.Word (Ast.Sealed (eval_expr envr o, (translate_sealable envr sb)))

let translate_instr (envr : env) (instr : machine_op) : Ast.machine_op =
  match instr with
  | Jmp r -> Ast.Jmp (translate_regname r)
  | Jnz (r1, r2) -> Ast.Jnz (translate_regname r1, translate_regname r2)
  | Move (r, c) -> Ast.Move (translate_regname r,
                             translate_reg_or_const envr c)
  | Load (r1, r2) -> Ast.Load (translate_regname r1,
                               translate_regname r2)
  | Store (r, c) -> Ast.Store (translate_regname r,
                               translate_reg_or_const envr c)
  | Add (r, c1, c2) -> Ast.Add (translate_regname r,
                                translate_reg_or_const envr c1,
                                translate_reg_or_const envr c2)
  | Sub (r, c1, c2) -> Ast.Sub (translate_regname r,
                                translate_reg_or_const envr c1,
                                translate_reg_or_const envr c2)
  | Mul (r, c1, c2) -> Ast.Mul (translate_regname r,
                                translate_reg_or_const envr c1,
                                translate_reg_or_const envr c2)
  | Rem (r, c1, c2) -> Ast.Rem (translate_regname r,
                                translate_reg_or_const envr c1,
                                translate_reg_or_const envr c2)
  | Div (r, c1, c2) -> Ast.Div (translate_regname r,
                                translate_reg_or_const envr c1,
                                translate_reg_or_const envr c2)
  | Lt (r, c1, c2) -> Ast.Lt (translate_regname r,
                              translate_reg_or_const envr c1,
                              translate_reg_or_const envr c2)
  | Lea (r, c) -> Ast.Lea (translate_regname r, translate_reg_or_const envr c)
  | Restrict (r, c) -> Ast.Restrict (translate_regname r,
                                     translate_reg_or_const envr c)
  | SubSeg (r, c1, c2) -> Ast.SubSeg (translate_regname r,
                                      translate_reg_or_const envr c1,
                                      translate_reg_or_const envr c2)
  | GetL (r1, r2) -> Ast.GetL (translate_regname r1, translate_regname r2)
  | GetB (r1, r2) -> Ast.GetB (translate_regname r1, translate_regname r2)
  | GetE (r1, r2) -> Ast.GetE (translate_regname r1, translate_regname r2)
  | GetA (r1, r2) -> Ast.GetA (translate_regname r1, translate_regname r2)
  | GetP (r1, r2) -> Ast.GetP (translate_regname r1, translate_regname r2)
  | GetOType (r1, r2) -> Ast.GetOType (translate_regname r1, translate_regname r2)
  | GetWType (r1, r2) -> Ast.GetWType (translate_regname r1, translate_regname r2)
  | Seal (r1, r2, r3) -> Ast.Seal (translate_regname r1, translate_regname r2, translate_regname r3)
  | UnSeal (r1, r2, r3) -> Ast.UnSeal (translate_regname r1, translate_regname r2, translate_regname r3)
  | LoadU (r1, r2, c) -> Ast.LoadU (translate_regname r1,
                                    translate_regname r2,
                                    translate_reg_or_const envr c)
  | StoreU (r, c1, c2) -> Ast.StoreU (translate_regname r,
                                      translate_reg_or_const envr c1,
                                      translate_reg_or_const envr c2)
  | PromoteU r -> Ast.PromoteU (translate_regname r)
  | Fail -> Ast.Fail
  | Halt -> Ast.Halt
  | Word w -> raise (WordException w)
  | Lbl s -> raise (UnknownLabelException s)

let rec translate_prog_aux (envr : env) (prog : t) : Ast.t =
  match prog with
  | [] -> []
  | (Lbl _) :: p -> translate_prog_aux envr p
  | (Word w) :: p -> translate_word envr w :: (translate_prog_aux envr p)
  | instr :: p -> (Op (translate_instr envr instr)) :: (translate_prog_aux envr p)

let translate_prog (prog : t) : Ast.t =
  let envr = compute_env 0 prog [] in
  translate_prog_aux envr prog
