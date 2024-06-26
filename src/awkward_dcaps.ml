open Libinterp
open Scall_dcap

let () =
  let radv = Ast.Reg 28 in
  let renv = Ast.Reg 30 in
  let prog = List.map (fun i -> Ast.Op i) (awkward_example radv @ adv_instr @ ret_instr) in

  (* let addr_max = List.length prog in *)
  (* let addr_max = (Int32.to_int Int32.max_int)/4096 in *)
  let addr_max = Z.of_int (Int32.to_int Int32.max_int / 32768) in
  let module Cfg = struct
    let addr_max = addr_max
  end in
  let module Ui = Interactive_ui.MkUi (Cfg) in
  let init_regfile = Machine.init_reg_state Cfg.addr_max in

  let m_init_state, m_init_conf = Program.init_machine prog init_regfile in
  let adv_upd radv conf =
    let addr_adv = Z.of_int (List.length (awkward_example radv)) in
    Machine.upd_reg radv
      (Ast.Sealable
         (Cap (E, Global, addr_adv, Int Z.(addr_adv + ~$(List.length adv_instr)), addr_adv)))
      conf
  in
  let ret_upd rret conf =
    let addr_ret = Z.of_int (List.length (awkward_example radv @ adv_instr)) in
    Machine.upd_reg rret
      (Ast.Sealable
         (Cap (E, Global, addr_ret, Int Z.(addr_ret + ~$(List.length ret_instr)), addr_ret)))
      conf
  in
  let env_upd renv conf =
    let addr_data = Z.of_int (List.length prog) in
    Machine.upd_reg renv
      (Ast.Sealable (Cap (RW, Global, addr_data, Int Z.(addr_data + ~$1), addr_data)))
      conf
  in
  let m_init_conf = env_upd renv (ret_upd (Reg 0) (adv_upd radv m_init_conf)) in
  let m_init = (m_init_state, m_init_conf) in

  (* let term = Term.create () in *)
  let prog_panel_start = ref Z.zero in
  let stk_panel_start = ref Z.(Cfg.addr_max / ~$2) in

  Ui.render_loop prog_panel_start stk_panel_start m_init
