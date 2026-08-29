let failure_message message =
  let detail = if String.equal message "" then "unknown parser failure" else message in
  "Parsing failed: " ^ detail ^ ". Check the token at the reported location and its operands."

let parse_program (filebuf : Lexing.lexbuf) =
  try
    match Parser_driver.parse_program filebuf with
    | Error _ as error -> error
    | Ok parsed -> (
        match Macro_expander.expand parsed with
        | Error _ as error -> error
        | Ok expanded ->
            let current_addresses_resolved = Current_address_resolver.resolve expanded in
            let labels_resolved = Label_resolver.resolve current_addresses_resolved in
            Ok (Expression_evaluator.evaluate labels_resolved))
  with
  | Label_resolver.Unknown_label label ->
      Error
        (Printf.sprintf "Unknown label %S. Define it with `%s:` or correct the label reference."
           label label)
  | Asm_ir.UnexpandedMacroException construct ->
      Error ("Internal assembler error: unexpanded " ^ construct ^ ".")
  | Asm_ir.UnresolvedExpressionException _ ->
      Error "Internal assembler error: unresolved expression reached the shared frontend."
  | Asm_ir.UnresolvedIrException construct ->
      Error ("Internal assembler error: unresolved " ^ construct ^ " reached the shared frontend.")
  | Failure message -> Error (failure_message message)

let parse_regfile (filebuf : Lexing.lexbuf) =
  try Parser_driver.parse_regfile filebuf with Failure message -> Error (failure_message message)
