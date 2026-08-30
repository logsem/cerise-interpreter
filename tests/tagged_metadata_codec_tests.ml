open Cerise
module Private_codec = Tagged_metadata_codec

let tagged (tag : int) (payload : int) : Z.t =
  Z.logor (Z.of_int tag) (Z.shift_left (Z.of_int payload) 3)

let check_z (label : string) (expected : Z.t) (actual : Z.t) : unit =
  Alcotest.(check string) label (Z.to_string expected) (Z.to_string actual)

let check_error (type value) (label : string) (expected : string)
    (decoder : Z.t -> (value, string) result) (encoded : Z.t) : unit =
  Alcotest.(check (result reject string)) label (Error expected) (decoder encoded)

let check_total_rejection (type value) (label : string) (decoder : Z.t -> (value, string) result)
    (encoded : Z.t) : unit =
  Alcotest.(check bool)
    label true
    (match decoder encoded with Error _ -> true | Ok _ -> false | exception _ -> false)

type sample = A | B | Unmapped
type place = Low | High

let sample_scalar = Private_codec.finite_scalar ~name:"sample" [ (A, Z.zero); (B, Z.of_int 2) ]
let place_scalar = Private_codec.finite_scalar ~name:"place" [ (Low, Z.zero); (High, Z.one) ]

let sample_pattern =
  Private_codec.encoding_pattern ~name:"sample" ~tag:2 ~wrong_tag_error:"wrong sample tag"
    ~malformed_payload_error:"malformed sample"
    (Private_codec.scalar_payload sample_scalar)

let pair_pattern =
  Private_codec.encoding_pattern ~name:"sample/place" ~tag:5 ~wrong_tag_error:"wrong pair tag"
    ~malformed_payload_error:"malformed pair"
    (Private_codec.packed_pair ~low_width:2 ~high_width:1 sample_scalar place_scalar)

let private_engine_round_trips (() : unit) : unit =
  let layout =
    Private_codec.compile
      [ Private_codec.pattern sample_pattern; Private_codec.pattern pair_pattern ]
    |> Result.get_ok
  in
  check_z "scalar numeric encoding" (tagged 2 2)
    (Private_codec.encode layout sample_pattern B |> Result.get_ok);
  Alcotest.(check bool)
    "scalar round trip" true
    (Private_codec.decode layout sample_pattern (tagged 2 2) = Ok B);
  check_z "packed numeric encoding" (tagged 5 6)
    (Private_codec.encode layout pair_pattern (B, High) |> Result.get_ok);
  Alcotest.(check bool)
    "packed round trip" true
    (Private_codec.decode layout pair_pattern (tagged 5 6) = Ok (B, High));
  Alcotest.(check bool)
    "unmapped scalar rejected" true
    (Result.is_error (Private_codec.encode layout sample_pattern Unmapped));
  check_error "negative input" "wrong sample tag"
    (Private_codec.decode layout sample_pattern)
    Z.minus_one;
  check_error "unknown tag" "wrong sample tag"
    (Private_codec.decode layout sample_pattern)
    (tagged 7 0);
  check_error "unknown scalar payload" "malformed sample"
    (Private_codec.decode layout sample_pattern)
    (tagged 2 1);
  check_error "unknown packed field" "malformed pair"
    (Private_codec.decode layout pair_pattern)
    (tagged 5 1);
  let huge = Z.logor (Z.of_int 5) (Z.shift_left Z.one 100_000) in
  check_error "huge packed payload" "malformed pair" (Private_codec.decode layout pair_pattern) huge

let pattern ~(name : string) ~(tag : int) scalar =
  Private_codec.encoding_pattern ~name ~tag ~wrong_tag_error:"wrong" ~malformed_payload_error:"bad"
    (Private_codec.scalar_payload scalar)

let compile_has (predicate : Private_codec.error -> bool) (patterns : Private_codec.pattern list) :
    bool =
  match Private_codec.compile patterns with
  | Error errors -> List.exists predicate errors
  | Ok _ -> false

let private_engine_validation (() : unit) : unit =
  let good = Private_codec.finite_scalar ~name:"good" [ (A, Z.zero); (B, Z.one) ] in
  let named = pattern ~name:"same" ~tag:0 good in
  let duplicate_name = pattern ~name:"same" ~tag:1 good in
  let duplicate_tag = pattern ~name:"different" ~tag:0 good in
  Alcotest.(check bool)
    "duplicate pattern name" true
    (compile_has
       (function Private_codec.Duplicate_pattern_name "same" -> true | _ -> false)
       [ Private_codec.pattern named; Private_codec.pattern duplicate_name ]);
  Alcotest.(check bool)
    "duplicate tag" true
    (compile_has
       (function Private_codec.Duplicate_tag 0 -> true | _ -> false)
       [ Private_codec.pattern named; Private_codec.pattern duplicate_tag ]);
  let invalid_tag = pattern ~name:"invalid tag" ~tag:8 good in
  Alcotest.(check bool)
    "invalid tag" true
    (compile_has
       (function Private_codec.Invalid_tag { tag = 8; _ } -> true | _ -> false)
       [ Private_codec.pattern invalid_tag ]);
  let invalid_name = pattern ~name:"  " ~tag:0 good in
  Alcotest.(check bool)
    "invalid pattern name" true
    (compile_has
       (function Private_codec.Invalid_pattern_name "  " -> true | _ -> false)
       [ Private_codec.pattern invalid_name ]);
  let duplicate_encoding =
    Private_codec.finite_scalar ~name:"duplicate encoding" [ (A, Z.zero); (B, Z.zero) ]
  in
  Alcotest.(check bool)
    "duplicate scalar encoding" true
    (compile_has
       (function Private_codec.Duplicate_scalar_encoding _ -> true | _ -> false)
       [ Private_codec.pattern (pattern ~name:"duplicate mapping" ~tag:0 duplicate_encoding) ]);
  let negative = Private_codec.finite_scalar ~name:"negative" [ (A, Z.minus_one) ] in
  Alcotest.(check bool)
    "negative scalar payload" true
    (compile_has
       (function Private_codec.Negative_scalar_encoding _ -> true | _ -> false)
       [ Private_codec.pattern (pattern ~name:"negative" ~tag:0 negative) ]);
  let overflow = Private_codec.finite_scalar ~name:"overflow" [ (A, Z.of_int 4) ] in
  let overflow_pattern =
    Private_codec.encoding_pattern ~name:"overflow" ~tag:0 ~wrong_tag_error:"wrong"
      ~malformed_payload_error:"bad"
      (Private_codec.packed_pair ~low_width:2 ~high_width:1 overflow place_scalar)
  in
  Alcotest.(check bool)
    "packed field overflow" true
    (compile_has
       (function Private_codec.Field_width_overflow { width = 2; _ } -> true | _ -> false)
       [ Private_codec.pattern overflow_pattern ]);
  let invalid_width_pattern =
    Private_codec.encoding_pattern ~name:"width" ~tag:0 ~wrong_tag_error:"wrong"
      ~malformed_payload_error:"bad"
      (Private_codec.packed_pair ~low_width:0 ~high_width:1 good place_scalar)
  in
  Alcotest.(check bool)
    "invalid packed width" true
    (compile_has
       (function Private_codec.Invalid_field_width { width = 0; _ } -> true | _ -> false)
       [ Private_codec.pattern invalid_width_pattern ])

let vanilla_metadata (() : unit) : unit =
  let open Vanilla.Ast in
  let permissions = [ (O, 0); (E, 1); (RO, 4); (RX, 5); (RW, 6); (RWX, 7) ] in
  List.iter
    (fun (permission, scalar) ->
      let encoded = Vanilla.Codec.encode_permission permission in
      check_z "vanilla permission golden" (tagged 0 scalar) encoded;
      Alcotest.(check bool)
        "vanilla permission round trip" true
        (Vanilla.Codec.decode_permission encoded = Ok permission))
    permissions;
  let seals = [ ((false, false), 0); ((false, true), 1); ((true, false), 2); ((true, true), 3) ] in
  List.iter
    (fun (permission, scalar) ->
      let encoded = Vanilla.Codec.encode_seal_permission permission in
      check_z "vanilla seal golden" (tagged 1 scalar) encoded;
      Alcotest.(check bool)
        "vanilla seal round trip" true
        (Vanilla.Codec.decode_seal_permission encoded = Ok permission))
    seals;
  List.iter
    (fun (word_type, scalar) ->
      check_z "vanilla word-type golden" (tagged 3 scalar)
        (Vanilla.Codec.encode_word_type word_type))
    [ (Integer, 0); (Capability, 1); (Seal_range, 2); (Sealed, 3) ];
  check_error "vanilla permission wrong tag" "not a vanilla permission encoding"
    Vanilla.Codec.decode_permission (tagged 1 0);
  check_error "vanilla permission malformed" "unknown vanilla permission"
    Vanilla.Codec.decode_permission (tagged 0 2);
  check_error "vanilla seal wrong tag" "not a seal permission encoding"
    Vanilla.Codec.decode_seal_permission (tagged 0 0);
  check_error "vanilla seal malformed" "unknown seal permission"
    Vanilla.Codec.decode_seal_permission (tagged 1 4)

let locality_metadata (() : unit) : unit =
  let open Locality_cerise.Ast in
  let permissions =
    [ (O, 0); (E, 1); (RO, 4); (RX, 5); (RW, 6); (RWX, 7); (RWL, 14); (RWLX, 15) ]
  in
  let localities = [ (Local, 1); (Global, 2) ] in
  List.iter
    (fun (permission, permission_scalar) ->
      let encoded = Locality_cerise.Codec.encode_permission permission in
      check_z "locality-Cerise permission golden" (tagged 0 permission_scalar) encoded;
      Alcotest.(check bool)
        "locality-Cerise permission round trip" true
        (Locality_cerise.Codec.decode_permission encoded = Ok permission);
      List.iter
        (fun (locality, locality_scalar) ->
          let encoded = Locality_cerise.Codec.encode_permission_locality permission locality in
          check_z "locality-Cerise permission/locality golden"
            (tagged 4 ((locality_scalar lsl 5) + permission_scalar))
            encoded;
          Alcotest.(check bool)
            "locality-Cerise permission/locality round trip" true
            (Locality_cerise.Codec.decode_permission_locality encoded = Ok (permission, locality)))
        localities)
    permissions;
  List.iter
    (fun (locality, scalar) ->
      let encoded = Locality_cerise.Codec.encode_locality locality in
      check_z "locality-Cerise locality golden" (tagged 2 scalar) encoded;
      Alcotest.(check bool)
        "locality-Cerise locality round trip" true
        (Locality_cerise.Codec.decode_locality encoded = Ok locality))
    localities;
  let seals = [ ((false, false), 0); ((false, true), 1); ((true, false), 2); ((true, true), 3) ] in
  List.iter
    (fun (permission, permission_scalar) ->
      let encoded = Locality_cerise.Codec.encode_seal_permission permission in
      check_z "locality-Cerise seal golden" (tagged 1 permission_scalar) encoded;
      Alcotest.(check bool)
        "locality-Cerise seal round trip" true
        (Locality_cerise.Codec.decode_seal_permission encoded = Ok permission);
      List.iter
        (fun (locality, locality_scalar) ->
          let encoded = Locality_cerise.Codec.encode_seal_permission_locality permission locality in
          check_z "locality-Cerise seal/locality golden"
            (tagged 5 ((locality_scalar lsl 2) + permission_scalar))
            encoded;
          Alcotest.(check bool)
            "locality-Cerise seal/locality round trip" true
            (Locality_cerise.Codec.decode_seal_permission_locality encoded
            = Ok (permission, locality)))
        localities)
    seals;
  List.iter
    (fun (word_type, scalar) ->
      check_z "locality-Cerise word-type golden" (tagged 3 scalar)
        (Locality_cerise.Codec.encode_word_type word_type))
    [ (Integer, 0); (Capability, 1); (Seal_range, 2); (Sealed, 3) ];
  check_error "local permission wrong tag" "not a locality-Cerise permission encoding"
    Locality_cerise.Codec.decode_permission (tagged 1 0);
  check_error "local permission malformed" "unknown locality-Cerise permission"
    Locality_cerise.Codec.decode_permission (tagged 0 2);
  check_error "local seal wrong tag" "not a seal permission encoding"
    Locality_cerise.Codec.decode_seal_permission (tagged 0 0);
  check_error "local seal malformed" "unknown seal permission"
    Locality_cerise.Codec.decode_seal_permission (tagged 1 4);
  check_error "local locality wrong tag" "not a locality encoding"
    Locality_cerise.Codec.decode_locality (tagged 0 0);
  check_error "local locality malformed" "unknown locality" Locality_cerise.Codec.decode_locality
    (tagged 2 0);
  check_error "local pair wrong tag" "not a permission/locality encoding"
    Locality_cerise.Codec.decode_permission_locality (tagged 0 0);
  check_error "local pair malformed" "unknown permission/locality"
    Locality_cerise.Codec.decode_permission_locality (tagged 4 0);
  check_error "local seal pair wrong tag" "not a seal-permission/locality encoding"
    Locality_cerise.Codec.decode_seal_permission_locality (tagged 0 0);
  check_error "local seal pair malformed" "unknown seal-permission/locality"
    Locality_cerise.Codec.decode_seal_permission_locality (tagged 5 0)

let historical_metadata (() : unit) : unit =
  let u_permissions =
    let open Ucerise.Ast in
    [
      (O, 0);
      (E, 1);
      (RO, 4);
      (RX, 5);
      (RW, 6);
      (RWX, 7);
      (RWL, 14);
      (RWLX, 15);
      (URW, 22);
      (URWX, 23);
      (URWL, 30);
      (URWLX, 31);
    ]
  in
  let u_localities = [ (Ucerise.Ast.Local, 1); (Ucerise.Ast.Global, 2) ] in
  List.iter
    (fun (permission, permission_scalar) ->
      let encoded = Ucerise.Codec.encode_permission permission in
      check_z "uCerise permission golden" (tagged 0 permission_scalar) encoded;
      Alcotest.(check bool)
        "uCerise permission round trip" true
        (Ucerise.Codec.decode_permission encoded = Ok permission);
      List.iter
        (fun (locality, locality_scalar) ->
          let encoded = Ucerise.Codec.encode_permission_locality permission locality in
          check_z "uCerise pair golden"
            (tagged 4 ((locality_scalar lsl 5) + permission_scalar))
            encoded;
          Alcotest.(check bool)
            "uCerise pair round trip" true
            (Ucerise.Codec.decode_permission_locality encoded = Ok (permission, locality)))
        u_localities)
    u_permissions;
  List.iter
    (fun (locality, scalar) ->
      let encoded = Ucerise.Codec.encode_locality locality in
      check_z "uCerise locality golden" (tagged 2 scalar) encoded;
      Alcotest.(check bool)
        "uCerise locality round trip" true
        (Ucerise.Codec.decode_locality encoded = Ok locality))
    u_localities;
  let m_permissions =
    let open Mcerise.Ast in
    [
      (O, 0);
      (E, 1);
      (RO, 4);
      (RX, 5);
      (RW, 6);
      (RWX, 7);
      (RWL, 14);
      (RWLX, 15);
      (URW, 22);
      (URWX, 23);
      (URWL, 30);
      (URWLX, 31);
    ]
  in
  let m_localities =
    [ (Mcerise.Ast.Directed, 0); (Mcerise.Ast.Local, 1); (Mcerise.Ast.Global, 2) ]
  in
  List.iter
    (fun (permission, permission_scalar) ->
      let encoded = Mcerise.Codec.encode_permission permission in
      check_z "mCerise permission golden" (tagged 0 permission_scalar) encoded;
      Alcotest.(check bool)
        "mCerise permission round trip" true
        (Mcerise.Codec.decode_permission encoded = Ok permission);
      List.iter
        (fun (locality, locality_scalar) ->
          let encoded = Mcerise.Codec.encode_permission_locality permission locality in
          check_z "mCerise pair golden"
            (tagged 4 ((locality_scalar lsl 5) + permission_scalar))
            encoded;
          Alcotest.(check bool)
            "mCerise pair round trip" true
            (Mcerise.Codec.decode_permission_locality encoded = Ok (permission, locality)))
        m_localities)
    m_permissions;
  List.iter
    (fun (locality, scalar) ->
      let encoded = Mcerise.Codec.encode_locality locality in
      check_z "mCerise locality golden" (tagged 2 scalar) encoded;
      Alcotest.(check bool)
        "mCerise locality round trip" true
        (Mcerise.Codec.decode_locality encoded = Ok locality))
    m_localities;
  check_error "u permission wrong tag" "not a uCerise permission encoding"
    Ucerise.Codec.decode_permission (tagged 1 0);
  check_error "u permission malformed" "unknown uCerise permission" Ucerise.Codec.decode_permission
    (tagged 0 2);
  check_error "u locality wrong tag" "not a uCerise locality encoding" Ucerise.Codec.decode_locality
    (tagged 0 0);
  check_error "u locality malformed" "unknown uCerise locality" Ucerise.Codec.decode_locality
    (tagged 2 0);
  check_error "u pair wrong tag" "not a uCerise permission/locality encoding"
    Ucerise.Codec.decode_permission_locality (tagged 0 0);
  check_error "u pair malformed" "unknown uCerise permission/locality"
    Ucerise.Codec.decode_permission_locality (tagged 4 0);
  check_error "m permission wrong tag" "not a mCerise permission encoding"
    Mcerise.Codec.decode_permission (tagged 1 0);
  check_error "m permission malformed" "unknown mCerise permission" Mcerise.Codec.decode_permission
    (tagged 0 2);
  check_error "m locality wrong tag" "not a mCerise locality encoding" Mcerise.Codec.decode_locality
    (tagged 0 0);
  check_error "m locality malformed" "unknown mCerise locality" Mcerise.Codec.decode_locality
    (tagged 2 3);
  check_error "m pair wrong tag" "not a mCerise permission/locality encoding"
    Mcerise.Codec.decode_permission_locality (tagged 0 0);
  check_error "m pair malformed" "unknown mCerise permission/locality"
    Mcerise.Codec.decode_permission_locality (tagged 4 3)

let cerisier_metadata (() : unit) : unit =
  let open Cerisier.Ast in
  let permissions = [ (O, 0); (E, 1); (RO, 4); (RX, 5); (RW, 6); (RWX, 7) ] in
  List.iter
    (fun (permission, scalar) ->
      let encoded = Cerisier.Codec.encode_permission permission in
      check_z "Cerisier permission golden" (tagged 0 scalar) encoded;
      Alcotest.(check bool)
        "Cerisier permission round trip" true
        (Cerisier.Codec.decode_permission encoded = Ok permission))
    permissions;
  let seals = [ ((false, false), 0); ((false, true), 1); ((true, false), 2); ((true, true), 3) ] in
  List.iter
    (fun (permission, scalar) ->
      let encoded = Cerisier.Codec.encode_seal_permission permission in
      check_z "Cerisier seal golden" (tagged 1 scalar) encoded;
      Alcotest.(check bool)
        "Cerisier seal round trip" true
        (Cerisier.Codec.decode_seal_permission encoded = Ok permission))
    seals;
  List.iter
    (fun (word_type, scalar) ->
      let encoded = Cerisier.Codec.encode_word_type word_type in
      check_z "Cerisier word-type golden" (tagged 3 scalar) encoded;
      Alcotest.(check bool)
        "Cerisier word-type round trip" true
        (Cerisier.Codec.decode_word_type encoded = Ok word_type))
    [ (Integer, 0); (Capability, 1); (Seal_range, 2); (Sealed, 3) ];
  check_error "Cerisier permission wrong tag" "not a Cerisier permission encoding"
    Cerisier.Codec.decode_permission (tagged 1 0);
  check_error "Cerisier permission malformed" "unknown Cerisier permission"
    Cerisier.Codec.decode_permission (tagged 0 2);
  check_error "Cerisier seal wrong tag" "not a seal permission encoding"
    Cerisier.Codec.decode_seal_permission (tagged 0 0);
  check_error "Cerisier seal malformed" "unknown seal permission"
    Cerisier.Codec.decode_seal_permission (tagged 1 4);
  check_error "Cerisier word wrong tag" "not a word-type encoding" Cerisier.Codec.decode_word_type
    (tagged 0 0);
  check_error "Cerisier word malformed" "unknown word type" Cerisier.Codec.decode_word_type
    (tagged 3 4)

let griotte_metadata (() : unit) : unit =
  let open Griotte.Ast in
  let rx_values = [ (Orx, 0); (R, 1); (X, 2); (XSR, 3) ] in
  let write_values = [ (Ow, 0); (W, 1); (WL, 2) ] in
  let deep_local_values = [ (DL, 0); (LG, 1) ] in
  let deep_read_only_values = [ (DRO, 0); (LM, 1) ] in
  let localities = [ (Local, 0); (Global, 1) ] in
  List.iter
    (fun (rx, rx_scalar) ->
      List.iter
        (fun (write, write_scalar) ->
          List.iter
            (fun (deep_local, deep_local_scalar) ->
              List.iter
                (fun (deep_read_only, deep_read_only_scalar) ->
                  let permission = (rx, write, deep_local, deep_read_only) in
                  let scalar =
                    (rx_scalar lsl 4) + (write_scalar lsl 2) + (deep_local_scalar lsl 1)
                    + deep_read_only_scalar
                  in
                  let encoded = Griotte.Codec.encode_permission permission in
                  check_z "Griotte permission golden" (tagged 0 scalar) encoded;
                  Alcotest.(check bool)
                    "Griotte permission round trip" true
                    (Griotte.Codec.decode_permission encoded = Ok permission);
                  List.iter
                    (fun (locality, locality_scalar) ->
                      let encoded = Griotte.Codec.encode_permission_locality permission locality in
                      check_z "Griotte permission/locality golden"
                        (tagged 4 ((locality_scalar lsl 6) + scalar))
                        encoded;
                      Alcotest.(check bool)
                        "Griotte permission/locality round trip" true
                        (Griotte.Codec.decode_permission_locality encoded
                        = Ok (permission, locality)))
                    localities)
                deep_read_only_values)
            deep_local_values)
        write_values)
    rx_values;
  List.iter
    (fun (locality, scalar) ->
      let encoded = Griotte.Codec.encode_locality locality in
      check_z "Griotte locality golden" (tagged 2 scalar) encoded;
      Alcotest.(check bool)
        "Griotte locality round trip" true
        (Griotte.Codec.decode_locality encoded = Ok locality))
    localities;
  let seals = [ ((false, false), 0); ((false, true), 1); ((true, false), 2); ((true, true), 3) ] in
  List.iter
    (fun (permission, permission_scalar) ->
      let encoded = Griotte.Codec.encode_seal_permission permission in
      check_z "Griotte seal golden" (tagged 1 permission_scalar) encoded;
      Alcotest.(check bool)
        "Griotte seal round trip" true
        (Griotte.Codec.decode_seal_permission encoded = Ok permission);
      List.iter
        (fun (locality, locality_scalar) ->
          let encoded = Griotte.Codec.encode_seal_permission_locality permission locality in
          check_z "Griotte seal/locality golden"
            (tagged 5 ((locality_scalar lsl 2) + permission_scalar))
            encoded;
          Alcotest.(check bool)
            "Griotte seal/locality round trip" true
            (Griotte.Codec.decode_seal_permission_locality encoded = Ok (permission, locality)))
        localities)
    seals;
  List.iter
    (fun (word_type, scalar) ->
      let encoded = Griotte.Codec.encode_word_type word_type in
      check_z "Griotte word-type golden" (tagged 3 scalar) encoded;
      Alcotest.(check bool)
        "Griotte word-type round trip" true
        (Griotte.Codec.decode_word_type encoded = Ok word_type))
    [ (W_I, 0); (W_Cap, 1); (W_SealRange, 2); (W_Sealed, 3); (W_Sentry, 4) ];
  check_error "Griotte permission wrong tag" "not a Griotte permission encoding"
    Griotte.Codec.decode_permission (tagged 1 0);
  check_error "Griotte permission malformed" "unknown Griotte permission"
    Griotte.Codec.decode_permission (tagged 0 12);
  check_error "Griotte seal wrong tag" "not a Griotte seal permission encoding"
    Griotte.Codec.decode_seal_permission (tagged 0 0);
  check_error "Griotte seal malformed" "unknown Griotte seal permission"
    Griotte.Codec.decode_seal_permission (tagged 1 4);
  check_error "Griotte locality wrong tag" "not a Griotte locality encoding"
    Griotte.Codec.decode_locality (tagged 0 0);
  check_error "Griotte locality malformed" "unknown Griotte locality" Griotte.Codec.decode_locality
    (tagged 2 2);
  check_error "Griotte word wrong tag" "not a Griotte word type encoding"
    Griotte.Codec.decode_word_type (tagged 0 0);
  check_error "Griotte word malformed" "unknown Griotte word type" Griotte.Codec.decode_word_type
    (tagged 3 5);
  check_error "Griotte pair wrong tag" "not a Griotte permission/locality encoding"
    Griotte.Codec.decode_permission_locality (tagged 0 0);
  check_error "Griotte pair malformed" "unknown Griotte permission/locality"
    Griotte.Codec.decode_permission_locality (tagged 4 12);
  check_error "Griotte seal pair wrong tag" "not a Griotte seal permission/locality encoding"
    Griotte.Codec.decode_seal_permission_locality (tagged 0 0);
  check_error "Griotte seal pair malformed" "unknown Griotte seal permission/locality"
    Griotte.Codec.decode_seal_permission_locality (tagged 5 8)

let oversized_backend_inputs (() : unit) : unit =
  let huge = Z.shift_left Z.one 100_000 in
  let inputs tag = [ huge; Z.neg huge; Z.logor (Z.of_int tag) (Z.shift_left huge 3) ] in
  let check label decoder tag =
    List.iteri
      (fun index encoded ->
        check_total_rejection (Printf.sprintf "%s %d" label index) decoder encoded)
      (inputs tag)
  in
  check "vanilla permission" Vanilla.Codec.decode_permission 0;
  check "vanilla seal" Vanilla.Codec.decode_seal_permission 1;
  check "local permission" Locality_cerise.Codec.decode_permission 0;
  check "local seal" Locality_cerise.Codec.decode_seal_permission 1;
  check "local locality" Locality_cerise.Codec.decode_locality 2;
  check "local pair" Locality_cerise.Codec.decode_permission_locality 4;
  check "local seal pair" Locality_cerise.Codec.decode_seal_permission_locality 5;
  check "u permission" Ucerise.Codec.decode_permission 0;
  check "u locality" Ucerise.Codec.decode_locality 2;
  check "u pair" Ucerise.Codec.decode_permission_locality 4;
  check "m permission" Mcerise.Codec.decode_permission 0;
  check "m locality" Mcerise.Codec.decode_locality 2;
  check "m pair" Mcerise.Codec.decode_permission_locality 4;
  check "Cerisier permission" Cerisier.Codec.decode_permission 0;
  check "Cerisier seal" Cerisier.Codec.decode_seal_permission 1;
  check "Cerisier word" Cerisier.Codec.decode_word_type 3;
  check "Griotte permission" Griotte.Codec.decode_permission 0;
  check "Griotte seal" Griotte.Codec.decode_seal_permission 1;
  check "Griotte locality" Griotte.Codec.decode_locality 2;
  check "Griotte word" Griotte.Codec.decode_word_type 3;
  check "Griotte pair" Griotte.Codec.decode_permission_locality 4;
  check "Griotte seal pair" Griotte.Codec.decode_seal_permission_locality 5

let () =
  Alcotest.run "Tagged metadata codec"
    [
      ( "private engine",
        [
          Alcotest.test_case "round trips and malformed inputs" `Quick private_engine_round_trips;
          Alcotest.test_case "declaration validation" `Quick private_engine_validation;
        ] );
      ( "backend layouts",
        [
          Alcotest.test_case "Vanilla" `Quick vanilla_metadata;
          Alcotest.test_case "Locality Cerise" `Quick locality_metadata;
          Alcotest.test_case "uCerise and mCerise" `Quick historical_metadata;
          Alcotest.test_case "Cerisier" `Quick cerisier_metadata;
          Alcotest.test_case "Griotte" `Quick griotte_metadata;
          Alcotest.test_case "oversized inputs" `Quick oversized_backend_inputs;
        ] );
    ]
