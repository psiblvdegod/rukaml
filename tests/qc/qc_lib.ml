let patname =
  let open QCheck.Gen in
  pure (Printf.sprintf "%c%c%c")
  <*> char_range 'A' 'Z'
  <*> char_range 'a' 'z'
  <*> char_range 'a' 'z'
;;

(* Empty list or a singleton *)
let short_list arg =
  let open QCheck.Gen in
  oneof [ pure []; pure (fun x -> [ x ]) <*> arg ]
;;

type pattern =
  [%import:
    (Miniml.Parsetree.pattern
    [@with
      string := (string [@gen patname]);
      list := (list [@gen short_list (gen_pattern_sized (n / 3))])])]
[@@deriving qcheck]

let arbitrary_pattern_auto =
  let open QCheck.Iter in
  QCheck.make (gen_pattern_sized 5) ~print:(Format.asprintf "%a" Miniml.Pprint.pp_pattern)
;;

let varname =
  let open QCheck.Gen in
  pure (Printf.sprintf "%c%c%c")
  <*> char_range 'a' 'z'
  <*> char_range 'a' 'z'
  <*> char_range 'a' 'z'
;;

type const = [%import: Miniml.Parsetree.const] [@@deriving qcheck]
type rec_flag = [%import: Miniml.Parsetree.rec_flag] [@@deriving qcheck]

type expr =
  [%import:
    (Miniml.Parsetree.expr
    [@with
      string := (string [@gen varname]);
      list := (list [@gen short_list (gen_expr_sized (n / 3))])])]
[@@deriving qcheck]
(* include struct
  let _ = fun (_ : expr) -> ()

  let rec gen_expr_sized n =
    match n with
    | 0 ->
      QCheck.Gen.frequency
        [ 1, QCheck.Gen.pure EUnit
        ; 1, QCheck.Gen.map (fun gen0 -> EConst gen0) gen_const
        ; 1, QCheck.Gen.map (fun gen0 -> EVar gen0) varname
        ]
    | _ ->
      QCheck.Gen.frequency
        [ 1, QCheck.Gen.pure EUnit
        ; 1, QCheck.Gen.map (fun gen0 -> EConst gen0) gen_const
        ; 1, QCheck.Gen.map (fun gen0 -> EVar gen0) varname
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1, gen2) -> EIf (gen0, gen1, gen2))
              (QCheck.Gen.triple
                 (gen_expr_sized (n / 2))
                 (gen_expr_sized (n / 2))
                 (gen_expr_sized (n / 2))) )
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1) -> ELam (gen0, gen1))
              (QCheck.Gen.pair gen_pattern (gen_expr_sized (n / 2))) )
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1) -> EApp (gen0, gen1))
              (QCheck.Gen.pair (gen_expr_sized (n / 2)) (gen_expr_sized (n / 2))) )
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1, gen2) -> ETuple (gen0, gen1, gen2))
              (QCheck.Gen.triple
                 (gen_expr_sized (n / 2))
                 (gen_expr_sized (n / 2))
                 (short_list (gen_expr_sized (n / 2)))) )
        ; ( 1
          , QCheck.Gen.map
              (fun (gen0, gen1, gen2, gen3) -> ELet (gen0, gen1, gen2, gen3))
              (QCheck.Gen.quad
                 gen_rec_flag
                 gen_pattern
                 (gen_expr_sized (n / 2))
                 (gen_expr_sized (n / 2))) )
        ]
  ;;

  let _ = gen_expr_sized
  let gen_expr = QCheck.Gen.sized gen_expr_sized
  let _ = gen_expr
  let arb_expr_sized n = QCheck.make @@ gen_expr_sized n
  let _ = arb_expr_sized
  let arb_expr = QCheck.make @@ gen_expr
  let _ = arb_expr
end *)

let arbitrary_expr =
  let open QCheck.Iter in
  QCheck.make (gen_expr_sized 3) ~print:(Format.asprintf "%a" Miniml.Pprint.pp_expr)
;;

let run_pattern () =
  QCheck_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_pattern_auto (fun l ->
          Result.ok l
          = Angstrom.parse_string
              ~consume:Angstrom.Consume.All
              Miniml.Parsing.pattern
              (Format.asprintf "%a" Miniml.Pprint.pp_pattern l)))
    ]
;;

let run_expr () =
  QCheck_runner.run_tests
    [ QCheck.(
        Test.make arbitrary_expr (fun l ->
          Result.ok l
          = Angstrom.parse_string
              ~consume:Angstrom.Consume.All
              Miniml.Parsing.(pack.expr_long pack)
              (Format.asprintf "%a" Miniml.Pprint.pp_expr l)))
    ]
;;
