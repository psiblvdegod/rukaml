open Frontend

let mangled_names : (Ident.t, Ident.t) Hashtbl.t = Hashtbl.create 100

let mangle (ident : Ident.t) =
  let buf = Buffer.create (String.length ident.hum_name) in
  String.iter
    (fun c ->
       match c with
       | 'A' .. 'Z' | 'a' .. 'z' | '0' .. '9' | '_' -> Buffer.add_char buf c
       | _ ->
         (* TODO: names may collide after this substitution *)
         Printf.bprintf buf "_code%u" (Char.code c))
    ident.hum_name;
  Buffer.add_string buf (Format.asprintf "_id%d" ident.id);
  Hashtbl.replace mangled_names ident (Ident.ident (Buffer.contents buf) ident.id)
;;

let contains (ident : Ident.t) = Hashtbl.mem mangled_names ident

let find ({ hum_name; _ } as ident : Ident.t) =
  match hum_name with
  | "main" ->
    (* TODO : worst cludge humanity have ever seen *)
    Ident.ident "main" 0
  | _ -> Hashtbl.find mangled_names ident
;;

let clear_names () = Hashtbl.clear mangled_names

let pp_mangled_names ppf () =
  Format.fprintf ppf "{\n";
  Hashtbl.iter
    (fun k v -> Format.fprintf ppf "\t%a ~> %a;\n" Ident.pp k Ident.pp v)
    mangled_names;
  Format.fprintf ppf "}"
;;

let%expect_test _ =
  mangle (Ident.ident "let*>" 42);
  pp_mangled_names Format.std_formatter ();
  [%expect
    {|
      {
      	let*> ~> let_code42_code62_id42;
      } |}]
;;

let rec rename_a ~(bounded : Ident.t list) : ANF.imm_expr -> ANF.imm_expr = function
  | (AUnit | AConst _ | APrimitive _) as i -> i
  | AVar v when contains v && not (List.mem v bounded) -> AVar (find v)
  | AVar _ as i -> i
  | ATuple (x1, x2, xs) ->
    ATuple (rename_a ~bounded x1, rename_a ~bounded x2, List.map (rename_a ~bounded) xs)
  | AConstruct (constr_name, fields) ->
    AConstruct (constr_name, List.map (rename_a ~bounded) fields)
  | AArray items -> AArray (List.map (rename_a ~bounded) items)
  | ALam (Apat_var v, rhs) -> ALam (Apat_var v, rename_e ~bounded:(v :: bounded) rhs)
  | ALam (lhs, rhs) -> ALam (lhs, rename_e ~bounded rhs)

and rename_c ~(bounded : Ident.t list) : ANF.c_expr -> ANF.c_expr = function
  | CApp (x1, x2, xs) ->
    CApp ((rename_a ~bounded) x1, (rename_a ~bounded) x2, List.map (rename_a ~bounded) xs)
  | CIte (x1, x2, x3) ->
    CIte ((rename_c ~bounded) x1, (rename_e ~bounded) x2, (rename_e ~bounded) x3)
  | CAtom atom -> CAtom (rename_a ~bounded atom)

and rename_e ~(bounded : Ident.t list) : ANF.expr -> ANF.expr = function
  | ELet (Frontend.Parsetree.NonRecursive, Apat_var v, rhs, body) ->
    ELet
      ( Frontend.Parsetree.NonRecursive
      , Apat_var v
      , rename_c ~bounded rhs
      , rename_e ~bounded:(v :: bounded) body )
  | ELet (Frontend.Parsetree.Recursive, Apat_var v, rhs, body) ->
    ELet
      ( Frontend.Parsetree.Recursive
      , Apat_var v
      , rename_c ~bounded:(v :: bounded) rhs
      , rename_e ~bounded:(v :: bounded) body )
  | ELet (flg, ((Apat_any | Apat_const _ | Apat_unit) as lhs), rhs, body) ->
    ELet (flg, lhs, rename_c ~bounded rhs, rename_e ~bounded body)
  | EComplex cexpr -> EComplex (rename_c ~bounded cexpr)
;;

(* bounded names would be ignored while renaming *)
let rename_stru ~(bounded : Ident.t list) : ANF.stru -> ANF.stru =
  let rename_stru_item ~(bounded : Ident.t list) : ANF.stru_item -> ANF.stru_item
    = function
    | ANF.ANF_vb (Frontend.Parsetree.NonRecursive, Apat_var name, rhs) ->
      let rhs = rename_e ~bounded rhs in
      mangle name;
      let lhs = ANF.Apat_var (find name) in
      ANF.ANF_vb (Frontend.Parsetree.NonRecursive, lhs, rhs)
    | ANF.ANF_vb (Frontend.Parsetree.Recursive, Apat_var name, rhs) ->
      mangle name;
      let rhs = rename_e ~bounded rhs in
      let lhs = ANF.Apat_var (find name) in
      ANF.ANF_vb (Frontend.Parsetree.Recursive, lhs, rhs)
    | ANF.ANF_vb (flg, ((Apat_any | Apat_const _ | Apat_unit) as lhs), rhs) ->
      ANF.ANF_vb (flg, lhs, rename_e ~bounded rhs)
  in
  fun stru -> List.map (rename_stru_item ~bounded) stru
;;

let run_single_test bounded input =
  match Frontend.Parsing.parse_structure input with
  | Error err -> Frontend.Parsing.pp_error Format.std_formatter err
  | Ok ast ->
    (match Frontend.Inferencer.structure Frontend.Typedtree.empty_table ast with
     | Error err -> Frontend.Inferencer.pp_error Format.std_formatter err
     | Ok (_env, typedtree) ->
       clear_names ();
       let anf = ANF.anf_stru typedtree in
       Format.printf "anf stru:\n";
       ANF.pp_stru Format.std_formatter anf;
       Format.printf "\n\nmangled stru:\n";
       ANF.pp_stru Format.std_formatter (rename_stru ~bounded anf);
       Format.printf "\n\nmangled names:\n";
       pp_mangled_names Format.std_formatter ())
;;

let initially_bounded = [ Ident.ident "print" 0; Ident.ident "exit" 0 ]

let%expect_test "global constant shadowing" =
  let input =
    {|
        let x = 10
        let f () = x
        let x = 20
        let g () = x
      |}
  in
  run_single_test initially_bounded input;
  [%expect
    {|
      anf stru:
      let x = 10

      let f weird1 = let () = weird1 in
                       x

      let x = 20

      let g weird2 = let () = weird2 in
                       x



      mangled stru:
      let x_id53 = 10

      let f_id54 weird1 = let () = weird1 in
                            x_id53

      let x_id55 = 20

      let g_id56 weird2 = let () = weird2 in
                            x_id55



      mangled names:
      {
      	x ~> x_id55;
      	g ~> g_id56;
      	x ~> x_id53;
      	f ~> f_id54;
      } |}]
;;

let%expect_test "let rec" =
  let input =
    {|
      let rec fact n = if n < 1 then 1 else n * fact (n - 1)
      let main = 0
    |}
  in
  run_single_test initially_bounded input;
  [%expect
    {|
    anf stru:
    let rec fact n = let temp3 = (n < 1) in
                                 let temp4 = (if temp3
                                             then 1
                                             else let temp5 = (n - 1) in
                                                    let temp6 = fact temp5  in
                                                      let temp7 = (n * temp6) in
                                                        temp7) in
                                   temp4

    let main = 0



    mangled stru:
    let rec fact_id59 n = let temp3 = (n < 1) in
                                            let temp4 = (if temp3
                                                        then 1
                                                        else let temp5 = (n - 1) in
                                                               let temp6 =
                                                               fact_id59 temp5  in
                                                                 let temp7 = (n * temp6) in
                                                                   temp7) in
                                              temp4

    let main = 0



    mangled names:
    {
    	main ~> main_id61;
    	fact ~> fact_id59;
    } |}]
;;

let%expect_test "replacing built-in print with user-defined one" =
  let input =
    {|
      let () = print 1
      let print _ = ()
      let () = print 2
    |}
  in
  run_single_test initially_bounded input;
  [%expect
    {|
    anf stru:
    let () = let temp8 = print 1  in
                         temp8

    let print weird9 = let _ = weird9 in
                         0

    let () = let temp10 = print 2  in
               temp10



    mangled stru:
    let () = let temp8 = print 1  in
                               temp8

    let print_id67 weird9 = let _ = weird9 in
                              0

    let () = let temp10 = print_id67 2  in
               temp10



    mangled names:
    {
    	print ~> print_id67;
    } |}]
;;

let%expect_test "shadowing global function with local one" =
  let input =
    {|
      let id x = x
      
      let () =
        let id y = y in
        id () 
      
      let () = id ()
    |}
  in
  run_single_test initially_bounded input;
  [%expect
    {|
    anf stru:
    let id x = x

    let () = let id y = y in
               let temp11 = id 0  in
                 temp11

    let () = let temp12 = id 0  in
               temp12



    mangled stru:
    let id_id71 x = x

    let () = let id y = y in
               let temp11 = id 0  in
                 temp11

    let () = let temp12 = id_id71 0  in
               temp12



    mangled names:
    {
    	id ~> id_id71;
    } |}]
;;

module StringMap = Stdlib.Map.Make (String)

let _aliases : string StringMap.t ref = ref StringMap.empty

let rec expand_aliases_a ~(bounded : string list) : ANF.imm_expr -> ANF.imm_expr
  = function
  | (AUnit | AConst _) as x -> x
  | AVar { hum_name; id }
    when (not (List.mem hum_name bounded)) && StringMap.mem hum_name !_aliases ->
    AVar { hum_name = StringMap.find hum_name !_aliases; id }
  | AVar _ as x -> x
  | APrimitive _ as x -> x
  | ATuple (x1, x2, xs) ->
    ATuple
      ( expand_aliases_a ~bounded x1
      , expand_aliases_a ~bounded x2
      , List.map (expand_aliases_a ~bounded) xs )
  | AConstruct (id, fields) -> AConstruct (id, List.map (expand_aliases_a ~bounded) fields)
  | AArray items -> AArray (List.map (expand_aliases_a ~bounded) items)
  | ALam ((Apat_var { hum_name; _ } as lhs), rhs) ->
    ALam (lhs, expand_aliases_e ~bounded:(hum_name :: bounded) rhs)
  | ALam (((Apat_any | Apat_unit | Apat_const _) as lhs), rhs) ->
    ALam (lhs, expand_aliases_e ~bounded rhs)

and expand_aliases_c ~(bounded : string list) : ANF.c_expr -> ANF.c_expr = function
  | CApp (c1, c2, cs) ->
    CApp
      ( expand_aliases_a ~bounded c1
      , expand_aliases_a ~bounded c2
      , List.map (expand_aliases_a ~bounded) cs )
  | CIte (c, t, e) ->
    CIte
      ( expand_aliases_c ~bounded c
      , expand_aliases_e ~bounded t
      , expand_aliases_e ~bounded e )
  | CAtom aexpr -> CAtom (expand_aliases_a ~bounded aexpr)

and expand_aliases_e ~(bounded : string list) : ANF.expr -> ANF.expr = function
  | EComplex cexpr -> EComplex (expand_aliases_c ~bounded cexpr)
  | ELet ((Parsetree.NonRecursive as flg), (Apat_var { hum_name; _ } as lhs), rhs, body)
    ->
    ELet
      ( flg
      , lhs
      , expand_aliases_c ~bounded rhs
      , expand_aliases_e ~bounded:(hum_name :: bounded) body )
  | ELet ((Parsetree.Recursive as flg), (Apat_var { hum_name; _ } as lhs), rhs, body) ->
    ELet
      ( flg
      , lhs
      , expand_aliases_c ~bounded:(hum_name :: bounded) rhs
      , expand_aliases_e ~bounded:(hum_name :: bounded) body )
  | ELet (flg, ((Apat_unit | Apat_any | Apat_const _) as lhs), rhs, body) ->
    ELet (flg, lhs, expand_aliases_c ~bounded rhs, expand_aliases_e ~bounded body)
;;

(* bounded names would be ignored while renaming *)
let expand_aliases_stru ~(aliases : (string * string) list) ~(bounded : string list)
  : ANF.stru -> ANF.stru
  =
  _aliases := StringMap.of_seq (List.to_seq aliases);
  let _bounded = ref bounded in
  let helper = function
    | ANF.ANF_vb (flg, ((Apat_any | Apat_unit | Apat_const _) as lhs), rhs) ->
      ANF.ANF_vb (flg, lhs, expand_aliases_e ~bounded:!_bounded rhs)
    | ANF.ANF_vb (Parsetree.NonRecursive, (Apat_var { hum_name; _ } as lhs), rhs) ->
      let rhs = expand_aliases_e ~bounded:!_bounded rhs in
      let () = _bounded := hum_name :: !_bounded in
      ANF.ANF_vb (Parsetree.NonRecursive, lhs, rhs)
    | ANF.ANF_vb (Parsetree.Recursive, (Apat_var { hum_name; _ } as lhs), rhs) ->
      let () = _bounded := hum_name :: !_bounded in
      ANF.ANF_vb (Parsetree.Recursive, lhs, expand_aliases_e ~bounded:!_bounded rhs)
  in
  List.map helper
;;

let aliases_for_tests = [ "print", "rukaml_print_int"; "exit", "rukaml_sys_exit" ]

let run_single_test bounded input =
  match Frontend.Parsing.parse_structure input with
  | Error err -> Frontend.Parsing.pp_error Format.std_formatter err
  | Ok ast ->
    (match Frontend.Inferencer.structure Frontend.Typedtree.empty_table ast with
     | Error err -> Frontend.Inferencer.pp_error Format.std_formatter err
     | Ok (_env, typedtree) ->
       let anf = ANF.anf_stru typedtree in
       Format.printf "anf stru:\n";
       ANF.pp_stru Format.std_formatter anf;
       Format.printf "\n\n anf stru with resolved aliases:\n";
       ANF.pp_stru
         Format.std_formatter
         (expand_aliases_stru ~aliases:aliases_for_tests ~bounded anf))
;;
