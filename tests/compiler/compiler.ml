(* [begin skip] *)

(* provides compability with ocamlopt (rukaml parser skips this block) *)

let string_of_char_list chs = String.of_seq (List.to_seq chs)
let string_len s = String.length s
let string_nth s n = s.[n]
let char_code = Char.code
let printf, fprintf, sprintf = Stdlib.Printf.(printf, fprintf, sprintf)
let array_get = Array.get
let array_set = Array.set
let list_length = List.length

let end_of_input ic =
  match input_char ic with
  | exception End_of_file -> true
  | c ->
    let () = seek_in ic (pos_in ic - 1) in
    false
;;

(* [end skip] *)

(* list primitives *)

let list_rev ls =
  let rec aux ls acc =
    match ls with
    | [] -> acc
    | x :: xs -> aux xs (x :: acc)
  in
  aux ls []
;;

let rec list_fold f ls init =
  match ls with
  | [] -> init
  | x :: xs -> list_fold f xs (f init x)
;;

let rec list_fold_right f ls init =
  match ls with
  | [] -> init
  | x :: xs -> f x (list_fold_right f xs init)
;;

let rec list_map f ls =
  match ls with
  | [] -> []
  | x :: xs -> f x :: list_map f xs
;;

let rec list_iter f ls =
  match ls with
  | [] -> ()
  | x :: xs ->
    let () = f x in
    list_iter f xs
;;

let rec list_length ls =
  match ls with
  | [] -> 0
  | _ :: xs -> 1 + list_length xs
;;

let is_lowercase ch = char_code 'a' <= char_code ch && char_code ch <= char_code 'z'
let is_uppercase ch = char_code 'A' <= char_code ch && char_code ch <= char_code 'Z'
let is_digit ch = char_code '0' <= char_code ch && char_code ch <= char_code '9'
let is_alphanum ch = is_lowercase ch || is_uppercase ch || is_digit ch || ch = '_'
let int_of_digit ch = char_code ch - char_code '0'

let int_of_digits chs =
  let rec pow b e = if e < 1 then 1 else b * pow b (e - 1) in
  let rez, _ =
    list_fold_right
      (fun digit (acc, pos) -> acc + (pow 10 pos * int_of_digit digit), pos + 1)
      chs
      (0, 0)
  in
  rez
;;

(* ast *)

type cpp_type =
  | CType_int
  | CType_void

type cpp_binop =
  | Cpp_add
  | Cpp_sub
  | Cpp_mul
  | Cpp_div
  | Cpp_eq
  | Cpp_ne
  | Cpp_lt
  | Cpp_gt
  | Cpp_le
  | Cpp_ge
  | Cpp_land
  | Cpp_lor

type cpp_expression =
  | CExpr_int of int
  | CExpr_var of string
  | CExpr_call of string * cpp_expression list
  | CExpr_binop of cpp_binop * cpp_expression * cpp_expression
  | CExpr_tern of cpp_expression * cpp_expression * cpp_expression
  | CExpr_assign of string * cpp_expression

type cpp_statement =
  | CStmt_return of cpp_expression option
  | CStmt_expr of cpp_expression
  | CStmt_block of cpp_statement list
  | CStmt_decl of cpp_type * string * cpp_expression option

type cpp_function =
  | CFunction of string * cpp_type * (cpp_type * string) list * cpp_statement list

type cpp_program = cpp_function list

let is_cpp_keyword s =
  match s with
  | "int" -> true
  | "void" -> true
  | _ -> false
;;

(* ast printer *)

let pp_cpp_type oc t =
  match t with
  | CType_int -> fprintf oc "int"
  | CType_void -> fprintf oc "void"
;;

let pp_cpp_binop oc op =
  match op with
  | Cpp_add -> fprintf oc "+"
  | Cpp_sub -> fprintf oc "-"
  | Cpp_mul -> fprintf oc "*"
  | Cpp_div -> fprintf oc "/"
  | Cpp_eq -> fprintf oc "=="
  | Cpp_ne -> fprintf oc "!="
  | Cpp_lt -> fprintf oc "<"
  | Cpp_gt -> fprintf oc ">"
  | Cpp_le -> fprintf oc "<="
  | Cpp_ge -> fprintf oc ">="
  | Cpp_land -> fprintf oc "&&"
  | Cpp_lor -> fprintf oc "||"
;;

let rec pp_cpp_expr oc e =
  match e with
  | CExpr_int n -> fprintf oc "%d" n
  | CExpr_var s -> fprintf oc "%s" s
  | CExpr_binop (op, e1, e2) ->
    fprintf oc "(%a %a %a)" pp_cpp_expr e1 pp_cpp_binop op pp_cpp_expr e2
  | CExpr_call (f, args) ->
    let () = fprintf oc "%s(" f in
    let rec pp_args xs =
      match xs with
      | [] -> ()
      | [ a ] -> pp_cpp_expr oc a
      | a :: rest ->
        let () = fprintf oc "%a, " pp_cpp_expr a in
        pp_args rest
    in
    let () = pp_args args in
    fprintf oc ")"
  | CExpr_assign (v, e) -> fprintf oc "%s = %a" v pp_cpp_expr e
  | CExpr_tern (c, t, e) ->
    fprintf oc "%a ? %a : %a" pp_cpp_expr c pp_cpp_expr t pp_cpp_expr e
;;

let rec pp_cpp_stmt oc stmt =
  match stmt with
  | CStmt_return None -> fprintf oc "return;"
  | CStmt_return (Some e) -> fprintf oc "return %a;" pp_cpp_expr e
  | CStmt_expr e -> fprintf oc "%a;" pp_cpp_expr e
  | CStmt_block stmts ->
    let () = fprintf oc "{\n" in
    let () = list_iter (fun s -> fprintf oc "  %a\n" pp_cpp_stmt s) stmts in
    fprintf oc "}"
  | CStmt_decl (t, name, None) -> fprintf oc "%a %s;" pp_cpp_type t name
  | CStmt_decl (t, name, Some e) ->
    fprintf oc "%a %s = %a;" pp_cpp_type t name pp_cpp_expr e
;;

let pp_cpp_function oc func =
  match func with
  | CFunction (fname, ftype, fparams, fbody) ->
    let () = fprintf oc "%a %s(" pp_cpp_type ftype fname in
    let rec pp_params xs =
      match xs with
      | [] -> ()
      | [ (t, n) ] -> fprintf oc "%a %s" pp_cpp_type t n
      | (t, n) :: rest ->
        let () = fprintf oc "%a %s, " pp_cpp_type t n in
        pp_params rest
    in
    let () = pp_params fparams in
    let () = fprintf oc ") {\n" in
    let () = list_iter (fprintf oc "  %a\n" pp_cpp_stmt) fbody in
    fprintf oc "}\n"
;;

let pp_cpp_program oc prog = list_iter (pp_cpp_function oc) prog

(* parser combinators *)

type parsing_error =
  | Perr_message of string
  | Perr_expected of string
  | Perr_unexpected_eof

type parser_state = string * int

type 'a parsing_result =
  | Prez_success of 'a * parser_state
  | Prez_error of parsing_error

type 'a parser = parser_state -> 'a parsing_result

let return x state = Prez_success (x, state)
let fail err _state = Prez_error err

let bind p f state =
  match p state with
  | Prez_error err -> Prez_error err
  | Prez_success (x, state2) -> f x state2
;;

let map p f state =
  match p state with
  | Prez_error err -> Prez_error err
  | Prez_success (rez, state) -> return (f rez) state
;;

let many p state =
  let rec aux acc state =
    match p state with
    | Prez_success (rez, state2) -> aux (rez :: acc) state2
    | Prez_error _ -> Prez_success (list_rev acc, state)
  in
  aux [] state
;;

let many1 p state =
  match p state with
  | Prez_error err -> Prez_error err
  | Prez_success (x, state2) ->
    (match many p state2 with
     | Prez_error err -> Prez_error err
     | Prez_success (xs, state3) -> return (x :: xs) state3)
;;

let choice2 p1 p2 state =
  match p1 state with
  | Prez_error _ -> p2 state
  | success -> success
;;

let rec choice ps state =
  match ps with
  | [] -> Prez_error (Perr_message "choice")
  | p :: ps ->
    (match p state with
     | Prez_success (rez, state2) -> Prez_success (rez, state2)
     | Prez_error _ -> choice ps state)
;;

let take_while pred state =
  let rec aux state acc =
    match state with
    | str, pos ->
      if pos >= string_len str
      then return (list_rev acc) (str, pos)
      else (
        let ch = string_nth str pos in
        if pred ch
        then aux (str, pos + 1) (ch :: acc)
        else return (list_rev acc) (str, pos))
  in
  aux state []
;;

let take_while1 pred (str, pos) =
  if pos >= string_len str
  then Prez_error Perr_unexpected_eof
  else (
    let ch1 = string_nth str pos in
    if pred ch1
    then map (take_while pred) (fun chs -> ch1 :: chs) (str, pos + 1)
    else Prez_error (Perr_message "take_while1"))
;;

let drop_left p1 p2 state =
  match p1 state with
  | Prez_error err -> Prez_error err
  | Prez_success (_, state2) -> p2 state2
;;

let drop_right p1 p2 state =
  match p1 state with
  | Prez_error err -> Prez_error err
  | Prez_success (rez, state2) ->
    (match p2 state2 with
     | Prez_error err -> Prez_error err
     | Prez_success (_, state3) -> return rez state3)
;;

let char ch (str, pos) =
  if pos >= string_len str
  then Prez_error Perr_unexpected_eof
  else if string_nth str pos = ch
  then return ch (str, pos + 1)
  else Prez_error (Perr_message "unexpected char")
;;

let string expected (str, pos) =
  let rec aux i =
    if i >= string_len expected
    then return expected (str, pos + i)
    else if pos + i >= string_len str
    then Prez_error Perr_unexpected_eof
    else if string_nth str (pos + i) = string_nth expected i
    then aux (i + 1)
    else Prez_error (Perr_message "unexpected string")
  in
  aux 0
;;

let rec drop_many ps p state =
  match ps with
  | [] -> p state
  | p1 :: ps ->
    (match p1 state with
     | Prez_error err -> Prez_error err
     | Prez_success (_, state2) -> drop_many ps p state2)
;;

(* parser implementation *)

let ws =
  drop_left (many (choice [ char ' '; char '\t'; char '\n'; char '\r' ])) (return ())
;;

let skip_ws p = drop_left ws p
let trim p = drop_right (drop_left ws p) ws
let parens p = drop_left (trim (char '(')) (drop_right p (trim (char ')')))
let braces p = drop_left (trim (char '{')) (drop_right p (trim (char '}')))

let parse_identifier =
  bind (take_while1 is_alphanum) (fun chs ->
    let name = string_of_char_list chs in
    if is_cpp_keyword name
    then fail (Perr_message "keyword as identifier")
    else if is_digit (string_nth name 0)
    then fail (Perr_expected "identifier")
    else return name)
;;

let parse_type =
  choice
    [ drop_left (string "int") (return CType_int)
    ; drop_left (string "void") (return CType_void)
    ]
;;

let make_binop_level expr_parser ops =
  let make_parser (op_str, op_ctor) =
    drop_left
      (trim (string op_str))
      (map expr_parser (fun right left -> CExpr_binop (op_ctor, left, right)))
  in
  let level = choice (list_map make_parser ops) in
  bind expr_parser (fun init ->
    map (many level) (fun fs -> list_fold (fun acc f -> f acc) fs init))
;;

let parse_expr_call parse_expr =
  bind (skip_ws parse_identifier) (fun fname ->
    parens
      (choice2
         (* at least one arg *)
         (bind parse_expr (fun e1 ->
            bind
              (many (skip_ws (drop_left (char ',') parse_expr)))
              (fun es -> return (CExpr_call (fname, e1 :: es)))))
         (* no args *)
         (return (CExpr_call (fname, [])))))
;;

let parse_expr_atom parse_expr =
  skip_ws
    (choice
       [ parse_expr_call parse_expr
       ; map (take_while1 is_digit) (fun chs -> CExpr_int (int_of_digits chs))
       ; map parse_identifier (fun name -> CExpr_var name)
       ; parens parse_expr
       ])
;;

let parse_expr_binop parse_expr =
  let parse_mul_div =
    make_binop_level (parse_expr_atom parse_expr) [ "*", Cpp_mul; "/", Cpp_div ]
  in
  let parse_add_sub = make_binop_level parse_mul_div [ "+", Cpp_add; "-", Cpp_sub ] in
  let parse_comp =
    make_binop_level
      parse_add_sub
      [ "==", Cpp_eq; "!=", Cpp_ne; "<", Cpp_lt; ">", Cpp_gt; "<=", Cpp_le; ">=", Cpp_ge ]
  in
  let parse_logical = make_binop_level parse_comp [ "&&", Cpp_land; "||", Cpp_lor ] in
  parse_logical
;;

let parse_expr_tern parse_expr =
  bind (parse_expr_binop parse_expr) (fun c ->
    choice2
      (drop_left
         (skip_ws (char '?'))
         (bind parse_expr (fun t ->
            drop_left
              (skip_ws (char ':'))
              (bind parse_expr (fun e -> return (CExpr_tern (c, t, e)))))))
      (return c))
;;

let parse_expr_assign parse_expr =
  choice2
    (bind (skip_ws parse_identifier) (fun v ->
       drop_left
         (skip_ws (char '='))
         (bind parse_expr (fun rhs -> return (CExpr_assign (v, rhs))))))
    (parse_expr_tern parse_expr)
;;

let rec parse_expression state = parse_expr_assign parse_expression state

let parse_stmt_return =
  drop_left
    (skip_ws (string "return"))
    (choice2
       (bind parse_expression (fun e ->
          drop_left (skip_ws (char ';')) (return (CStmt_return (Some e)))))
       (skip_ws (drop_left (char ';') (return (CStmt_return None)))))
;;

let parse_stmt_decl =
  bind (skip_ws parse_type) (fun t ->
    bind (skip_ws parse_identifier) (fun name ->
      choice2
        (drop_left
           (drop_left ws (char '='))
           (bind parse_expression (fun e ->
              drop_left (skip_ws (char ';')) (return (CStmt_decl (t, name, Some e))))))
        (drop_left (skip_ws (char ';')) (return (CStmt_decl (t, name, None))))))
;;

let parse_stmt_block parse_stmt =
  map (braces (many parse_stmt)) (fun stmts -> CStmt_block stmts)
;;

let parse_stmt_expr =
  bind parse_expression (fun e -> drop_left (skip_ws (char ';')) (return (CStmt_expr e)))
;;

let rec parse_statement state =
  (choice
     [ parse_stmt_return
     ; parse_stmt_decl
     ; parse_stmt_block parse_statement
     ; parse_stmt_expr
     ])
    state
;;

let parse_fun_param =
  bind (skip_ws parse_type) (fun ty ->
    bind (skip_ws parse_identifier) (fun name -> return (ty, name)))
;;

let parse_fun_params =
  parens
    (choice2
       (* at least one param *)
       (bind parse_fun_param (fun p1 ->
          bind
            (many (skip_ws (drop_left (char ',') parse_fun_param)))
            (fun ps -> return (p1 :: ps))))
       (* no params *)
       (return []))
;;

let parse_function =
  bind (skip_ws parse_type) (fun ty ->
    bind (skip_ws parse_identifier) (fun name ->
      bind parse_fun_params (fun params ->
        bind parse_statement (fun body ->
          match body with
          | CStmt_block stmts -> return (CFunction (name, ty, params, stmts))
          | _ -> fail (Perr_expected "function body")))))
;;

let parse_program = many1 (skip_ws parse_function)

let pp_parsing_error oc err =
  match err with
  | Perr_message msg -> fprintf oc "error: %s" msg
  | Perr_expected x -> fprintf oc "expected: %s" x
  | Perr_unexpected_eof -> fprintf oc "unexpected eof"
;;

(* codegen *)

let label_counter = [| 0 |] (* TODO: replace this cringe with ref *)

let fresh_label prefix =
  let n = array_get label_counter 0 in
  let () = array_set label_counter 0 (n + 1) in
  sprintf "%s_%d" prefix n
;;

let local_vars = [| [] |] (* TODO: replace this cringe with ref *)
let clear_locals () = array_set local_vars 0 []

let add_local name offset =
  array_set local_vars 0 ((name, offset) :: array_get local_vars 0)
;;

let pp_local_var oc name =
  let rec loop vars =
    match vars with
    | [] -> printf "ERROR: %s not found !!!\n" name
    | (name2, offset) :: tail ->
      if name = name2 then fprintf oc "%d(fp)" offset else loop tail
  in
  loop (array_get local_vars 0)
;;

let rec codegen_expr oc expr =
  match expr with
  | CExpr_var name -> fprintf oc "  ld a0, %a\n" pp_local_var name
  | CExpr_int n -> fprintf oc "  li a0, %d\n" n
  | CExpr_tern (cond, t, e) ->
    let else_label = fresh_label ".Lelse" in
    let end_label = fresh_label ".Lend" in
    let () = codegen_expr oc cond in
    let () = fprintf oc "  beqz a0, %s\n" else_label in
    let () = codegen_expr oc t in
    let () = fprintf oc "  j %s\n" end_label in
    let () = fprintf oc "%s:\n" else_label in
    let () = codegen_expr oc e in
    let () = fprintf oc "%s:\n" end_label in
    ()
  | CExpr_binop (op, e1, e2) ->
    let () = codegen_expr oc e1 in
    let () = fprintf oc "  addi sp, sp, -8\n" in
    let () = fprintf oc "  sd a0, 0(sp)\n" in
    let () = codegen_expr oc e2 in
    let () = fprintf oc "  ld t0, 0(sp)\n" in
    let () = fprintf oc "  addi sp, sp, 8\n" in
    (match op with
     | Cpp_add -> fprintf oc "  add a0, t0, a0\n"
     | Cpp_sub -> fprintf oc "  sub a0, t0, a0\n"
     | Cpp_mul -> fprintf oc "  mul a0, t0, a0\n"
     | Cpp_div -> fprintf oc "  div a0, t0, a0\n"
     | Cpp_eq -> fprintf oc "  sub a0, t0, a0\n  seqz a0, a0\n"
     | Cpp_ne -> fprintf oc "  sub a0, t0, a0\n  snez a0, a0\n"
     | Cpp_lt -> fprintf oc "  slt a0, t0, a0\n"
     | Cpp_gt -> fprintf oc "  slt a0, a0, t0\n"
     | Cpp_le -> fprintf oc "  slt a0, a0, t0\n  xori a0, a0, 1\n"
     | Cpp_ge -> fprintf oc "  slt a0, t0, a0\n  xori a0, a0, 1\n"
     | Cpp_land -> fprintf oc "  and a0, t0, a0\n"
     | Cpp_lor -> fprintf oc "  or a0, t0, a0\n")
  | CExpr_call (fname, args) ->
    let argc = list_length args in
    let rec alloc_args args i =
      match args with
      | [] -> ()
      | x :: xs ->
        let () = codegen_expr oc x in
        let () = fprintf oc "  sd a0, %d(sp)\n" (8 * (argc - i - 1)) in
        alloc_args xs (i + 1)
    in
    let () = if argc > 0 then fprintf oc "  addi sp, sp, -%d\n" (8 * argc) else () in
    let () = alloc_args args 0 in
    let () = fprintf oc "  call %s\n" fname in
    let () = if argc > 0 then fprintf oc "  addi sp, sp, %d\n" (8 * argc) else () in
    ()
  | CExpr_assign (var, expr) ->
    let () = codegen_expr oc expr in
    fprintf oc "  sd a0, %a\n" pp_local_var var
;;

let rec codegen_statement oc epilogue stmt =
  match stmt with
  | CStmt_return None -> fprintf oc "  j %s\n" epilogue
  | CStmt_return (Some e) ->
    let () = codegen_expr oc e in
    let () = fprintf oc "  j %s\n" epilogue in
    ()
  | CStmt_expr e -> codegen_expr oc e
  | CStmt_block stmts -> list_iter (codegen_statement oc epilogue) stmts
  | _ -> printf "ERROR: not implemented codegen_statement\n"
;;

let pp_prologue oc fname =
  let () = fprintf oc ".global %s\n" fname in
  let () = fprintf oc ".text\n" in
  let () = fprintf oc "%s:\n" fname in
  let () = fprintf oc "  addi sp, sp, -16\n" in
  let () = fprintf oc "  sd fp, 8(sp)\n" in
  let () = fprintf oc "  sd ra, 0(sp)\n" in
  let () = fprintf oc "  mv fp, sp\n" in
  ()
;;

let pp_epilogue oc epilogue_label =
  let () = fprintf oc "%s:\n" epilogue_label in
  let () = fprintf oc "  ld ra, 0(sp)\n" in
  let () = fprintf oc "  ld fp, 8(sp)\n" in
  let () = fprintf oc "  addi sp, sp, 16\n" in
  let () = fprintf oc "  ret\n" in
  ()
;;

let codegen_function oc func =
  match func with
  | CFunction (fname, _, params, stms) ->
    let rec add_params params i =
      match params with
      | [] -> ()
      | (ty, name) :: xs ->
        let offset = 16 + (i * 8) in
        let () = add_local name offset in
        add_params xs (i + 1)
    in
    let () = clear_locals () in
    let () = add_params params 0 in
    let epilogue_label = fresh_label (sprintf "%s_epilogue" fname) in
    let () = pp_prologue oc fname in
    let () = list_iter (codegen_statement oc epilogue_label) stms in
    let () = pp_epilogue oc epilogue_label in
    ()
;;

let codegen_program oc prog = list_iter (codegen_function oc) prog

(* driver *)

let run_single_parser input =
  match parse_program (input, 0) with
  | Prez_error err -> printf "parsing failed: %a" pp_parsing_error err
  | Prez_success (prog, _) -> printf "parsed:\n%a" pp_cpp_program prog
;;

let run_single_program oc input =
  match parse_program (input, 0) with
  | Prez_error err -> printf "parsing failed: %a" pp_parsing_error err
  | Prez_success (ast, _) -> codegen_program oc ast
;;

(* TODO: rewrite *)
let rec input_all ic =
  let rec loop acc =
    match end_of_input ic with
    | true -> string_of_char_list (list_rev acc)
    | false ->
      let ch = input_char ic in
      loop (ch :: acc)
  in
  loop []
;;

let input_path = "program.cpp"
let output_path = "_build/program.s"

let main =
  let ic = open_in input_path in
  let oc = open_out output_path in
  let () = run_single_program oc (input_all ic) in
  let () = close_in ic in
  let () = close_out oc in
  0
;;
