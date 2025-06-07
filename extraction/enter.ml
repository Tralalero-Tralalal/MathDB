open Lexer
open Cabs
open Schema
open Regex

exception Full_error of string


let print_char_list clist =
  clist |> Stdlib.List.iter (fun c -> print_char c);
  print_newline ()


(*This prints all rows*)
let print_row_list (rows : row list)  =
  Stdlib.List.iter (fun row -> Printf.printf "(%d, %s, %s)\n" (Char.code row.id) (Regex.char_list_to_string row.name) (Regex.char_list_to_string row.email)) rows

let print_table (tbl : table) =
  Printf.printf "num_rows: %d\n" (Char.code tbl.num_rows);
  Stdlib.List.iteri
    (fun i opt_page ->
       Printf.printf "Page %d: " i;
       match opt_page with
       | Some chars ->
           print_char_list chars
       | None ->
           print_endline "<empty>")
    tbl.pages

let rec exec_ast (tbl : table) (ast : Cabs.sql_stmt) : table = 
  match ast with
  | Cabs.INSERT_STMT x -> exec_insert tbl x
  | Cabs.PRINT_STMT -> let rows = execute_select tbl in print_int (Stdlib.List.length rows); print_row_list rows; tbl
  | ERR_STMT x -> print_endline (char_list_to_string x); tbl

and exec_insert tbl (ast : Cabs.insert_stmt) : table =
  match ast with 
  | WITH_INSERT (x, y, z) -> exec_lit tbl x y z
  | ERR_INSERT x -> print_endline (char_list_to_string x); tbl

and exec_lit tbl (f : Cabs.expr) (l : Cabs.expr) (z : Cabs.expr): table =
  match f, l, z with
  | EXPR_LIT a, EXPR_LIT b, EXPR_LIT c -> 
    let id = get_int a in
    let name = get_str b in
    let email = get_str c in
    let row = {
      id = id;
      name = name;
      email = email;
    } in
    let updated_tbl = match execute_insert tbl row with
                      | Some x -> x
                      | None -> raise (Full_error "too full") in
    Printf.printf "Insert(%d, %s, %s).\n" (Char.code id) (Regex.char_list_to_string name) (Regex.char_list_to_string email);
    updated_tbl
  | _, _, _ -> print_endline "errors with literals"; tbl

and get_str (ast : Cabs.constant) =
  match ast with
  | STR_LIT s ->  s
  | _ -> print_endline "can't use this lit, will instead add NULL string"; (string_to_char_list "NULL")
 
and get_int (ast : Cabs.constant) =
  match ast with
  | INTEGER_LIT s ->  s
  | _ -> print_endline "can't use this lit, will instead put 0"; '\x00'

let is_keyword s = Stdlib.List.mem s ["INSERT"; "PRINT"]

let rec tokenize (s : string list) : tokens list =
  Stdlib.List.map (fun word ->
    match word with
    | "*" -> Star
    | "," -> Comma
    | ";" -> Semi
    | w when is_keyword (Stdlib.String.uppercase_ascii w) -> Keyword (string_to_char_list (Stdlib.String.uppercase_ascii w))
    | w when Str.string_match int_lit_re w 0 ->
        Literal (Int_lit (Char.chr (int_of_string w)))
    | w when Str.string_match string_lit_re w 0 ->
        let str_val = Str.matched_group 1 w in
        Literal (String_lit (string_to_char_list str_val))
    | w -> Identifier (string_to_char_list w)
  ) s

let string_of_token = function
  | Keyword kw        -> Printf.sprintf "Keyword(%s)" (char_list_to_string kw)
  | Identifier id     -> Printf.sprintf "Identifier(%s)" (char_list_to_string id)
  | Star              -> "Star(*)"
  | Comma             -> "Comma(,)"
  | Semi              -> "SEMI"
  | Literal (Int_lit i)      -> Printf.sprintf "IntLiteral(%d)" (Char.code i)
  | Literal (String_lit str) -> Printf.sprintf "StringLiteral(\"%s\")" (char_list_to_string str)

let print_tokens tokens =
  Stdlib.List.iter (fun tok ->
    Printf.printf "%s\n" (string_of_token tok)
  ) tokens

let rec repl (tbl : table) =
  print_string ">>> ";
  let line = read_line () in
  match line with
  | "exit" | "quit" -> print_endline "Goodbye!"
  | "table" -> print_table tbl; repl tbl
  | input ->
  let words = Stdlib.String.split_on_char ' ' input in
    let tokens = tokenize words in
    let ast = parse tokens in
    let new_tbl = exec_ast tbl ast in
    repl new_tbl

let () = repl new_tbl
