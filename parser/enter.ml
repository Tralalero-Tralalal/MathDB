open Lexer
open Cabs
open Schema
open Pages
open Regex
open Unix

let integer_to_int (z : Z.t) : int =
  try Z.to_int z
  with _ -> failwith "Overflow: Z value too large to fit in an OCaml int"

let pager_flush (pager : pager) (page_num : int) (size : int) : pager =
  match pager.pages.(page_num) with
  | None ->
      prerr_endline "Tried to flush null page";
      exit 1
  | Some page ->
      let offset = page_num * page_size in
      let _ = Unix.lseek pager.file_descriptor offset Unix.SEEK_SET in
      let written = Unix.write pager.file_descriptor page 0 size in
      if written = -1 then (
        prerr_endline "Error writing page";
        exit 1
      ) else pager

let pager_open (filename : string) : pager =
  let fd = Unix.openfile filename [Unix.O_RDWR; Unix.O_CREAT] 0o600 in
  let file_length = Unix.lseek fd 0 Unix.SEEK_END in
  let pages = Array.make table_max_pages None in
{ file_descriptor = fd; file_length; pages }

let open_db (filename : string) = 
  let pager = pager_open filename in 
    let num_rows = pager.file_length / Schema.row_size in
          let return_tbl = {
            num_rows = num_rows;
            _pager = pager;
          } in return_tbl

let db_close (table: Pages.table) = 
  let pager = table._pager in
    let num_full_pages = table.num_rows / Schema.rows_per_page in 
      for i = 0 to num_full_pages - 1 do
        match pager.pages.(i) with
        | Some page -> pager_flush pager i page_size;
        pager.pages.(i) <- None
        | None -> ()
      done;
    let num_additional_rows = table.num_rows mod Schema.rows_per_page in
      if (num_additional_rows > 0) then begin
        let page_num = num_full_pages in
        match pager.pages.(page_num) with
        | Some page -> pager_flush pager page_num (num_additional_rows * Schema.row_size); pager.pages.(page_num) <- None
        | None -> ()
      end;
  (try Unix.close pager.file_descriptor
    with Unix.Unix_error (_, _, _) ->
     prerr_endline "Error closing db file";
     exit 1);
  Array.iteri (fun i page_opt ->
    match page_opt with
    | Some _ -> pager.pages.(i) <- None
    | None -> ()
  ) pager.pages

let rec exec_ast (tbl : table) (ast : Cabs.sql_stmt) : table = 
  match ast with
  | Cabs.INSERT_STMT x -> exec_insert tbl x
  | Cabs.PRINT_STMT -> let _ = execute_select tbl in tbl
  | ERR_STMT x -> print_endline (char_list_to_string x); tbl

and exec_insert tbl (ast : Cabs.insert_stmt) : table =
  match ast with 
  | WITH_INSERT (x, y) -> exec_lit tbl x y
  | ERR_INSERT x -> print_endline (char_list_to_string x); tbl

and exec_lit tbl (f : Cabs.expr) (l : Cabs.expr) : table =
  match f, l with
  | EXPR_LIT a, EXPR_LIT b -> 
    let id = get_int a in
    let name = get_str b in
    let row = {
      id = id;
      name = name;
    } in
    let updated_tbl = execute_insert tbl row in
    Printf.printf "Insert(%d, %s).\n" id (Regex.char_list_to_string name);
    updated_tbl
  | _, _ -> print_endline "errors with literals"; tbl

and get_str (ast : Cabs.constant) =
  match ast with
  | STR_LIT s ->  s
  | _ -> print_endline "can't use this lit, will instead add NULL string"; (string_to_char_list "NULL")
 
and get_int (ast : Cabs.constant) =
  match ast with
  | INTEGER_LIT s ->  s
  | _ -> print_endline "can't use this lit, will instead put 0"; 0

let is_keyword s = List.mem s ["INSERT"; "PRINT"]

let rec tokenize (s : string list) : tokens list =
  Stdlib.List.map (fun word ->
    match word with
    | "*" -> Star
    | "," -> Comma
    | ";" -> Semi
    | w when is_keyword (Stdlib.String.uppercase_ascii w) -> Keyword (string_to_char_list (Stdlib.String.uppercase_ascii w))
    | w when Str.string_match float_lit_re w 0 ->
        Literal (Float_lit (float_of_string w))
    | w when Str.string_match int_lit_re w 0 ->
        Literal (Int_lit (int_of_string w))
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
  | Literal (Int_lit i)      -> Printf.sprintf "IntLiteral(%d)" i
  | Literal (Float_lit f)    -> Printf.sprintf "FloatLiteral(%f)" f
  | Literal (String_lit str) -> Printf.sprintf "StringLiteral(\"%s\")" (char_list_to_string str)

let print_tokens tokens =
  Stdlib.List.iter (fun tok ->
    Printf.printf "%s\n" (string_of_token tok)
  ) tokens

let rec repl (tbl : table) =
  print_string ">>> ";
  let line = read_line () in
  match line with
  | "exit" | "quit" -> db_close tbl; print_endline "Goodbye!"
  | input ->
  let words = Stdlib.String.split_on_char ' ' input in
    let tokens = tokenize words in
    print_tokens tokens;
    let ast = parse tokens in
    let new_tbl = exec_ast tbl ast in
    repl new_tbl

let () = repl (open_db "mydb.db");

