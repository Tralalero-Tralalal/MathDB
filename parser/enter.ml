open Lexer
open Cabs
open Schema
open Pages

let tbl : table =
  let pages = Array.make 100 None in
  let prealloc_page = Bytes.make page_size '\000' in
  pages.(1) <- Some prealloc_page;
  TABLE (0, pages)

let rec exec_ast (tbl : table) (ast : Cabs.sql_stmt) : table = 
  match ast with
  | Cabs.INSERT_STMT x -> exec_insert tbl x
  | Cabs.PRINT_STMT -> let _ = execute_select tbl in tbl
  | ERR_STMT x -> print_endline x; tbl

and exec_insert tbl (ast : Cabs.insert_stmt) : table =
  match ast with 
  | WITH_INSERT (x, y) -> exec_lit tbl x y
  | ERR_INSERT x -> print_endline x; tbl

and exec_lit tbl (f : Cabs.expr) (l : Cabs.expr) : table =
  match f, l with
  | EXPR_LIT a, EXPR_LIT b -> 
    let id = get_int a in
    let name = get_str b in
    let row = ROW (id, name) in
    let updated_tbl = execute_insert tbl row in
    Printf.printf "Insert(%d, %s).\n" id name;
    updated_tbl
  | _, _ -> print_endline "errors with literals"; tbl

and get_str (ast : Cabs.constant) =
  match ast with
  | STR_LIT s -> s
  | _ -> print_endline "can't use this lit, will instead add NULL string"; "NULL"
 
and get_int (ast : Cabs.constant) =
  match ast with
  | INTEGER_LIT s -> s
  | _ -> print_endline "can't use this lit, will instead put 0"; 0

let rec repl (tbl : table) =
  print_string ">>> ";
  let line = read_line () in
  match line with
  | "exit" | "quit" -> print_endline "Goodbye!"
  | input ->
    let tokens = tokenize input in
    let ast = parse tokens in
    let new_tbl = exec_ast tbl ast in
    repl new_tbl

let () = repl tbl
