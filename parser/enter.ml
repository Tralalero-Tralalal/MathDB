open Lexer
open Cabs
open Schema
open Pages

let tbl : table =
  let pages = Array.make 100 None in
  let prealloc_page = Bytes.make page_size '\000' in
  pages.(1) <- Some prealloc_page;
  TABLE (0, pages)

let make_row (first : Cabs.constant) (second : Cabs.constant) : row =
  let id =
    match first with
    | INTEGER_LIT i -> i
    | _ -> failwith "Expected integer literal for ID"
  in
  let name =
    match second with
    | STR_LIT s -> s
    | _ -> failwith "Expected string literal for name"
  in
  ROW (id, name)

let rec repl (tbl : table) =
  print_string ">>> ";
  let line = read_line () in
  match line with
  | "exit" | "quit" -> print_endline "Goodbye!"
  | input ->
    let tokens = tokenize input in
    let ast = parse tokens in
    let new_tbl =
      match ast with
      | Cabs.INSERT_STMT (WITH_INSERT (EXPR_LIT a, EXPR_LIT b)) ->
          let row = make_row a b in
          let updated_tbl = execute_insert tbl row in
          Printf.printf "Insert result: %s\n";
          updated_tbl
      | Cabs.PRINT_STMT ->
          let _ = execute_select tbl in
          tbl
    in
    repl new_tbl

let () = repl (tbl)
