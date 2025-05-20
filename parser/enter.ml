open Lexer
open Cabs


let rec repl () =
  print_string ">>> ";
  let line = read_line () in
  match line with
  | "exit" | "quit" -> print_endline "Goodbye!"
  | input ->  let tokens = tokenize input in
      Printf.printf "You typed: %s\n" input;
    print_tokens tokens;
    parse tokens;
      repl ()

let () = repl ()
