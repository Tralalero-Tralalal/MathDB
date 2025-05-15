exception Lexing_error of string

open List
open Sedlexing
open Uchar

(* Function to convert a Uchar array to a string *)
let uchar_array_to_string (arr: Uchar.t array) : string =
  Array.fold_left (fun acc uchar ->
    acc ^ (Uchar.to_char uchar |> Stdlib.String.make 1)
  ) "" arr

(* Lexer utilities *)
let pos buf = lexing_position_start buf

(* Whitespace and comments handling *)
let rec token buf =
  match%sedlex buf with
    | "SELECT" -> Pre_parser.SELECT (pos buf)
    | eof -> EOF
    | white_space -> token buf
    | _ -> raise (Lexing_error "Unexpected character")
