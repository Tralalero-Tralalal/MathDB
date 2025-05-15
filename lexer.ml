exception Lexing_error of string

open List
open Sedlexing
open Uchar

let identifier_or_keyword =
  [%sedlex.regexp? xid_start, Star xid_continue]

(* Function to convert a Uchar array to a string *)
let uchar_array_to_string (arr: Uchar.t array) : string =
  Array.fold_left (fun acc uchar ->
    acc ^ (Uchar.to_char uchar |> Stdlib.String.make 1)
  ) "" arr

(* Lexer utilities *)
let pos buf = lexing_position_start buf

let keyword_of_string s loc =
  match Stdlib.String.uppercase_ascii s with
  | "SELECT" -> Pre_parser.SELECT loc
  | _           -> raise (Lexing_error "Unexpected character")

let rec token buf =
match%sedlex buf with
| identifier_or_keyword  ->
let uArr = Sedlexing.lexeme buf in
let id = uchar_array_to_string uArr in
keyword_of_string id (lexing_position_start buf)

| white_space -> token buf
| eof         -> Pre_parser.EOF
| _           -> raise (Lexing_error "Unexpected character")
