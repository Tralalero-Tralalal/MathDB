  open Parser
  open Pre_parser
  open Cabs
  open Uchar

let string_of_loc (loc : Lexing.position) =
  Printf.sprintf "line: %d, offset: %d" loc.pos_lnum (loc.pos_cnum - loc.pos_bol)

let convert_token = function
  | Pre_parser.SELECT loc -> Parser.SELECT loc 
  | Pre_parser.EOF -> Parser.EOF ()

let string_of_token = function
  | Parser.SELECT loc -> Printf.sprintf "SELECT, loc: %s" (string_of_loc loc)
  | Parser.EOF loc -> "EOF"

open Format

let rec print_program fmt = function
  | Cabs.PROGRAM word -> Stdlib.List.iter (print_select fmt) word

and print_select fmt = function
  | Cabs.SELECT loc -> Format.fprintf fmt "select\n"
