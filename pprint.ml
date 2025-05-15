  open Parser
  open Pre_parser
  open Cabs
  open Uchar

let string_of_loc (loc : Lexing.position) =
  Printf.sprintf "line: %d, offset: %d" loc.pos_lnum (loc.pos_cnum - loc.pos_bol)

let uchar_of_int64 (i : int64) : Uchar.t =
  let n = Int64.to_int i in
  if not (Uchar.is_valid n) then
    failwith "int64 value out of valid Unicode scalar range"
  else
    Uchar.of_int n

let utf8_of_char_code_list (codes : int64 list) : string =
  let buf = Buffer.create 16 in
  Stdlib.List.iter
    (fun code ->
      let i = Int64.to_int code in
      if Uchar.is_valid i then
        Buffer.add_utf_8_uchar buf (Uchar.of_int i)
      else
        Buffer.add_utf_8_uchar buf (Uchar.of_int 0xFFFD)
    )
    codes;
  Buffer.contents buf

let convert_token = function
  | Pre_parser.SELECT loc -> Parser.SELECT loc 
  | Pre_parser.EOF -> Parser.EOF ()


let string_of_token = function
  | Parser.SELECT loc -> Printf.sprintf "SELECT, loc: %s" (string_of_loc loc)
  | Parser.EOF loc -> "EOF"

open Format

let rec print_program fmt = function
  | Cabs.TOKEN word -> print_select fmt word

and print_select fmt = function
  | Cabs.SELECT loc -> Format.fprintf fmt "select\n"
