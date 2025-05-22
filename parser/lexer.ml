exception Parse_error of string
exception Lexing_error of string

open Str
open Cabs

let int_literal_re = Str.regexp "[0-9]+";;
let float_literal_re = Str.regexp "[0-9]+\\.[0-9]+";;
let string_literal_re = Str.regexp "\"\\([^\"]*\\)\"";;


;;

type token =
  | Keyword of string
  | Identifier of string
  | Star
  | Comma
  | Semi
  | Literal of literal

and literal =
  | Int_lit of int
  | Float_lit of float
  | String_lit of string

let is_keyword s = List.mem s ["INSERT"; "PRINT"]

let rec tokenize (s : string) : token list =
  let words = Stdlib.String.split_on_char ' ' s in
  List.map (fun word ->
    match word with
    | "*" -> Star
    | "," -> Comma
    | ";" -> Semi
    | w when is_keyword (Stdlib.String.uppercase_ascii w) -> Keyword (Stdlib.String.uppercase_ascii w)
    | w when Str.string_match float_literal_re w 0 ->
        Literal (Float_lit (float_of_string w))
    | w when Str.string_match int_literal_re w 0 ->
        Literal (Int_lit (int_of_string w))
    | w when Str.string_match string_literal_re w 0 ->
        let str_val = Str.matched_group 1 w in
        Literal (String_lit str_val)
    | w -> Identifier w
  ) words

let string_of_token = function
  | Keyword kw        -> Printf.sprintf "Keyword(%s)" kw
  | Identifier id     -> Printf.sprintf "Identifier(%s)" id
  | Star              -> "Star(*)"
  | Comma             -> "Comma(,)"
  | Semi              -> "SEMI"
  | Literal (Int_lit i)      -> Printf.sprintf "IntLiteral(%d)" i
  | Literal (Float_lit f)    -> Printf.sprintf "FloatLiteral(%f)" f
  | Literal (String_lit str) -> Printf.sprintf "StringLiteral(\"%s\")" str

let print_tokens tokens =
  List.iter (fun tok ->
    Printf.printf "%s\n" (string_of_token tok)
  ) tokens

let rec parse = function
    | Keyword("INSERT") :: rest -> Cabs.INSERT_STMT (parse_args rest)
    | [Keyword("PRINT"); Semi ]-> Cabs.PRINT_STMT
    | _ -> ERR_STMT "must begin with a keyword!"

and parse_args = function
    | [Literal(x); Literal(y); Semi] -> Cabs.WITH_INSERT (parse_lits x, parse_lits y)
    | [Literal(x); Literal(y)] -> raise (Parse_error "forgor semi")
    | _ -> ERR_INSERT "Incorrect num of args, expect 2"

and parse_lits = function
    | Int_lit(i) -> Cabs.EXPR_LIT (Cabs.INTEGER_LIT i)
    | Float_lit(f) -> Cabs.EXPR_LIT (Cabs.FLOATER_LIT f)
    | String_lit(s) -> Cabs.EXPR_LIT (Cabs.STR_LIT s)
    | _ -> ERR_LIT "What the helly"
  
