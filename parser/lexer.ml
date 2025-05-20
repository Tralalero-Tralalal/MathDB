open Str

let int_literal_re = Str.regexp "[0-9]+";;
let float_literal_re = Str.regexp "[0-9]+\\.[0-9]+";;
let string_literal_re = Str.regexp "\"\\([^\"]*\\)\"";;

type token =
  | Keyword of string
  | Identifier of string
  | Star
  | Comma
  | Int_lit of int
  | Float_lit of float
  | String_lit of string
  | Eof

let is_keyword s = List.mem s ["INSERT"]

let rec tokenize (s : string) : token list =
  let words = String.split_on_char ' ' s in
  List.map (fun word ->
    match word with
    | "*" -> Star
    | "," -> Comma
    | w when is_keyword (String.uppercase_ascii w) -> Keyword (String.uppercase_ascii w)
    | w when Str.string_match float_literal_re w 0 ->
        Float_lit (float_of_string w)
    | w when Str.string_match int_literal_re w 0 ->
        Int_lit (int_of_string w)
    | w when Str.string_match string_literal_re w 0 ->
        let str_val = Str.matched_group 1 w in
        String_lit str_val
    | w -> Identifier w
  ) words

let string_of_token = function
  | Keyword kw        -> Printf.sprintf "Keyword(%s)" kw
  | Identifier id     -> Printf.sprintf "Identifier(%s)" id
  | Star              -> "Star(*)"
  | Comma             -> "Comma(,)"
  | Int_lit i      -> Printf.sprintf "IntLiteral(%d)" i
  | Float_lit f    -> Printf.sprintf "FloatLiteral(%f)" f
  | String_lit str -> Printf.sprintf "StringLiteral(\"%s\")" str

let print_tokens tokens =
  List.iter (fun tok ->
    Printf.printf "%s\n" (string_of_token tok)
  ) tokens

