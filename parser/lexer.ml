type token =
  | Keyword of string
  | Identifier of string
  | Star
  | Comma
  | Literal of string
  | Eof

let is_keyword s = List.mem s ["INSERT"]

let rec tokenize (s : string) : token list =
  let words = String.split_on_char ' ' s in
  List.map (fun word ->
    match word with
    | "*" -> Star
    | "," -> Comma
    | w when is_keyword (String.uppercase_ascii w) -> Keyword (String.uppercase_ascii w)
    | w -> Identifier w
  ) words
