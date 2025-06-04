open Str

let int_lit_re = Str.regexp "[0-9]+";;
let float_lit_re = Str.regexp "[0-9]+\\.[0-9]+";;
let string_lit_re = Str.regexp "\"\\([^\"]*\\)\"";;

let char_list_to_string (cl : char list) : string =
  Stdlib.String.of_seq (Stdlib.List.to_seq cl)

let string_to_char_list (s : string) : char list =
  Stdlib.List.of_seq (Stdlib.String.to_seq s)

