From Stdlib Require Import String.
From Stdlib Require Import Ascii.
Require Import Stdlib.Lists.List.

Require Import Parser.Cabs.
Import ListNotations.
Open Scope string_scope.

Require Import ZArith.

Parameter string_uppercase : Cabs.str -> Cabs.str.
Parameter char_list_to_string : string -> Cabs.str.
Parameter string_to_char_list : Cabs.str -> string.

Inductive tokens :=
  | Keyword : string -> tokens
  | Identifier : string -> tokens
  | Star
  | Comma
  | Semi
  | Literal : literal -> tokens

with literal :=
  | Int_lit : ascii -> literal
  | String_lit : string -> literal.

Definition make_uppercase (s : string) : string :=
  (string_to_char_list (string_uppercase (char_list_to_string s))).

Definition is_keyword (s : string) : bool :=
  existsb (String.eqb (make_uppercase s)) ["INSERT"; "PRINT"].

Definition parse_lits (t : literal) : Cabs.expr  :=
  match t with
    | Int_lit i => Cabs.EXPR_LIT (Cabs.INTEGER_LIT i)
    | String_lit s => Cabs.EXPR_LIT (Cabs.STR_LIT s)
  end.

Definition parse_args (args : list tokens) : Cabs.insert_stmt :=
  match args with
  | [Literal x; Literal y; Literal z; Semi] => Cabs.WITH_INSERT (parse_lits x) (parse_lits y) (parse_lits z)
  | [Literal _; Literal _; Literal _] => Cabs.ERR_INSERT "forgor semi"
  | _ => Cabs.ERR_INSERT "Incorrect num of args, expect 3"
  end.

Definition parse (tokens : list tokens) : Cabs.sql_stmt :=
  match tokens with
  | Keyword "INSERT" :: rest => Cabs.INSERT_STMT (parse_args rest)
  | [Keyword "PRINT"; Semi] => Cabs.PRINT_STMT
  | _ => Cabs.ERR_STMT "must begin with a keyword!"
  end.

Extract Constant Lexer.string_uppercase => "Stdlib.String.uppercase_ascii".
Extract Constant Lexer.char_list_to_string => "Regex.char_list_to_string".
Extract Constant Lexer.string_to_char_list => "Regex.string_to_char_list".
