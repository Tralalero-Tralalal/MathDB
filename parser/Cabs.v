From Stdlib Require Import String.
From Stdlib Require Import Ascii.

Parameter loc : Type.
Parameter uchars : Type.
Parameter integer : Type.
Parameter floater : Type.
Parameter char_code : Type.

Inductive prog :=
  | PROGRAM : sql_stmt -> prog

with sql_stmt :=
  | INSERT_STMT       : insert_stmt -> sql_stmt
  | PRINT_STMT

with expr :=
  | EXPR_LIT : constant -> expr
      (*Leave it incomplete for now*)
with constant :=
  | STR_LIT : uchars -> constant
  | INTEGER_LIT : integer -> constant
  | FLOATER_LIT : floater -> constant

with insert_stmt :=
  | WITH_INSERT : expr -> expr -> insert_stmt

with insert_body :=
  | INSERT_VALUES : list (list expr) ->(* omit clauses for now*) insert_body
  | INSERT_DEFAULT.

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant loc => "Lexing.position".
Extract Constant uchars => "Uchar.t array".
Extract Constant integer => "int".
Extract Constant floater => "float".
