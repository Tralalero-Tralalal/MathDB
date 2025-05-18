From Stdlib Require Import String.
From Stdlib Require Import Ascii.

Parameter loc : Type.

Parameter str : Type.

Parameter char_code : Type.

Inductive prog :=
  | PROGRAM : sql_stmt -> prog

with sql_stmt :=
  | INSERT_STMT       : insert_stmt -> sql_stmt
  | PRINT_STMT

with expr :=
  | EXPR_LIT : str -> expr
      (*Leave it incomplete for now*)

with insert_stmt :=
  | WITH_INSERT : expr -> expr -> insert_stmt

with insert_body :=
  | INSERT_VALUES : list (list expr) ->(* omit clauses for now*) insert_body
  | INSERT_DEFAULT.

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant loc => "Lexing.position".
Extract Constant str => "string".
Extract Constant char_code => "int64".
