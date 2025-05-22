From Stdlib Require Import String.
From Stdlib Require Import Ascii.

Parameter loc : Type.
Parameter integer : Type.
Parameter floater : Type.
Parameter str : Type.

Inductive prog :=
  | PROGRAM : sql_stmt -> prog

with sql_stmt :=
  | INSERT_STMT       : insert_stmt -> sql_stmt
  | PRINT_STMT

with insert_stmt :=
  | WITH_INSERT : expr -> expr -> insert_stmt

with expr :=
  | EXPR_LIT : constant -> expr
      (*Leave it incomplete for now*)
with constant :=
  | STR_LIT : str -> constant
  | INTEGER_LIT : integer -> constant
  | FLOATER_LIT : floater -> constant

with insert_body :=
  | INSERT_VALUES : list (list expr) ->(* omit clauses for now*) insert_body
  | INSERT_DEFAULT.
