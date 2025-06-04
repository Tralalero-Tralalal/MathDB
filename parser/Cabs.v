From Stdlib Require Import String.
From Stdlib Require Import Ascii.

Require Import ZArith.

Inductive prog :=
  | PROGRAM : sql_stmt -> prog

with sql_stmt :=
  | INSERT_STMT       : insert_stmt -> sql_stmt
  | PRINT_STMT
  | ERR_STMT : string -> sql_stmt

with insert_stmt :=
  | WITH_INSERT : expr -> expr -> expr -> insert_stmt
  | ERR_INSERT : string -> insert_stmt

with expr :=
  | EXPR_LIT : constant -> expr
  | ERR_LIT : string -> expr

with constant :=
  | STR_LIT : string -> constant
  | INTEGER_LIT : ascii -> constant
  | FLOATER_LIT : floater -> constant
  | ERR_CONST : string -> constant

with insert_body :=
  | INSERT_VALUES : list (list expr) ->(* omit clauses for now*) insert_body
  | INSERT_DEFAULT.

