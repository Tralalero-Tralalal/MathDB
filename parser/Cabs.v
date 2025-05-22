From Stdlib Require Import String.
From Stdlib Require Import Ascii.

Require Import ZArith.

Parameter loc : Type.
Parameter integer : Type.
Parameter floater : Type.
Parameter str : Type.

Inductive prog :=
  | PROGRAM : sql_stmt -> prog

with sql_stmt :=
  | INSERT_STMT       : insert_stmt -> sql_stmt
  | PRINT_STMT
  | ERR_STMT : string -> sql_stmt

with insert_stmt :=
  | WITH_INSERT : expr -> expr -> insert_stmt
  | ERR_INSERT : string -> insert_stmt

with expr :=
  | EXPR_LIT : constant -> expr
  | ERR_LIT : string -> expr

with constant :=
  | STR_LIT : string -> constant
  | INTEGER_LIT : integer -> constant
  | FLOATER_LIT : floater -> constant
  | ERR_CONST : string -> constant

with insert_body :=
  | INSERT_VALUES : list (list expr) ->(* omit clauses for now*) insert_body
  | INSERT_DEFAULT.

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant Cabs.loc => "Lexing.position".
Extract Constant Cabs.integer => "int".
Extract Constant Cabs.floater => "float".
Extract Constant Cabs.str => "string".

