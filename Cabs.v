From Stdlib Require Import String.
From Stdlib Require Import Ascii.


Parameter loc : Type.

Parameter str : Type.

Inductive sql-stmt-list :=
  | SQL_STMT_LIST : list sql-stmt -> sql-stmt-list

with sql-stmt :=
  | SQL_STMT : bool (* Explain *) -> bool (*Query plan*) -> statements -> sql-stmt


with statements :=
  | ALTER_TABLE_STMT : -> statements
  | ANALYZE_STMT : -> statements
  | ATTACH_STMT : -> statements
  | BEGIN_STMT : -> statements
  | COMMIT_STMT : -> statements
  | CREATE_INDEX_STMT : -> statements
  | CREATE_TABLE_STMT : -> statements
  | CREATE_TRIGGER_STMT : -> statements
  | CREATE_VIEW_STMT : -> statements
  | CREATE_VIRTUAL_TABLE_STMT : -> statements
  | DELETE_STMT : -> statements
  | DELETE_STMT_LIMITED : -> statements
  | DETACH_STMT : -> statements
  | DROP_INDEX_STMT : -> statements
  | DROP_TRIGGER_STMT : -> statements
  | DROP_VIEW_STATEMENT : -> statements
  | INSERT_STMT : -> statements
  | PRAGMA_STMT : -> statements
  | REINDEX_STMT : -> statements
  | RELEASE_STMT : -> statements
  | ROLLBACK_STMT : -> statements
  | SAVEPOINT_STMT : -> statements
  | SELECT_STMT : -> statements
  | UPDATE_STMT : -> statements
  | UPDATE_STMT_LIMITED : -> statements
  | VACUUM_STMT : -> statements

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant loc => "Lexing.position".
Extract Constant str => "string".

