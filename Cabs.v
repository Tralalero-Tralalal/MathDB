From Stdlib Require Import String.
From Stdlib Require Import Ascii.

Parameter loc : Type.

Parameter str : Type.

Inductive sql-stmt-list :=
  | SQL_STMT_LIST : list sql-stmt -> sql-stmt-list

with sql-stmt :=
  | SQL_STMT : bool (* Explain *) -> bool (*Query plan*) -> statements -> sql-stmt


with statements :=
  | ALTER_TABLE_STMT : alter_table_stmt -> statements
  | ANALYZE_STMT : analyze_stmt -> statements
  | ATTACH_STMT : attach_stmt -> statements
  | BEGIN_STMT : begin_stmt -> statements
  | COMMIT_STMT : commit_stmt -> statements
  | CREATE_INDEX_STMT : create_index_stmt -> statements
  | CREATE_TABLE_STMT : create_table_stmt -> statements
  | CREATE_TRIGGER_STMT : create_trigger_stmt -> statements
  | CREATE_VIEW_STMT : create_view_stmt -> statements
  | CREATE_VIRTUAL_TABLE_STMT : create_virtual_table_stmt -> statements
  | DELETE_STMT : delete_stmt -> statements
  | DELETE_STMT_LIMITED : delete_stmt_limited -> statements
  | DETACH_STMT : detach_stmt -> statements
  | DROP_INDEX_STMT : drop_index_stmt -> statements
  | DROP_TRIGGER_STMT : drop_trigger_stmt -> statements
  | DROP_VIEW_STATEMENT : drop_view_stmt -> statements
  | INSERT_STMT : insert_stmt -> statements
  | PRAGMA_STMT : pragma_stmt -> statements
  | REINDEX_STMT : reindex_stmt -> statements
  | RELEASE_STMT : release_stmt -> statements
  | ROLLBACK_STMT : rollback_stmt -> statements
  | SAVEPOINT_STMT : savepoint_stmt -> statements
  | SELECT_STMT : select_stmt -> statements
  | UPDATE_STMT : update_stmt -> statements
  | UPDATE_STMT_LIMITED : update_stmt_limited -> statements
  | VACUUM_STMT : vacuum_stmt -> statements

with alter_table_stmt :=
  | ALTER_TABLE_STMT_ : option schema_name -> table_name -> alter_table_statements -> alter_table_stmt

with alter_table_statements :=
  | RENAME_TO : new_table_name -> alter_table_statements
  | RENAME_COLUMN : column_name -> new_column_name -> alter_table_statements 
  | RENAME_TABLE : column_name -> new_column_name -> alter_table_statements
  | ADD_COLUMN : column_def -> alter_table_statements 
  | ADD_TABLE : column_def -> alter_table_statements
  | DROP_COLUMN : column_name -> alter_table_statements 
  | DROP_TABLE : column_name -> alter_table_statements

with analyze_stmt :=
  | ANALYZE : analyze_stmt

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant loc => "Lexing.position".
Extract Constant str => "string".

