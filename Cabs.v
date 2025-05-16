From Stdlib Require Import String.
From Stdlib Require Import Ascii.

Parameter loc : Type.

Parameter str : Type.

Parameter char_code : Type.

Inductive prog :=
  | PROGRAM : list sql_stmt -> prog

with sql_stmt :=
  | ALTER_TABLE_STMT  : alter_table_stmt -> sql_stmt
  | ANALYZE_STMT      : analyze_stmt -> sql_stmt
  | ATTACH_STMT       : attach_stmt -> sql_stmt
  | BEGIN_STMT        : begin_stmt -> sql_stmt
  | COMMIT_STMT       : commit_stmt -> sql_stmt
  | CREATE_INDEX_STMT : create_index_stmt -> sql_stmt
  | CREATE_TABLE_STMT : create_table_stmt -> sql_stmt
  | CREATE_TRIGGER_STMT : create_trigger_stmt -> sql_stmt
  | CREATE_VIEW_STMT  : create_view_stmt -> sql_stmt
  | CREATE_VIRTUAL_TABLE_STMT : create_virtual_table_stmt -> sql_stmt
  | DELETE_STMT       : delete_stmt -> sql_stmt
  | DELETE_STMT_LIMITED : delete_stmt_limited -> sql_stmt
  | DETACH_STMT       : detach_stmt -> sql_stmt
  | DROP_INDEX_STMT   : drop_index_stmt -> sql_stmt
  | DROP_TABLE_STMT   : drop_table_stmt -> sql_stmt
  | DROP_TRIGGER_STMT : drop_trigger_stmt -> sql_stmt
  | DROP_VIEW_STMT    : drop_view_stmt -> sql_stmt
  | INSERT_STMT       : insert_stmt -> sql_stmt
  | PRAGMA_STMT       : pragma_stmt -> sql_stmt
  | REINDEX_STMT      : reindex_stmt -> sql_stmt
  | RELEASE_STMT      : release_stmt -> sql_stmt
  | ROLLBACK_STMT     : rollback_stmt -> sql_stmt
  | SAVEPOINT_STMT    : savepoint_stmt -> sql_stmt
  | SELECT_STMT       : select_stmt -> sql_stmt
  | UPDATE_STMT       : update_stmt -> sql_stmt
  | UPDATE_STMT_LIMITED : update_stmt_limited -> sql_stmt
  | VACUUM_STMT       : vacuum_stmt -> sql_stmt

with schema_table_name :=
  | SCHEMA_NAME_NAME : option str -> str -> schema_table_name


with alter_table_stmt :=
  | ALTER_TABLE_STMT_ : schema_table_name -> alter_types -> alter_table_stmt

with alter_types :=
  | RENAME_TO : str -> alter_types
  | RENAME_COLUMN : str -> str -> alter_types
  | ADD_COLUMN : column_def -> alter_types
  | DROP_COLUMN : str -> alter_types

with analyze_stmt :=
  | ANALYZE_SCHEMA : str -> analyze_stmt
  | ANALYZE_INDEX_OR_TABLE : str -> analyze_stmt
  | ANALYZE_SCHEMA_INDEX_OR_TABLE : str -> str -> analyze_stmt

with attach_stmt :=
  | ATTACH_STMT_ : expr -> attach_stmt

with begin_stmt :=
  | BEGIN_TRANSFERED : begin_ops -> begin_stmt
  | BEGIN_STMT_ : begin_ops -> begin_stmt
      
with begin_ops :=
  | DEFERRED
  | IMMEDIATE
  | EXCLUSIVE

with column_constraint_types :=
  | PRIMARY_CONSTRAINT : incline -> conflict_clause -> bool -> column_constraint
  | NOT_CONSTRAINT : conflict_clause -> column_constraint
  | UNIQUE_CONSTRAINT : conflict_clause -> column_constraint
  | CHECK_CONSTRAINT : expr -> column_constraint
  | DEFAULT_CONSTRAINT : default_types -> column_constraint
  | COLLATE_CONSTRAINT : str -> column_constraint
  | FOREIGN_CONSTRAINT : foreign_key_clause -> column_constraint
  | GENERATED_CONSTRAINT : expr -> generated_as -> column_constraint
  | AS_CONSTRAINT : expr -> generated_as -> column_constraint

with column_constraint :=
  | CONSTRAINT_NAME_ : str -> column_constraint_types -> column_constraint
  | CONSTRAINT_ : column_constraint_types -> column_constraint

with default_types :=
  | DEFAULT_EXPR : expr -> default_types
  | DEFAULT_LIT : literal_value -> default_types 
  | DEFAULT_SIGNED : signed_number -> default_types

with generated_as :=
  | STORED
  | VIRTUAL
  | NEITHER

with incline :=
  | ASC
  | DESC
  | NEUTRAL

with column_def := 
  | COLUMN_DEF_TYPE : str -> str -> list column_constraint -> column_def
  | COLUMN_DEF_ : str -> list column_constraint -> column_def

with column_name_list :=
  | COLUMN_NAME_LIST : list str -> column_name_list

with commit_stmt :=
  | COMMIT_STMT_ : bool -> commit_stmt
  | END_STMT : bool -> commit_stmt


with common_table_expression :=
  | COMMON_TABLE_EXPRESSION : str -> list str -> bool -> bool -> select_stmt -> common_table_expression

with compound_operator :=
  | UNION
  | UNION_ALL
  | INTERSECT
  | EXCEPT 

with compound_core :=
  | COMPOUND_CORE : compound_operator -> select_core -> compound_core

with compound_select_stmt :=
  | WITH_COMPOUND :  bool -> list common_table_expression -> select_core -> compound_core -> compound_select_last -> compound_select_stmt
  | COMPOUND_SELECT : select_core -> compound_core -> compound_select_last -> compound_select_stmt

with compound_select_last :=
  | ORDER_LIMIT_SELECT : list ordering_term -> expr -> expr_compound_typ -> compound_select_last
  | LIMIT_SELECT : expr -> expr_compound_typ -> compound_select_last
  | ORDER_SELECT : list ordering_term -> compound_select_last
  | COMPOUND_NONE 

with expr_compound_typ :=
  | OFFSET_EXPR : expr -> expr_compound_typ
  | COMMA_EXPR : expr -> expr_compound_typ
  | NO_EXPR

with conflict_clause :=
  | NO_CONFLICT_CLAUSE
  | ON_CONFLICT_CLAUSE : conflict_clause_typs -> conflict_clause

with conflict_clause_typs := 
  | ROLLBACK 
  | ABORT 
  | FAIL 
  | IGNORE 
  | REPLACE

with create_index_stmt :=
  | CREATE_INDEX_STMT_ : bool -> schema_table_name -> str -> list indexed_column -> where_expr -> create_index_stmt
  | CREATE_INDEX_STMT_IF_NOT : bool -> schema_table_name -> str -> list indexed_column -> where_expr -> create_index_stmt

with where_expr :=
  | WHERE_EXPR : expr -> where_expr
  | FOUND_EXPR 

with create_table_stmt :=
  | CREATE_TABLE_STMT_ : bool -> schema_table_name -> create_table_typ -> create_table_stmt  
  | CREATE_TABLE_STMT_IF_NOT : bool -> schema_table_name -> create_table_typ -> create_table_stmt  

with create_table_typ :=
  | CREATE_BODY : list column_def -> list table_constraint -> table_options -> create_table_typ
  | CREATE_AS : select_stmt -> create_table_typ

with create_trigger_stmt :=
  | CREATE_TRIGGER_STMT_ : bool -> schema_table_name -> trigger_type -> trigger_action -> create_trigger_stmt
  | CREATE_TRIGGER_STMT_IF_NOT : bool -> schema_table_name -> create_trigger_stmt

with trigger_type :=
  | BEFORE
  | AFTER
  | INSTEAD

with trigger_action :=
  | DELETE : str -> trigger_action
  | INSERT : str -> trigger_action
  | UPDATE : str -> trigger_action
  | UPDATE_OF : list str -> str -> trigger_action

with trigger_ops :=
  | WHEN : expr -> list selected_stmts -> trigger_ops
  | FOR_EACH_ROW : expr -> list selected_stmts -> trigger_ops

with selected_stmts :=
  | UPD_STMT : update_stmt -> select_stmt
  | INS_STMT : insert_stmt -> select_stmt
  | DEL_STMT : delete_stmt -> select_stmt
  | SEL_STMT : select_stmt -> select_stmt

with create_view_stmt :=
  | CREATE_VIEW_STMT_ : bool -> schema_table_name -> list str -> select_stmt -> create_view_stmt
  | CREATE_VIEW_STMT_IF_NOT : bool -> schema_table_name -> list str -> select_stmt -> create_view_stmt

with create_virtual_table_stmt :=
  | CREATE_VIRTUAL_TABLE_STMT_ : bool -> schema_table_name -> str -> list module_argument -> create_virtual_table_stmt
  | CREATE_VIRTUAL_TABLE_STMT_IF_NOT : bool -> schema_table_name -> str -> list module_argument -> create_virtual_table_stmt

with cte_table_name :=
  | CTE_TABLE_NAME : list str -> cte_table_name

with delete_stmt :=
  | WITH_DELETE : bool -> list common_table_expression -> del_stmt -> delete_from -> delete_stmt
  | DELETE_STMT_ : delete_from -> delete_stmt

with delete_from :=
  | DELETE_FROM : str -> where_expr -> option returning_clause -> delete_from

with delete_stmt_limited :=
  | WITH_DELETE_LIMITED : bool -> list common_table_expression -> del_stmt -> delete_from_limited -> delete_stmt
  | DELETE_STMT_LIMITED : delete_from_limited -> delete_stmt

with delete_from_limited :=
  | DELETE_FROM : str -> where_expr -> option returning_clause -> 
      list ordering_term -> delete_from_limited -> expr -> expr_compound_typ -> delete_from_limited

with detach_stmt :=
  | DETACH_DATABASE : str -> detach_stmt
  | DETACH_STMT_ : str -> detach_stmt

with drop_index_stmt :=
  | DROP_INDEX_STMT_ : schema_table_name -> drop_index_stmt
  | DROP_INDEX_STMT_IF : schema_table_name -> drop_index_stmt

with drop_table_stmt :=
  | DROP_TABLE_STMT_ : schema_table_name -> drop_table_stmt
  | DROP_TABLE_STMT_IF : schema_table_name -> drop_table_stmt

with drop_trigger_stmt :=
  | DROP_TRIGGER_STMT_ : schema_table_name -> drop_trigger_stmt
  | DROP_TRIGGER_STMT_IF : schema_table_name -> drop_trigger_stmt

with drop_view_stmt :=
  | DROP_VIEW_STMT_ : schema_table_name -> drop_view_stmt
  | DROP_VIEW_STMT_IF : schema_table_name -> drop_view_stmt

with expr :=
  | EXPR_LIT : str -> expr
  | EXPR_BIND_PARAM : bind_parameter -> expr
  | EXPR_UNARY : unary -> expr -> expr
  | EXPR_BINARY : expr -> binary -> expr -> expr
  | EXPR_FUNC : str -> function_arguments -> option filter_clause -> option over_clause -> expr
  | EXPR_LS : list expr -> expr
      (*Leave it incomplete for now*)

with factored_select_stmt: =
  | WITH_FACTORED : bool -> list common_table_expression -> select_core -> factored_select_stmt
  | 


Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant loc => "Lexing.position".
Extract Constant str => "string".
Extract Constant char_code => "int64".
