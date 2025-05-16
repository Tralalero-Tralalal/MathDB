From Stdlib Require Import String.
From Stdlib Require Import Ascii.

Parameter loc : Type.

Parameter str : Type.

Parameter char_code : Type.

Inductive prog :=
  | PROGRAM : list sql_stmt -> prog

with sql_stmt :=
  | ALTER_TABLE_STMT  : alter_table_stmt -> sql_stmt
  | DELETE_STMT       : delete_stmt -> sql_stmt
  | INSERT_STMT       : insert_stmt -> sql_stmt
  | SELECT_STMT       : select_stmt -> sql_stmt
  | UPDATE_STMT       : update_stmt -> sql_stmt

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
  | DELETE_STMT_LIMITED_ : delete_from_limited -> delete_stmt

with delete_from_limited :=
  | DELETE_FROM_LIMITED : str -> where_expr -> option returning_clause -> 
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

with factored_select_stmt :=
  | WITH_FACTORED : bool -> list common_table_expression -> factored_cor_compound -> factored_select_stmt
  | WITH_FACTORED_EMP : factored_cor_compound -> factored_select_stmt  

with factored_cor_compound :=
  | FAC : select_core_and_compound_op -> order_limit -> factored_cor_compound

with select_core_and_compound_op :=
  | SC_CO : list select_core -> list compound_operator -> select_core_and_compound_op

with order_limit :=
  | ORDER_BY : list ordering_term -> order_limit
  | ORDER_BY_LIMIT : expr -> expr_compound_typ -> order_limit
  | ORDER_LIMIT_NONE

with foreign_key_clause :=
  | FOREIGN_KEY_CLAUSE : foreign_table -> list str -> foreign_key_clause

with foreign_key_body :=
  | FOREIGN_KEY_ON : del_or_up -> on_ops -> foreign_key_body -> foreign_key_body
  | FOREIGN_KEY_MATCH : str -> foreign_key_body -> foreign_key_body 
  | FOREIGN_KEY_DEFERRABLE : bool -> deferrable_when -> foreign_key_body 

with deferrable_when :=
  | INITIALLY_DEFERRED
  | IMMEDIATE_DEFERRED
  | NOT_DEFERRFED

with del_or_up :=
  | DELETE_OP
  | UPDATE_OP

with on_ops :=
  | SET_NULL
  | SET_DEFAULT
  | CASCADE
  | RESTRICT
  | NO_ACTION

with frame_spec :=
  | FRAME_SPEC : frame_spec_ops -> frame_spec_body -> frame_spec

with frame_spec_ops :=
  | RANGE
  | ROWS
  | GROUPS

with frame_spec_body :=
  | BETWEEN : between_body -> and_body -> exclude_framespec -> frame_spec_body
  | UNBOUNDED : exclude_framespec -> frame_spec_body
  | EXPR_PRECEDING : expr -> exclude_framespec -> frame_spec_body
  | CURRENT_ROW : exclude_framespec -> frame_spec_body

with between_body := 
  | UNBOUNDED_BET
  | EXPR_PRECEDING_BET : expr -> between_body
  | CURRENT_ROW_BET 
  | EXPR_FOLLOWING_BET : expr -> between_body

with and_body :=
  | EXPR_PRECEDING_AND : expr -> between_body
  | CURRENT_ROW_AND  
  | EXPR_FOLLOWING_AND : expr -> between_body
  | UNBOUNDED_FOLLOWING

with exclude_framespec :=
  | EXCLUDE_NO_OTHERS
  | EXCLUDE_CURRENT_ROW
  | EXCLUDE_GROUP
  | EXCLUDE_TIES
  | EXCLUDE_NONE 

with function_arguments :=
  | EMP
  | FUNC_ARG : bool -> list expr -> order_by_arg -> function_arguments

with order_by_arg :=
  | ORDER_BY_ARG : list ordering_term -> order_by_arg
  | ORDER_NO_ARG

with indexed_column :=
  | EXPR_INDEXED : expr -> collate_op -> incline -> indexed_column
  | COLUMN_INDEXED : str -> collate_op -> incline -> indexed_column

with collate_op :=
  | COLLATE : str -> collate_op
  | NO_COLLATE

with insert_stmt :=
  | WITH_INSERT : bool -> list common_table_expression -> insert_statements -> insert_stmt
  | OTHER_INSERT : insert_statements -> insert_stmt

with insert_statements :=
  | REPLACE_INTO : schema_table_name -> option alias -> list str -> insert_body -> insert_statements
  | INSERT_INTO : schema_table_name -> option alias -> list str -> insert_body -> insert_statements
  | INSERT_OR : conflict_clause_typs -> schema_table_name -> option alias -> list str -> insert_body -> insert_statements

with insert_body :=
  | INSERT_VALUES : list (list expr) ->(* omit clauses for now*) insert_body
  | INSERT_SELECT : select_stmt -> insert_body
  | INSERT_DEFAULT 

Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.

Extract Constant loc => "Lexing.position".
Extract Constant str => "string".
Extract Constant char_code => "int64".
