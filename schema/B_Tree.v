From Stdlib Require Import String.
From Stdlib Require Import Ascii.
From Stdlib Require Import List.
From Stdlib Require Import Arith.
From Stdlib Require Import NArith.
From Stdlib Require Import ZArith.
From Stdlib Require Import ExtrOcamlIntConv.
Import List.ListNotations.

From Schema Require Import Records.
From Schema Require Import Helpers.

Definition NODE_TYPE_SIZE := 1.
Definition NODE_TYPE_OFFSET := 0.
Definition IS_ROOT_SIZE := 1.
Definition IS_ROOT_OFFSET := 1.
Definition PARENT_POINTER_SIZE := 4.
Definition PARENT_POINTER_OFFSET := IS_ROOT_OFFSET + IS_ROOT_SIZE.
Definition COMMON_NODE_HEADER_SIZE :=
     NODE_TYPE_SIZE + IS_ROOT_SIZE + PARENT_POINTER_SIZE.

Definition LEAF_NODE_NUM_CELLS_SIZE := 4.
Definition LEAF_NODE_NUM_CELLS_OFFSET := COMMON_NODE_HEADER_SIZE.
Definition LEAF_NODE_HEADER_SIZE :=
    COMMON_NODE_HEADER_SIZE + LEAF_NODE_NUM_CELLS_SIZE.

Definition LEAF_NODE_KEY_SIZE := 4.
Definition LEAF_NODE_KEY_OFFSET := 0.
Definition LEAF_NODE_VALUE_SIZE := row_size. (*33 bytes*)
Definition LEAF_NODE_VALUE_OFFSET :=
    LEAF_NODE_KEY_OFFSET + LEAF_NODE_KEY_SIZE.
Definition LEAF_NODE_CELL_SIZE := LEAF_NODE_KEY_SIZE + LEAF_NODE_VALUE_SIZE.
Definition LEAF_NODE_SPACE_FOR_CELLS := page_size - LEAF_NODE_HEADER_SIZE.
Definition LEAF_NODE_MAX_CELLS :=
    LEAF_NODE_SPACE_FOR_CELLS / LEAF_NODE_CELL_SIZE.


Definition leaf_node_cell_offset (cell_num : nat) :=
   (LEAF_NODE_HEADER_SIZE + (cell_num * LEAF_NODE_CELL_SIZE)).

Definition sub_leaf_node_num_cells (p : page) (n : nat) :=
   list_blit p (make_list [(ascii_of_nat n)] LEAF_NODE_NUM_CELLS_SIZE) LEAF_NODE_NUM_CELLS_OFFSET.

(*Only take the first byte out of 4*)
Definition leaf_node_num_cells (p : page) :=
  nat_of_ascii (hd zero (list_sub p LEAF_NODE_NUM_CELLS_OFFSET LEAF_NODE_NUM_CELLS_SIZE)). (*Only take the first byte*)

Definition leaf_node_cell (p : page) (cell_num : nat) :=
  list_sub p (leaf_node_cell_offset cell_num) LEAF_NODE_CELL_SIZE.

Definition sub_leaf_node_key (p : page) (cell_num : nat) (new_key : nat) :=
   list_blit p (make_list [(ascii_of_nat new_key)] LEAF_NODE_KEY_SIZE)
   (leaf_node_cell_offset cell_num).

(*Only take the first byte out of 4*)
Definition leaf_node_key (p : page) (cell_num : nat) :=
  list_sub (leaf_node_cell p cell_num) LEAF_NODE_KEY_OFFSET LEAF_NODE_KEY_SIZE.

Definition sub_leaf_node_value (p : page) (cell_num : nat) (value : list ascii) :=
   list_blit p (make_list value LEAF_NODE_VALUE_SIZE)
   ((leaf_node_cell_offset cell_num) + LEAF_NODE_VALUE_OFFSET).

Definition leaf_node_value (p : page) (cell_num : nat) :=
  list_sub (leaf_node_cell p cell_num) LEAF_NODE_VALUE_OFFSET LEAF_NODE_VALUE_SIZE.

Definition initialize_leaf_node (p : page) :=
   list_blit p (make_list_of LEAF_NODE_NUM_CELLS_SIZE Ascii.zero) LEAF_NODE_NUM_CELLS_OFFSET.

Definition rando := ["A"%char; "A"%char; "H"%char; "M"%char; "A"%char; "D"%char].

Definition plain_page := make_list_of page_size zero.

Compute (leaf_node_num_cells 
(sub_leaf_node_num_cells plain_page 7)).
Compute (leaf_node_key (sub_leaf_node_key plain_page 7 7) 7).
Compute (leaf_node_value (sub_leaf_node_value plain_page 7 rando) 7).
