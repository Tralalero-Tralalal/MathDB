From Stdlib Require Import String.
From Stdlib Require Import Ascii.
From Stdlib Require Import List.
From Stdlib Require Import Arith.
From Stdlib Require Import NArith.
From Stdlib Require Import ZArith.
From Schema Require Import Helpers.
From SimpleIO Require Import SimpleIO.
From SimpleIO Require Import IO_Unix.
From Stdlib Require Import ExtrOcamlIntConv.

Definition pages_type := list (option (list ascii)).

Definition byte := ascii.
Definition _bytes := list ascii.
Definition page := _bytes.
Definition id_size := 1.
Definition name_size := 32. (* String can be 32 ascii asciis long *)
Definition id_offset := 0.
Definition name_offset := id_offset + id_size.
Definition row_size := id_size + name_size.
Definition page_size := 4096. (* 4 kilobytes*)
Definition table_max_pages := 100.

Record pager := {
  file_descriptor : OUnix.file_descr;
  file_length : int;
  pages : pages_type;
  num_pages : int;
}.

Record table := {
  root_page_num : int; (*int8*)
  _pager : pager
}.

Record row := {
  id : ascii; (*int8*)
  name : string
}.

Record cursor := {
  _table : table;
  page_num : int;
  cell_num : int;
  eot : bool;
}.
