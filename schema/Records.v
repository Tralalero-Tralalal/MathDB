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

Record pager := {
  file_descriptor : OUnix.file_descr;
  file_length : int;
  pages : pages_type
}.

Record table := {
  num_rows : int; (*int8*)
  _pager : pager
}.

Record row := {
  id : ascii; (*int8*)
  name : string
}.

Record cursor := {
  _table : table;
  row_num : int;
  eot : bool;
}.
