From Stdlib Require Import String.
From Stdlib Require Import Ascii.
Require Import Proj.Cabs.

Parameter pages_type : Type.

Inductive table :=
  | TABLE : Cabs.integer -> pages_type -> table.

Inductive row :=
  | ROW : Cabs.integer -> string -> row.


Extract Constant Pages.pages_type => "Bytes.t option array".

