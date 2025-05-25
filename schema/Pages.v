From Stdlib Require Import String.
From Stdlib Require Import Ascii.
Require Import Proj.Cabs.

Parameter pages_type : Type.

Definition byte := ascii.
Definition bytes := list ascii.

Record table := {
  num_rows : ascii; (*int8*)
  pages : pages_type
}.

Record row := {
  id : ascii; (*int8*)
  name : string
}.

Extract Constant Pages.pages_type => "Bytes.t option array".

