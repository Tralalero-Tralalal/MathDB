From Stdlib Require Import String.
From Stdlib Require Import Ascii.
Require Import Proj.Cabs.

Parameter pages_type : Type.

Record table := {
  num_rows : ascii;
  pages : pages_type
}.

Record row := {
  id : ascii;
  name : string
}.

Extract Constant Pages.pages_type => "Bytes.t option array".

