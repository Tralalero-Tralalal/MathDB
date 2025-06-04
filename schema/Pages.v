From Stdlib Require Import String.
From Stdlib Require Import Ascii.
Require Import Parser.Cabs.

Parameter pages_type : Type.

Record table := {
  num_rows : Cabs.integer;
  pages : pages_type
}.

Record row := {
  id : Cabs.integer;
  name : string
}.

Extract Constant Pages.pages_type => "Bytes.t option array".

