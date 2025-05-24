From Stdlib Require Import String.
From Stdlib Require Import Ascii.
Require Import Proj.Cabs.

Parameter pages_type : Type.
Parameter file_descriptor_typ : Type.

Record pager := {
  file_descriptor : file_descriptor_typ;
  file_length : Cabs.integer;
  pages : pages_type
}.

Record table := {
  num_rows : Cabs.integer;
  _pager : pager
}.

Record row := {
  id : Cabs.integer;
  name : string
}.

Extract Constant pages_type => "Bytes.t option array".
Extract Constant file_descriptor_typ => "Unix.file_descr".
