Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.
Require Import Proj.Schema.
Require Import Proj.Cabs.
Require Import Proj.Lexer.

Set Extraction Output Directory "./extraction/".

Separate Extraction Cabs.prog Schema.row Schema.table Lexer.parse Schema.serialize_row Schema.deserialize_row Schema.execute_select Schema.execute_insert
Schema.new_tbl.
