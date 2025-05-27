Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.
Require Import Proj.Pages.
Require Import Proj.Cabs.
Require Import Proj.Lexer.

Set Extraction Output Directory "./extraction/".

Separate Extraction Cabs.prog Pages.row Pages.table Lexer.parse Pages.serialize_row Pages.deserialize_row Pages.execute_select Pages.execute_insert
Pages.new_tbl.
