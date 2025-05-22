Require Import ExtrOcamlBasic.
Require Import ExtrOcamlString.
Require Import Proj.Pages.
Require Import Proj.Cabs.

Extract Constant Cabs.loc => "Lexing.position".
Extract Constant Cabs.integer => "int".
Extract Constant Cabs.floater => "float".
Extract Constant Cabs.str => "string".

Extract Constant Pages.pages_type => "Bytes.t option array".

Set Extraction Output Directory "./extraction/".

Separate Extraction Cabs.prog Pages.row Pages.table.
